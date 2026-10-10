read_item_validation <- function() {
  responses <- arrow::read_parquet(source_path("item_responses"))
  people <- arrow::read_parquet(source_path("item_participants"))
  catalog <- arrow::read_parquet(source_path("items"))
  keys <- c("poll_id", "source_dataset", "respondent_id")
  stopifnot(
    !anyDuplicated(people[keys]),
    !anyDuplicated(responses[c(keys, "wave", "item_id")]),
    !anyDuplicated(catalog[c("poll_id", "item_id")])
  )
  people <- people |>
    dplyr::filter(source_dataset == "historical", participant %in% TRUE)
  responses <- responses |>
    dplyr::filter(source_dataset == "historical", wave %in% c("t1", "t2")) |>
    dplyr::inner_join(dplyr::select(people, dplyr::all_of(keys), small_group_id),
      by = keys, relationship = "many-to-one"
    )
  stopifnot(nrow(dplyr::anti_join(responses, catalog, by = c("poll_id", "item_id"))) == 0)
  responses
}

item_panel <- function(responses) {
  stopifnot(
    dplyr::n_distinct(responses$poll_id) == 1,
    !anyDuplicated(responses[c("respondent_id", "wave", "item_id")])
  )
  common <- responses |>
    dplyr::distinct(wave, item_id) |>
    dplyr::count(item_id) |>
    dplyr::filter(n == 2) |>
    dplyr::pull(item_id) |>
    sort()
  people <- responses |>
    dplyr::distinct(respondent_id, small_group_id) |>
    dplyr::arrange(respondent_id)
  stopifnot(!anyDuplicated(people$respondent_id))
  matrices <- lapply(c("t1", "t2"), function(wave) {
    data <- responses[responses$wave == wave & responses$item_id %in% common, ]
    out <- matrix(NA_real_, nrow(people), length(common), dimnames = list(people$respondent_id, common))
    out[cbind(match(data$respondent_id, people$respondent_id), match(data$item_id, common))] <- data$correct
    out
  })
  keep <- stats::complete.cases(matrices[[1]], matrices[[2]])
  list(
    pre = matrices[[1]][keep, , drop = FALSE], post = matrices[[2]][keep, , drop = FALSE],
    people = people[keep, ], total_people = nrow(people), excluded = sum(!keep), items = common
  )
}

validation_folds <- function(people, seed) {
  group <- ifelse(is.na(people$small_group_id), paste0("person:", people$respondent_id),
    paste0("group:", people$small_group_id)
  )
  clusters <- sort(unique(group))
  if (length(clusters) < 5) stop("Fewer than five independent fold units")
  labels <- withr::with_seed(seed, sample(rep(seq_len(5), length.out = length(clusters))))
  labels[match(group, clusters)]
}

validation_partition <- function(pre_train, mode, seed) {
  stopifnot(ncol(pre_train) >= 4, all(is.finite(pre_train)), mode %in% c("random", "easy"))
  j <- ncol(pre_train)
  order <- if (mode == "easy") {
    order(-colMeans(pre_train), colnames(pre_train))
  } else {
    withr::with_seed(seed, sample.int(j))
  }
  list(predictor = order[seq_len(floor(j / 2))], validation = order[-seq_len(floor(j / 2))])
}

validation_predictions <- function(pre_train, post_train, pre_test, post_test, partition) {
  p <- partition$predictor
  v <- partition$validation
  stopifnot(length(intersect(p, v)) == 0, length(p) >= 2, length(v) >= 2)
  train <- data.frame(
    x1 = rowMeans(pre_train[, p, drop = FALSE]),
    x2 = rowMeans(post_train[, p, drop = FALSE])
  )
  x1 <- rowMeans(pre_test[, p, drop = FALSE])
  x2 <- rowMeans(post_test[, p, drop = FALSE])
  residual <- if (stats::sd(train$x1) == 0) {
    x2 - mean(train$x2)
  } else {
    x2 - stats::predict(stats::lm(x2 ~ x1, train), newdata = data.frame(x1 = x1))
  }
  list(
    scores = list(baseline = x1, post_score = x2, observed_gain = x2 - x1, residualized = residual),
    validation_gain = rowMeans(post_test[, v, drop = FALSE] - pre_test[, v, drop = FALSE])
  )
}

validate_item_subsets <- function(responses, replications = 100, seed = 20263010) {
  results <- partitions <- samples <- list()
  polls <- sort(unique(responses$poll_id))
  for (poll in polls) {
    panel <- item_panel(responses[responses$poll_id == poll, ])
    eligible <- ncol(panel$pre) >= 4 && nrow(panel$pre) >= 20
    reason <- if (eligible) "eligible" else "insufficient paired items or complete participants"
    first_folds <- tryCatch(validation_folds(panel$people, seed), error = identity)
    if (inherits(first_folds, "error")) {
      eligible <- FALSE
      reason <- conditionMessage(first_folds)
    }
    samples[[poll]] <- tibble::tibble(
      poll_id = poll, total_people = panel$total_people,
      complete_people = nrow(panel$pre), excluded = panel$excluded,
      items = ncol(panel$pre), status = reason
    )
    if (!eligible) next
    for (replication in seq_len(replications)) {
      split_seed <- seed + 1000L * match(poll, polls) + replication
      folds <- validation_folds(panel$people, split_seed)
      for (fold in seq_len(5)) {
        test <- folds == fold
        train <- !test
        for (mode in c("random", "easy")) {
          partition <- validation_partition(panel$pre[train, , drop = FALSE], mode, split_seed + fold)
          key <- paste(poll, replication, fold, mode, sep = "/")
          partitions[[key]] <- tibble::tibble(
            poll_id = poll, replicate = replication, fold = fold, mode = mode,
            seed = split_seed, predictor_items = paste(panel$items[partition$predictor], collapse = "|"),
            validation_items = paste(panel$items[partition$validation], collapse = "|"),
            training_n = sum(train), test_n = sum(test)
          )
          predicted <- validation_predictions(
            panel$pre[train, , drop = FALSE], panel$post[train, , drop = FALSE],
            panel$pre[test, , drop = FALSE], panel$post[test, , drop = FALSE], partition
          )
          results[[key]] <- purrr::imap_dfr(predicted$scores, function(score, estimator) {
            truth <- predicted$validation_gain
            correlation <- if (length(truth) < 4) NA_real_ else cor_if_variable(score, truth)
            rank_correlation <- if (length(truth) < 4) NA_real_ else cor_if_variable(rank(score), rank(truth))
            tibble::tibble(
              poll_id = poll, replicate = replication, fold = fold, mode = mode, estimator = estimator,
              n = length(truth), correlation = correlation, rank_correlation = rank_correlation,
              status = if (length(truth) < 4) {
                "too few validation participants"
              } else if (is.na(correlation)) {
                "constant score or validation gain"
              } else {
                "ok"
              }
            )
          })
        }
      }
    }
  }
  list(
    results = dplyr::bind_rows(results), partitions = dplyr::bind_rows(partitions),
    samples = dplyr::bind_rows(samples)
  )
}

summarise_item_validation <- function(results) {
  paired <- results |>
    dplyr::select(poll_id, replicate, fold, mode, estimator, correlation) |>
    tidyr::pivot_wider(names_from = estimator, values_from = correlation) |>
    dplyr::mutate(post_minus_gain = post_score - observed_gain, post_minus_baseline = post_score - baseline)
  per_split <- paired |>
    dplyr::summarise(
      dplyr::across(c(
        baseline, post_score, observed_gain, residualized,
        post_minus_gain, post_minus_baseline
      ), finite_median),
      .by = c(poll_id, replicate, mode)
    )
  per_split |>
    tidyr::pivot_longer(-c(poll_id, replicate, mode), names_to = "metric", values_to = "value") |>
    dplyr::summarise(
      median = finite_median(value), lower = finite_quantile(value, 0.05), upper = finite_quantile(value, 0.95),
      valid_splits = sum(is.finite(value)), splits = dplyr::n(), .by = c(poll_id, mode, metric)
    )
}

finite_quantile <- function(x, probability) {
  if (any(is.finite(x))) unname(stats::quantile(x[is.finite(x)], probability)) else NA_real_
}

finite_median <- function(x) finite_quantile(x, 0.5)

# Does education above the poll's participant median predict the learning proxy?
# observed gain alone, or post-process knowledge controlling for initial
# knowledge (equivalently, gain controlling for initial knowledge).
who_learns <- function(polardata) {
  data <- polardata |>
    dplyr::filter(!is.na(education_above_median), !is.na(t1know), !is.na(t2know), !is.na(pollgroup)) |>
    dplyr::mutate(high_education = as.numeric(education_above_median), gain = t2know - t1know)
  specs <- list(
    "Observed gain" = gain ~ high_education,
    "Post-process knowledge" = t2know ~ high_education,
    "Post-process knowledge given initial" = t2know ~ high_education + t1know
  )
  by_poll <- purrr::imap(specs, \(formula, label) {
    data |>
      dplyr::group_split(pollname) |>
      purrr::map(\(poll) {
        fit <- stats::lm(formula, data = poll)
        vc <- sandwich::vcovCL(fit, cluster = poll$pollgroup, type = "HC1")
        tibble::tibble(
          pollname = poll$pollname[[1]], proxy = label,
          n = nrow(poll), groups = dplyr::n_distinct(poll$pollgroup),
          n_above = sum(poll$high_education), n_at_or_below = sum(1 - poll$high_education),
          estimate = stats::coef(fit)[["high_education"]],
          std_error = sqrt(vc["high_education", "high_education"])
        )
      }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
  pooled <- by_poll |>
    dplyr::group_split(proxy) |>
    purrr::map(\(x) {
      fit <- bayesmeta::bayesmeta(y = x$estimate, sigma = x$std_error, labels = x$pollname, tau.prior = tau_prior)
      tibble::tibble(
        proxy = x$proxy[[1]], pollname = "Pooled estimate",
        estimate = fit$summary["median", "mu"],
        lower = fit$summary["95% lower", "mu"], upper = fit$summary["95% upper", "mu"]
      )
    }) |>
    purrr::list_rbind()
  dplyr::bind_rows(
    dplyr::mutate(by_poll, lower = estimate - 1.96 * std_error, upper = estimate + 1.96 * std_error),
    pooled
  )
}

tau_prior <- \(t) bayesmeta::dhalfnormal(t, scale = 0.1)

# Share of attitude indices on which each proxy predicts moving with the net
# change, and how often it does so at conventional significance.
summarise_attitudes <- function(regressions) {
  regressions |>
    dplyr::summarise(
      indices = dplyr::n(),
      mean_estimate = mean(estimate),
      share_positive = mean(estimate > 0),
      share_significant_positive = mean(estimate / std_error > 1.96),
      share_significant_negative = mean(estimate / std_error < -1.96),
      .by = proxy
    )
}

# Attitude change on a learning proxy and the distance from one's small group,
# one regression per poll and attitude index (the model of the paper's opening
# illustration). The learning coefficient is multiplied by the sign of the
# poll's net change on that index, so a positive value means those who learned
# more moved further in the direction the sample moved, and is expressed per
# standard deviation of the proxy.
index_regressions <- function(polardata, indices) {
  purrr::pmap(indices, function(dpnum, att_index, t1var, t2_t3var, ...) {
    poll <- polardata |>
      dplyr::filter(.data$dpnum == .env$dpnum) |>
      dplyr::transmute(
        group = pollgroup,
        a1 = .data[[t1var]], a2 = .data[[t2_t3var]],
        x1 = t1know, x2 = t2know
      ) |>
      dplyr::filter(!is.na(group), !is.na(a1), !is.na(a2), !is.na(x1), !is.na(x2)) |>
      dplyr::mutate(
        group_distance = a1 - (sum(a1) - a1) / (dplyr::n() - 1),
        .by = group
      ) |>
      dplyr::filter(is.finite(group_distance)) |>
      dplyr::mutate(change = a2 - a1, gain = x2 - x1)
    if (nrow(poll) < 50 || stats::sd(poll$change) == 0) {
      return(NULL)
    }
    direction <- sign(mean(poll$change))
    fit_proxy <- function(formula, proxy) {
      fit <- stats::lm(formula, data = poll)
      vc <- sandwich::vcovCL(fit, cluster = ~group, type = "HC1")
      scale <- direction * stats::sd(poll[[proxy]])
      tibble::tibble(estimate = stats::coef(fit)[[proxy]] * scale, std_error = sqrt(vc[proxy, proxy]) * abs(scale))
    }
    dplyr::bind_rows(
      gain = fit_proxy(change ~ gain + group_distance, "gain"),
      x2 = fit_proxy(change ~ x2 + group_distance, "x2"),
      x2_given_x1 = fit_proxy(change ~ x2 + x1 + group_distance, "x2"),
      x1 = fit_proxy(change ~ x1 + group_distance, "x1"),
      .id = "proxy"
    ) |>
      dplyr::mutate(dpnum = dpnum, index = att_index, n = nrow(poll), net_change = mean(poll$change), .before = 1)
  }) |>
    purrr::list_rbind()
}

run_empirical <- function() {
  verify_sources()
  polardata <- read_polardata()
  indices <- read_indices()
  write_tab(who_learns(polardata), "who_learns.csv")

  attitudes <- index_regressions(polardata, indices)
  write_tab(attitudes, "attitudes.csv")
  write_tab(summarise_attitudes(attitudes), "attitudes_summary.csv")

  validation <- validate_item_subsets(read_item_validation())
  write_tab(validation$results, "item_validation.csv")
  write_tab(validation$partitions, "item_partitions.csv")
  write_tab(validation$samples, "item_validation_samples.csv")
  write_tab(summarise_item_validation(validation$results), "item_validation_summary.csv")
}
