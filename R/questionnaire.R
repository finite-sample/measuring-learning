piecewise_scores <- function(initial, b = 2, beta = 1.5) {
  stopifnot(all(is.finite(initial)), all(initial >= 0), b > 0, beta > 1)
  pre <- pmin(b * initial, 1)
  post <- pmin(b * beta * initial, 1)
  tibble::tibble(
    initial = initial, pre = pre, post = post, gain = post - pre,
    true_gain = (beta - 1) * initial
  )
}

piecewise_moments <- function(b = 2, beta = 1.5, maximum = 1) {
  stopifnot(b > 0, beta > 1, maximum > 0)
  boundaries <- pmin(c(0, 1 / (b * beta), 1 / b, maximum), maximum)
  lower <- head(boundaries, -1)
  upper <- tail(boundaries, -1)
  tibble::tibble(
    region = c("Below both ceilings", "Reaches follow-up ceiling", "At both ceilings"),
    lower = lower, upper = upper, probability = (upper - lower) / maximum,
    mean_initial = ifelse(upper > lower, (lower + upper) / 2, NA_real_),
    variance_initial = ifelse(upper > lower, (upper - lower)^2 / 12, NA_real_)
  )
}

score_metrics <- function(pre, post, truth) {
  x1 <- rowMeans(pre)
  x2 <- rowMeans(post)
  scores <- list(baseline = x1, observed_gain = x2 - x1, post_score = x2)
  purrr::imap_dfr(scores, function(score, estimator) {
    tibble::tibble(
      estimator = estimator, n = length(truth),
      correlation = cor_if_variable(score, truth),
      rank_correlation = cor_if_variable(rank(score), rank(truth)),
      ceiling_pre = mean(x1 == 1), ceiling_post = mean(x2 == 1)
    )
  })
}

questionnaire_world <- function(n = 1000, seed = 20261010) {
  withr::with_seed(seed, {
    list(
      initial = stats::rgamma(n, shape = 1),
      rate = exp(log(0.5) + 0.5 * stats::rnorm(n)),
      item_quantiles = stats::runif(30),
      pre_noise = matrix(stats::runif(n * 30), n),
      post_noise = matrix(stats::runif(n * 30), n)
    )
  })
}

questionnaire_responses <- function(world, items, difficulty, learning_form) {
  stopifnot(items %in% c(5, 10, 15, 30), difficulty %in% c("easy", "broad"))
  bounds <- if (difficulty == "easy") c(0.05, 0.6) else c(0.05, 0.95)
  difficulties <- stats::qgamma(bounds[1] + diff(bounds) * world$item_quantiles, shape = 1)
  gain <- switch(learning_form,
    proportional = world$initial * world$rate,
    additive = world$rate,
    catch_up = world$rate * pmin(1 / world$initial, 5),
    stop("Unknown learning form")
  )
  response <- function(knowledge, noise) {
    probability <- 0.25 + 0.75 * stats::plogis(3 * outer(log(knowledge), log(difficulties), `-`))
    (noise < probability)[, seq_len(items), drop = FALSE] * 1
  }
  list(
    pre = response(world$initial, world$pre_noise),
    post = response(world$initial + gain, world$post_noise), gain = gain
  )
}

compare_questionnaires <- function(replications = 100, n = 1000, seed = 20261010) {
  conditions <- expand.grid(
    items = c(5L, 10L, 15L, 30L), difficulty = c("easy", "broad"),
    learning_form = c("proportional", "additive", "catch_up"), stringsAsFactors = FALSE
  )
  purrr::map_dfr(seq_len(replications), function(replication) {
    world <- questionnaire_world(n, seed + replication)
    purrr::pmap_dfr(conditions, function(items, difficulty, learning_form) {
      data <- questionnaire_responses(world, items, difficulty, learning_form)
      score_metrics(data$pre, data$post, data$gain) |>
        dplyr::mutate(
          replicate = replication, seed = seed + replication, items = items,
          difficulty = difficulty, learning_form = learning_form
        )
    })
  })
}

domain_world <- function(n = 1000, seed = 20262010) {
  withr::with_seed(seed, {
    initial <- matrix(stats::rgamma(n * 2, shape = 1), n)
    rate <- exp(log(0.5) + 0.5 * stats::rnorm(n))
    target <- rowMeans(initial) * rate
    exposed <- sample.int(2, n, replace = TRUE)
    change <- matrix(0, n, 2)
    change[cbind(seq_len(n), exposed)] <- 2 * target
    difficulty <- matrix(stats::qgamma(stats::runif(20, 0.05, 0.95), 1), 10, 2)
    response <- function(knowledge) {
      lapply(seq_len(2), function(domain) {
        p <- 0.25 + 0.75 * stats::plogis(3 * outer(log(knowledge[, domain]), log(difficulty[, domain]), `-`))
        matrix(stats::runif(n * 10), n) < p
      })
    }
    list(
      initial = initial, later = initial + change, gain = target,
      exposed = exposed, pre = response(initial), post = response(initial + change)
    )
  })
}

compare_domains <- function(replications = 100, n = 1000, seed = 20262010) {
  purrr::map_dfr(seq_len(replications), function(replication) {
    world <- domain_world(n, seed + replication)
    purrr::map_dfr(c("balanced", "one_domain"), function(coverage) {
      select <- function(x) {
        if (coverage == "one_domain") x[[1]] else cbind(x[[1]][, 1:5], x[[2]][, 1:5])
      }
      purrr::imap_dfr(list(
        all = rep(TRUE, n), first_domain = world$exposed == 1,
        second_domain = world$exposed == 2
      ), function(take, exposure) {
        score_metrics(
          select(world$pre)[take, , drop = FALSE],
          select(world$post)[take, , drop = FALSE], world$gain[take]
        ) |>
          dplyr::mutate(
            replicate = replication, seed = seed + replication,
            coverage = coverage, exposure = exposure
          )
      })
    })
  })
}

paired_score_differences <- function(results, keys) {
  results |>
    dplyr::select(dplyr::all_of(keys), estimator, correlation, rank_correlation) |>
    tidyr::pivot_wider(names_from = estimator, values_from = c(correlation, rank_correlation)) |>
    dplyr::mutate(
      post_minus_gain = correlation_post_score - correlation_observed_gain,
      post_minus_baseline = correlation_post_score - correlation_baseline,
      rank_post_minus_gain = rank_correlation_post_score - rank_correlation_observed_gain
    )
}
