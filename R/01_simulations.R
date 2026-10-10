# A person knows item j at time t with probability plogis(a * (log X_t - log d_j));
# a = Inf makes this a threshold (knows the item iff X_t > d_j). Items not known
# are answered correctly by lucky guess with probability c.
answer_items <- function(x_true, difficulty, discrimination, guess) {
  stopifnot(
    is.numeric(x_true), length(x_true) > 0, all(is.finite(x_true)), all(x_true >= 0),
    is.numeric(difficulty), length(difficulty) > 0, all(is.finite(difficulty)), all(difficulty > 0),
    is.numeric(discrimination), length(discrimination) == 1L, !is.na(discrimination), discrimination > 0,
    is.numeric(guess), length(guess) == 1L, is.finite(guess), guess >= 0, guess <= 1
  )
  known <- if (is.infinite(discrimination)) {
    outer(x_true, difficulty, `>`) * 1
  } else {
    p <- stats::plogis(discrimination * outer(log(x_true), log(difficulty), `-`))
    matrix(stats::rbinom(length(p), 1, p), nrow = length(x_true))
  }
  lucky <- matrix(stats::rbinom(length(known), 1, guess), nrow = nrow(known))
  rowMeans(pmax(known, lucky))
}

# One simulated process. True knowledge X_1 is right-skewed (gamma). Learning is
# proportional to what one already knows (Delta X = X_1 * r, r lognormal: the
# knowledgeable learn more), additive (Delta X = r * mean(X_1): unrelated to X_1),
# or catch-up (Delta X = r * mean(X_1)^2 / X_1, capped: the less knowledgeable
# learn more). The propositions cover the first two; the third is a stress test.
# Item difficulties are quantiles of the X_1 distribution drawn from
# [easiest, hardest]: an easy questionnaire has most items below the median.
simulate_process <- function(n = 1000, items = 10, easiest = 0.05, hardest = 0.6,
                             shape = 1, learning = 0.5, learning_sd = 0.5,
                             discrimination = Inf, guess = 0, learning_form = "proportional") {
  x1_true <- stats::rgamma(n, shape = shape)
  rate <- exp(log(learning) + learning_sd * stats::rnorm(n))
  gain <- switch(learning_form,
    proportional = x1_true * rate,
    additive = rate * mean(x1_true),
    catch_up = rate * pmin(mean(x1_true)^2 / x1_true, 5 * mean(x1_true))
  )
  x2_true <- x1_true + gain
  difficulty <- stats::quantile(x1_true, stats::runif(items, easiest, hardest), names = FALSE)
  x1 <- answer_items(x1_true, difficulty, discrimination, guess)
  x2 <- answer_items(x2_true, difficulty, discrimination, guess)
  tibble::tibble(true_gain = x2_true - x1_true, x1 = x1, x2 = x2, observed_gain = x2 - x1)
}

# Keep score prediction and partial association separate: residualizing true
# gain changes the target, whereas residualizing only x2 keeps it fixed.
proxy_correlations <- function(sim) {
  residual_score <- stats::resid(stats::lm(x2 ~ x1, sim))
  partial <- if (stats::sd(sim$x2) == 0 || stats::sd(sim$true_gain) == 0) {
    NA_real_
  } else {
    cor_if_variable(stats::resid(stats::lm(x2 ~ x1, sim)), stats::resid(stats::lm(true_gain ~ x1, sim)))
  }
  tibble::tibble(
    baseline = cor_if_variable(sim$x1, sim$true_gain),
    observed_gain = cor_if_variable(sim$observed_gain, sim$true_gain),
    x2 = cor_if_variable(sim$x2, sim$true_gain),
    x2_residualized = if (stats::sd(sim$x2) == 0) NA_real_ else cor_if_variable(residual_score, sim$true_gain),
    x2_given_x1 = partial,
    mean_x1 = mean(sim$x1),
    mean_gain = mean(sim$observed_gain),
    sd_x1 = stats::sd(sim$x1),
    r_x1_x2 = cor_if_variable(sim$x1, sim$x2),
    r_gain_x1 = cor_if_variable(sim$observed_gain, sim$x1)
  )
}

# Observed features of the Deliberative Polls: a simulated process is kept only
# if what it would show a researcher lies within the range of real polls.
poll_features <- function(polardata) {
  polardata |>
    dplyr::filter(!is.na(t1know), !is.na(t2know)) |>
    dplyr::summarise(
      mean_x1 = mean(t1know),
      mean_gain = mean(t2know - t1know),
      sd_x1 = stats::sd(t1know),
      r_x1_x2 = stats::cor(t1know, t2know),
      r_gain_x1 = stats::cor(t2know - t1know, t1know),
      slope_x2_x1 = stats::cov(t1know, t2know) / stats::var(t1know),
      .by = pollname
    )
}

looks_like_a_poll <- function(sims, features) {
  inside <- \(x, name) is.finite(x) & x >= min(features[[name]]) & x <= max(features[[name]])
  inside(sims$mean_x1, "mean_x1") & inside(sims$mean_gain, "mean_gain") & inside(sims$sd_x1, "sd_x1") &
    inside(sims$r_x1_x2, "r_x1_x2") & inside(sims$r_gain_x1, "r_gain_x1")
}

# Draws process parameters over plausible ranges and records the correlations.
simulate_grid <- function(draws = 2000, n = 1000, seed = 20260923, learning_form = "proportional") {
  withr::with_seed(seed, {
    params <- tibble::tibble(
      items = sample(c(5, 7, 10, 15), draws, replace = TRUE),
      easiest = stats::runif(draws, 0, 0.3),
      hardest = stats::runif(draws, 0.35, 0.95),
      shape = stats::runif(draws, 0.5, 3),
      learning = stats::runif(draws, 0.1, 1.5),
      learning_sd = stats::runif(draws, 0.1, 1),
      discrimination = sample(c(Inf, 1, 3), draws, replace = TRUE),
      guess = sample(c(0, 0.1, 0.25), draws, replace = TRUE),
      learning_form = learning_form
    )
    results <- purrr::pmap(params, \(...) proxy_correlations(simulate_process(n = n, ...))) |>
      purrr::list_rbind()
  })
  dplyr::bind_cols(params, results)
}

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

validate_item_pair <- function(pre, post) {
  stopifnot(
    is.matrix(pre), is.matrix(post), identical(dim(pre), dim(post)),
    ncol(pre) >= 3L, nrow(pre) > 0L,
    all(is.na(pre) | pre %in% c(0, 1)), all(is.na(post) | post %in% c(0, 1))
  )
}

# PRE is log knowledge; CHANGE is its change. Baseline mean zero and variance
# one fix the scale. Matched items share slopes and intercepts across waves.
fit_joint_irt <- function(pre, post, guess = 0, max_cycles = 1500) {
  validate_item_pair(pre, post)
  items <- ncol(pre)
  stopifnot(length(guess) %in% c(1L, items), all(is.finite(guess)), all(guess >= 0 & guess < 1))
  data <- cbind(pre, post)
  colnames(data) <- paste0("item", seq_len(2 * items))
  model <- paste0(
    "PRE = 1-", 2 * items, "\nCHANGE = ", items + 1, "-", 2 * items,
    "\nCOV = PRE*CHANGE, CHANGE*CHANGE\nMEAN = CHANGE"
  )
  guesses <- rep(rep(guess, length.out = items), 2)
  parameters <- mirt::mirt(data, model, itemtype = "2PL", guess = guesses, pars = "values", verbose = FALSE)
  parameters$value[parameters$name == "COV_22"] <- 0.15
  parameters$value[parameters$name == "MEAN_2"] <- 0.4
  parameters$lbound[parameters$name == "COV_22"] <- 0.0025
  active <- parameters$name %in% c("a1", "a2") & parameters$est
  parameters$lbound[active] <- 0.2
  parameters$ubound[active] <- 5
  constraints <- unlist(lapply(seq_len(items), function(j) {
    first <- parameters$item == paste0("item", j)
    second <- parameters$item == paste0("item", j + items)
    list(
      parameters$parnum[(first & parameters$name == "a1") | (second & parameters$name %in% c("a1", "a2"))],
      parameters$parnum[(first | second) & parameters$name == "d"]
    )
  }), recursive = FALSE)
  warnings <- character()
  change_grid <- seq(-2, 3, length.out = 61)
  for (attempt in seq_len(3)) {
    fit <- withCallingHandlers(
      mirt::mirt(
        data, model,
        itemtype = "2PL", guess = guesses, pars = parameters, constrain = constraints,
        technical = list(
          NCYCLES = max_cycles,
          customTheta = as.matrix(expand.grid(seq(-6, 6, length.out = 41), change_grid))
        ),
        accelerate = "none", verbose = FALSE
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    coefficients <- mirt::coef(fit, simplify = TRUE)
    change_mean <- coefficients$means[2]
    change_sd <- sqrt(coefficients$cov[2, 2])
    outside <- stats::pnorm(min(change_grid), change_mean, change_sd) +
      stats::pnorm(max(change_grid), change_mean, change_sd, lower.tail = FALSE)
    if (outside < 1e-6) break
    parameters <- mirt::mod2values(fit)
    change_grid <- seq(
      min(min(change_grid), change_mean - 6 * change_sd),
      max(max(change_grid), change_mean + 6 * change_sd),
      by = 1 / 12
    )
  }
  if (outside >= 1e-6) stop("Change distribution extends beyond the integration grid")
  list(
    items = coefficients$items, mean = coefficients$means, covariance = coefficients$cov,
    converged = mirt::extract.mirt(fit, "converged"), warnings = unique(warnings),
    iterations = mirt::extract.mirt(fit, "iterations"), quadrature_tail = outside
  )
}

# Integrate gain itself, not the gain between two posterior mean abilities.
# Intervals condition on the fitted population and item parameters.
posterior_irt_learning <- function(model, pre, post, nodes = 41) {
  validate_item_pair(pre, post)
  stopifnot(nrow(model$items) == 2 * ncol(pre), nodes >= 5L)
  quadrature <- statmod::gauss.quad.prob(nodes, dist = "normal")
  pairs <- expand.grid(seq_len(nodes), seq_len(nodes))
  standard <- cbind(quadrature$nodes[pairs[[1]]], quadrature$nodes[pairs[[2]]])
  theta <- sweep(standard %*% chol(model$covariance), 2, model$mean, `+`)
  prior <- quadrature$weights[pairs[[1]]] * quadrature$weights[pairs[[2]]]
  linear <- sweep(theta %*% t(model$items[, c("a1", "a2")]), 2, model$items[, "d"], `+`)
  probability <- sweep(sweep(stats::plogis(linear), 2, 1 - model$items[, "g"], `*`), 2, model$items[, "g"], `+`)
  probability <- pmin(pmax(probability, 1e-15), 1 - 1e-15)
  answers <- cbind(pre, post)
  correct <- (!is.na(answers) & answers == 1) * 1
  incorrect <- (!is.na(answers) & answers == 0) * 1
  log_weight <- correct %*% t(log(probability)) + incorrect %*% t(log1p(-probability))
  log_weight <- sweep(log_weight, 2, log(prior), `+`)
  log_weight <- log_weight - apply(log_weight, 1, max)
  weight <- exp(log_weight)
  weight <- weight / rowSums(weight)
  initial <- exp(theta[, 1])
  later <- exp(theta[, 1] + theta[, 2])
  gain <- later - initial
  order_gain <- order(gain)
  limits <- t(apply(weight[, order_gain, drop = FALSE], 1, function(w) {
    cumulative <- cumsum(w)
    vapply(c(0.025, 0.975), function(p) gain[order_gain[which(cumulative >= p)[1]]], numeric(1))
  }))
  tibble::tibble(
    estimate = as.vector(weight %*% gain), lower = limits[, 1], upper = limits[, 2],
    initial = as.vector(weight %*% initial), later = as.vector(weight %*% later),
    probability_positive = as.vector(weight %*% (gain > 0)),
    items_pre = rowSums(!is.na(pre)), items_post = rowSums(!is.na(post))
  )
}

simulate_irt_panel <- function(n, items = 10, difficulty = "easy", learning_form = "proportional",
                               heterogeneous_guess = FALSE, drift = FALSE) {
  initial <- stats::rnorm(n)
  rate <- exp(log(0.5) + 0.5 * stats::rnorm(n))
  knowledge <- exp(initial)
  gain <- switch(learning_form,
    proportional = knowledge * rate,
    additive = exp(0.5) * rate,
    catch_up = rate * pmin(exp(1) / knowledge, 5 * exp(0.5))
  )
  later <- log(knowledge + gain)
  difficulties <- if (difficulty == "easy") seq(-2, 0.2, length.out = items) else seq(-1.5, 1.5, length.out = items)
  guesses <- if (heterogeneous_guess) rep(c(0, 0.25, 0.5), length.out = items) else rep(0.25, items)
  answers <- function(theta, shift = 0) {
    p <- stats::plogis(1.5 * outer(theta, difficulties + shift, `-`))
    p <- sweep(sweep(p, 2, 1 - guesses, `*`), 2, guesses, `+`)
    matrix(stats::rbinom(length(p), 1, p), nrow = n)
  }
  list(pre = answers(initial), post = answers(later, if (drift) -0.4 else 0), gain = gain, guess = guesses)
}

compare_learning_estimators <- function(scenario, replicate, seed, training_n = 300, test_n = 1000) {
  withr::with_seed(seed, {
    parameters <- as.list(scenario[setdiff(names(scenario), "scenario")])
    train <- do.call(simulate_irt_panel, c(list(n = training_n), parameters))
    test <- do.call(simulate_irt_panel, c(list(n = test_n), parameters))
    x1 <- rowMeans(test$pre)
    x2 <- rowMeans(test$post)
    adjusted <- rowMeans(sweep(sweep(test$post, 2, test$guess, `-`), 2, 1 - test$guess, `/`))
    scores <- list(baseline = x1, observed_gain = x2 - x1, post_score = x2, guess_adjusted_post = adjusted)
    status <- "ok"
    warnings <- ""
    fitted <- tryCatch(fit_joint_irt(train$pre, train$post, train$guess), error = identity)
    if (inherits(fitted, "error")) {
      status <- conditionMessage(fitted)
    } else {
      warnings <- paste(fitted$warnings, collapse = " | ")
      if (!fitted$converged) status <- "did not converge"
      if (any(grepl("unstable|non-positive definite", fitted$warnings))) status <- "unstable fit"
    }
    intervals <- list()
    if (status == "ok") {
      predictions <- tryCatch(
        {
          missing <- matrix(NA_real_, nrow = test_n, ncol = ncol(test$pre))
          list(
            joint = posterior_irt_learning(fitted, test$pre, test$post),
            post = posterior_irt_learning(fitted, missing, test$post),
            pre = posterior_irt_learning(fitted, test$pre, missing)
          )
        },
        error = identity
      )
      if (inherits(predictions, "error")) {
        status <- conditionMessage(predictions)
      } else {
        scores$joint_irt <- predictions$joint$estimate
        scores$post_only_irt <- predictions$post$estimate
        scores$separate_irt <- predictions$post$later - predictions$pre$initial
        intervals <- list(joint_irt = predictions$joint, post_only_irt = predictions$post)
      }
    }
    purrr::imap(scores, function(score, estimator) {
      purrr::imap(list(all = rep(TRUE, test_n), baseline_perfect = x1 == 1), function(take, subset) {
        truth <- test$gain[take]
        prediction <- score[take]
        amount <- grepl("irt$", estimator)
        interval <- intervals[[estimator]]
        tibble::tibble(
          scenario = scenario$scenario, replicate = replicate, seed = seed, subset = subset,
          estimator = estimator, n = sum(take), fit_status = status, fit_warnings = warnings,
          correlation = cor_if_variable(prediction, truth),
          rank_correlation = cor_if_variable(rank(prediction), rank(truth)),
          rmse = if (amount) sqrt(mean((prediction - truth)^2)) else NA_real_,
          bias = if (amount) mean(prediction - truth) else NA_real_,
          coverage = if (!is.null(interval)) {
            mean(truth >= interval$lower[take] & truth <= interval$upper[take])
          } else {
            NA_real_
          }
        )
      }) |>
        purrr::list_rbind()
    }) |>
      purrr::list_rbind()
  })
}

estimator_scenarios <- function() {
  tibble::tibble(
    scenario = c("easy_5", "easy_10", "easy_15", "broad_10", "additive", "catch_up", "varying_guess", "item_drift"),
    items = c(5L, 10L, 15L, rep(10L, 5)),
    difficulty = c(rep("easy", 3), "broad", rep("easy", 4)),
    learning_form = c(rep("proportional", 4), "additive", "catch_up", rep("proportional", 2)),
    heterogeneous_guess = c(rep(FALSE, 6), TRUE, FALSE),
    drift = c(rep(FALSE, 7), TRUE)
  )
}

run_simulations <- function() {
  verify_sources()
  features <- poll_features(read_polardata())
  write_tab(features, "poll_features.csv")

  simulations <- c("proportional", "additive", "catch_up") |>
    purrr::map(\(form) simulate_grid(draws = 4000, n = 300, learning_form = form)) |>
    purrr::list_rbind() |>
    dplyr::mutate(plausible = looks_like_a_poll(dplyr::pick(dplyr::everything()), features))
  write_tab(simulations, "simulations.csv")

  questionnaires <- compare_questionnaires()
  write_tab(questionnaires, "questionnaires.csv")
  write_tab(
    paired_score_differences(questionnaires, c("replicate", "items", "difficulty", "learning_form")),
    "questionnaire_differences.csv"
  )
  domains <- compare_domains()
  write_tab(domains, "domains.csv")
  write_tab(paired_score_differences(domains, c("replicate", "coverage", "exposure")), "domain_differences.csv")
  write_tab(piecewise_scores(seq(0, 1, length.out = 301)), "piecewise.csv")
  write_tab(piecewise_moments(), "piecewise_moments.csv")
}

run_estimation <- function() {
  scenarios <- estimator_scenarios()
  jobs <- expand.grid(scenario = seq_len(nrow(scenarios)), replicate = seq_len(10))
  cores <- as.integer(Sys.getenv("IRT_CORES", "1"))
  stopifnot(is.finite(cores), cores >= 1)
  results <- parallel::mclapply(seq_len(nrow(jobs)), function(i) {
    job <- jobs[i, ]
    message("Estimator comparison ", i, "/", nrow(jobs), ": ", scenarios$scenario[job$scenario])
    compare_learning_estimators(scenarios[job$scenario, ], job$replicate, 20261009L + i)
  }, mc.cores = cores, mc.set.seed = FALSE) |>
    purrr::list_rbind()
  write_tab(results, "estimator_comparison.csv")
}
