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
