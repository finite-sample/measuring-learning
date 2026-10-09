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

# Correlations of each proxy with true gain. Given x1, the observed gain and x2
# carry the same information: their partial correlations with true gain are equal.
cor_if_variable <- function(x, y) {
  if (length(x) < 2 || any(!is.finite(x)) || any(!is.finite(y)) || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }
  stats::cor(x, y)
}

proxy_correlations <- function(sim) {
  partial <- if (stats::sd(sim$x2) == 0 || stats::sd(sim$true_gain) == 0) {
    NA_real_
  } else {
    cor_if_variable(stats::resid(stats::lm(x2 ~ x1, sim)), stats::resid(stats::lm(true_gain ~ x1, sim)))
  }
  tibble::tibble(
    observed_gain = cor_if_variable(sim$observed_gain, sim$true_gain),
    x2 = cor_if_variable(sim$x2, sim$true_gain),
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
