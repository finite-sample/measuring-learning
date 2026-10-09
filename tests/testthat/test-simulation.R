test_that("constant outcomes have undefined correlations without numerical warnings", {
  sim <- tibble::tibble(x1 = c(0, 0.25, 0.5, 1), x2 = 1, true_gain = 1:4, observed_gain = 1 - x1)
  expect_warning(result <- proxy_correlations(sim), NA)
  expect_true(is.na(result$x2))
  expect_true(is.na(result$x2_given_x1))
  expect_true(is.na(result$x2_residualized))
})

test_that("score correlations retain learning explained by baseline in the target", {
  initial <- c(-3, -1, 1, 3)
  shock <- c(1, -1, -1, 1)
  sim <- tibble::tibble(
    x1 = initial, x2 = 2 * initial + shock,
    true_gain = 10 * initial + shock, observed_gain = x2 - x1
  )
  result <- proxy_correlations(sim)
  expect_equal(result$x2_given_x1, 1)
  expect_equal(result$x2_residualized, 1 / sqrt(501))
  expect_gt(result$x2, result$x2_residualized)
})

test_that("nonfinite features do not define a plausible poll", {
  features <- tibble::tibble(mean_x1 = 0.5, mean_gain = 0.2, sd_x1 = 0.1, r_x1_x2 = 0.4, r_gain_x1 = -0.2)
  sim <- features
  sim$r_x1_x2 <- NA_real_
  expect_identical(looks_like_a_poll(sim, features), FALSE)
})

test_that("item probabilities reject invalid parameters", {
  expect_error(answer_items(c(0, 1), 1, -Inf, 0))
  expect_error(answer_items(c(0, 1), 1, Inf, 1.1))
  expect_error(answer_items(c(0, 1), 0, 1, 0))
  expect_equal(answer_items(c(0, 1), 1, Inf, 1), c(1, 1))
})

test_that("simulation grids are reproducible and preserve the caller's RNG", {
  withr::local_seed(10)
  before <- .Random.seed
  a <- simulate_grid(draws = 5, n = 100, seed = 50)
  expect_identical(.Random.seed, before)
  expect_identical(a, simulate_grid(draws = 5, n = 100, seed = 50))
})
