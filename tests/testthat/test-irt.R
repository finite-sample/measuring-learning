irt_fixture <- function() {
  list(
    items = cbind(a1 = rep(1.5, 6), a2 = rep(c(0, 1.5), each = 3), d = rep(c(1, 0, -1), 2), g = 0.25, u = 1),
    mean = c(0, 0.4), covariance = matrix(c(1, 0.1, 0.1, 0.15), 2)
  )
}

test_that("posterior integration recovers the analytic prior mean of gain without answers", {
  model <- irt_fixture()
  missing <- matrix(NA_real_, 1, 3)
  result <- posterior_irt_learning(model, missing, missing)
  expected <- exp(sum(model$mean) + sum(model$covariance) / 2) - exp(0.5)
  expect_equal(result$estimate, expected, tolerance = 1e-8)
  expect_equal(result$items_pre, 0)
  expect_equal(result$items_post, 0)
  expect_lt(result$lower, result$estimate)
  expect_gt(result$upper, result$estimate)
})

test_that("joint posterior scores preserve respondents and are stable to finer quadrature", {
  pre <- rbind(c(0, 0, 0), c(1, 1, 1), c(1, NA, 0))
  post <- rbind(c(1, 1, 0), c(1, 1, 1), c(1, 1, NA))
  scores <- posterior_irt_learning(irt_fixture(), pre, post)
  finer <- posterior_irt_learning(irt_fixture(), pre, post, nodes = 61)
  expect_equal(scores$estimate, finer$estimate, tolerance = 1e-5)
  expect_equal(scores$estimate, scores$later - scores$initial)
  reversed <- posterior_irt_learning(irt_fixture(), pre[3:1, ], post[3:1, ])
  expect_equal(scores$estimate, rev(reversed$estimate))
  expect_equal(scores$items_post, c(3, 3, 2))
})

test_that("common guessing correction preserves ranking and correlation", {
  score <- c(0, 0.2, 0.5, 0.9, 1)
  truth <- c(0.1, 0.2, 0.7, 0.4, 0.8)
  adjusted <- (score - 0.25) / 0.75
  expect_identical(rank(score), rank(adjusted))
  expect_equal(stats::cor(score, truth), stats::cor(adjusted, truth))
})

test_that("invalid item responses fail before fitting or scoring", {
  pre <- matrix(c(0, 1, 0), 1)
  expect_error(validate_item_pair(pre, matrix(0, 2, 3)))
  expect_error(validate_item_pair(pre, matrix(2, 1, 3)))
})

test_that("joint fitting preserves the anchor scale and matched-item constraints", {
  data <- withr::with_seed(20261009, simulate_irt_panel(200, items = 5, difficulty = "broad"))
  fit <- fit_joint_irt(data$pre, data$post, data$guess)
  expect_true(fit$converged)
  expect_equal(unname(fit$mean[1]), 0)
  expect_equal(unname(fit$covariance[1, 1]), 1)
  expect_equal(fit$items[1:5, "a1"], fit$items[6:10, "a1"], ignore_attr = TRUE)
  expect_equal(fit$items[6:10, "a1"], fit$items[6:10, "a2"], ignore_attr = TRUE)
  expect_equal(fit$items[1:5, "d"], fit$items[6:10, "d"], ignore_attr = TRUE)
  expect_equal(unname(fit$items[, "g"]), rep(0.25, 10))
  expect_lt(fit$quadrature_tail, 1e-6)
})
