read_tab <- \(name) readr::read_csv(file.path("../../tabs", name), show_col_types = FALSE)

test_that("threshold items are known exactly when true knowledge exceeds difficulty", {
  x <- answer_items(c(0.5, 2, 3), difficulty = c(1, 2.5), discrimination = Inf, guess = 0)
  expect_equal(x, c(0, 0.5, 1))
})

test_that("post-process knowledge never correlates negatively with true gain when the knowledgeable learn more", {
  sims <- read_tab("simulations.csv") |> dplyr::filter(plausible, learning_form == "proportional")
  expect_gt(nrow(sims), 500)
  expect_true(all(sims$x2 > 0))
  expect_gt(mean(sims$x2 > sims$observed_gain), 0.95)
})

test_that("gain and post-process knowledge give the same effect once initial knowledge is held constant", {
  d <- tibble::tibble(x1 = runif(200), ba = rbinom(200, 1, 0.4), x2 = pmin(1, x1 + 0.1 * ba + runif(200, 0, 0.2)))
  gain_fit <- stats::coef(stats::lm(I(x2 - x1) ~ ba + x1, d))
  x2_fit <- stats::coef(stats::lm(x2 ~ ba + x1, d))
  expect_equal(gain_fit[["ba"]], x2_fit[["ba"]])
  expect_equal(x2_fit[["x1"]] - gain_fit[["x1"]], 1)
})

test_that("the empirical analyses cover every public poll and attitude index", {
  expect_equal(nrow(read_tab("poll_features.csv")), 21)
  expect_true(all(read_tab("poll_features.csv")$r_gain_x1 < 0))
  expect_equal(unique(read_tab("attitudes_summary.csv")$indices), 129)
  learners <- read_tab("who_learns.csv") |> dplyr::filter(pollname == "Pooled estimate")
  expect_equal(nrow(learners), 3)
})
