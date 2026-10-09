test_that("poll features use the same paired sample at both waves", {
  poll <- tibble::tibble(
    pollname = "Example", t1know = c(0, 0.2, 0.4, 1, NA),
    t2know = c(0.1, 0.4, 0.5, NA, 0.9)
  )
  paired <- poll[stats::complete.cases(poll), ]
  features <- poll_features(poll)
  expect_equal(features, poll_features(paired))
  expect_equal(features$slope_x2_x1, unname(stats::coef(stats::lm(t2know ~ t1know, poll))[[2]]))
  expect_equal(features$mean_x1, mean(paired$t1know))
  expect_equal(features$sd_x1, stats::sd(paired$t1know))
})
