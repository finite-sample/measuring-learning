test_that("missing discussion groups are excluded from clustered attitude models", {
  withr::local_seed(42)
  d <- tibble::tibble(
    dpnum = 1, pollgroup = rep(seq_len(10), each = 10),
    a1 = runif(100), a2 = runif(100), t1know = runif(100), t2know = runif(100)
  )
  d$pollgroup[1:4] <- NA
  indices <- tibble::tibble(dpnum = 1, att_index = "Example", t1var = "a1", t2_t3var = "a2")
  actual <- index_regressions(d, indices)
  expected <- index_regressions(d[!is.na(d$pollgroup), ], indices)
  expect_equal(actual, expected)
  expect_equal(actual$n, rep(96L, 4))
})

test_that("education models use the fixed upstream median flag and complete clustered sample", {
  withr::local_seed(9)
  d <- tibble::tibble(
    pollname = rep(c("A", "B"), each = 100), pollgroup = rep(seq_len(20), each = 10),
    education_above_median = rep(c(FALSE, TRUE), 100), educ3 = 0,
    t1know = runif(200),
    t2know = 0.2 + 0.5 * t1know + 0.1 * education_above_median + rnorm(200, sd = 0.02)
  )
  d$pollgroup[1] <- NA
  d$education_above_median[2] <- NA
  d$t2know[3] <- NA
  result <- who_learns(d)
  sample <- d[stats::complete.cases(d), ]
  expected <- coef(lm(t2know ~ as.numeric(education_above_median) + t1know, sample[sample$pollname == "A", ]))[[2]]
  actual <- result[result$pollname == "A" & result$proxy == "Post-process knowledge given initial", ]
  expect_equal(actual$estimate, unname(expected))
  expect_equal(actual$n, 97L)
  expect_equal(actual$n_above + actual$n_at_or_below, actual$n)
  expect_equal(unique(result$n[result$pollname == "A"]), 97L)
})
