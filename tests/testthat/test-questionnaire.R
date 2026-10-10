test_that("piecewise regions partition the distribution and reproduce its moments", {
  for (maximum in c(0.2, 0.4, 1)) {
    regions <- piecewise_moments(maximum = maximum)
    take <- regions$probability > 0
    expect_equal(sum(regions$probability), 1)
    expect_equal(sum(regions$probability[take] * regions$mean_initial[take]), maximum / 2)
    second <- regions$variance_initial[take] + regions$mean_initial[take]^2
    expect_equal(sum(regions$probability[take] * second), maximum^2 / 3)
  }
  boundaries <- c(1 / 3, 1 / 2)
  for (x in boundaries) {
    scores <- piecewise_scores(c(x - 1e-9, x, x + 1e-9))
    expect_lt(max(abs(diff(scores$gain))), 1e-7)
  }
  curves <- piecewise_scores(seq(0, 1, length.out = 1001))
  expect_true(all(curves$pre >= 0 & curves$post <= 1))
  expect_equal(curves$gain[curves$initial >= 0.5], rep(0, sum(curves$initial >= 0.5)))
  expect_equal(stats::integrate(function(x) piecewise_scores(x)$gain, 0, 1)$value, 1 / 12)
  expect_equal(stats::integrate(function(x) piecewise_scores(x)$true_gain, 0, 1)$value, 1 / 4)
})

test_that("longer questionnaires retain the exact same respondents and shorter responses", {
  world <- questionnaire_world(200, 42)
  short <- questionnaire_responses(world, 5, "easy", "proportional")
  long <- questionnaire_responses(world, 30, "easy", "proportional")
  broad <- questionnaire_responses(world, 5, "broad", "proportional")
  expect_identical(short$pre, long$pre[, 1:5])
  expect_identical(short$post, long$post[, 1:5])
  expect_identical(short$gain, broad$gain)
  expect_true(all(broad$pre <= short$pre))
  expect_true(all(broad$post <= short$post))
  withr::local_seed(20)
  before <- .Random.seed
  expect_identical(world, questionnaire_world(200, 42))
  expect_identical(.Random.seed, before)
})

test_that("domain coverage changes measurement while preserving total learning", {
  world <- domain_world(500, 21)
  expect_equal(rowMeans(world$later - world$initial), world$gain)
  change <- world$later - world$initial
  expect_true(all(change[cbind(seq_len(500), 3 - world$exposed)] == 0))
  result <- compare_domains(2, 100, 32)
  paired <- paired_score_differences(result, c("replicate", "coverage", "exposure"))
  expect_equal(nrow(paired), 12)
  expect_equal(paired$post_minus_gain, paired$correlation_post_score - paired$correlation_observed_gain)
})
