validation_fixture <- function() {
  withr::with_seed(45, {
    pre <- matrix(stats::rbinom(240, 1, 0.5), 40, dimnames = list(NULL, paste0("item", 1:6)))
    post <- matrix(stats::rbinom(240, 1, 0.7), 40, dimnames = dimnames(pre))
    list(pre = pre, post = post)
  })
}

test_that("item splits are disjoint and their selection uses only training baselines", {
  data <- validation_fixture()
  training <- 1:25
  test <- 26:40
  for (mode in c("easy", "random")) {
    partition <- validation_partition(data$pre[training, ], mode, 12)
    expect_length(intersect(partition$predictor, partition$validation), 0)
    expect_setequal(c(partition$predictor, partition$validation), 1:6)
    predictions <- validation_predictions(
      data$pre[training, ], data$post[training, ],
      data$pre[test, ], data$post[test, ], partition
    )
    changed_pre <- data$pre
    changed_post <- data$post
    changed_pre[test, partition$validation] <- 0
    changed_post[test, partition$validation] <- 1
    changed <- validation_predictions(
      changed_pre[training, ], changed_post[training, ],
      changed_pre[test, ], changed_post[test, ], partition
    )
    expect_identical(predictions$scores, changed$scores)
    expect_equal(changed$validation_gain, rep(1, length(test)))
    expect_identical(partition, validation_partition(changed_pre[training, ], mode, 12))
  }
})

test_that("fold assignment preserves discussion groups and respondent identity", {
  people <- tibble::tibble(respondent_id = as.character(1:30), small_group_id = rep(letters[1:10], each = 3))
  folds <- validation_folds(people, 4)
  expect_true(all(vapply(split(folds, people$small_group_id), function(x) length(unique(x)), 1L) == 1))
  expect_setequal(folds, 1:5)
  expect_identical(folds, validation_folds(people, 4))
  people$small_group_id <- NA_character_
  expect_setequal(validation_folds(people, 4), 1:5)
})

test_that("missing questionnaires and missing items do not become zero scores", {
  responses <- tidyr::expand_grid(
    poll_id = "p", respondent_id = as.character(1:20),
    item_id = paste0("item", 1:4), wave = c("t1", "t2")
  ) |>
    dplyr::mutate(small_group_id = respondent_id, correct = 1)
  responses$correct[responses$respondent_id == "1" & responses$wave == "t2"] <- NA_real_
  responses$correct[responses$respondent_id == "2" & responses$item_id == "item1"] <- NA_real_
  panel <- item_panel(responses)
  expect_equal(panel$excluded, 2)
  expect_equal(nrow(panel$pre), 18)
  expect_false(any(panel$people$respondent_id %in% c("1", "2")))
  expect_error(item_panel(dplyr::bind_rows(responses, responses[1, ])))
  result <- validate_item_subsets(responses, 1)
  expect_match(result$samples$status, "insufficient")
  responses$correct <- 1
  result <- validate_item_subsets(responses, 1)
  expect_true(all(is.na(result$results$correlation)))
  expect_true(all(result$results$status == "constant score or validation gain"))
  summary <- summarise_item_validation(result$results)
  expect_true(all(summary$valid_splits == 0))
  expect_true(all(is.na(summary$median)))
})

test_that("fold-specific residualization does not fit held-out responses", {
  data <- validation_fixture()
  p <- validation_partition(data$pre[1:25, ], "easy", 1)
  a <- validation_predictions(data$pre[1:25, ], data$post[1:25, ], data$pre[26:40, ], data$post[26:40, ], p)
  changed_post <- data$post[26:40, ]
  changed_post[1, p$predictor] <- 1 - changed_post[1, p$predictor]
  b <- validation_predictions(data$pre[1:25, ], data$post[1:25, ], data$pre[26:40, ], changed_post, p)
  expect_equal(a$scores$residualized[-1], b$scores$residualized[-1])
  expect_equal(a$validation_gain, b$validation_gain)
})
