# =====================================================
# Happy Path Tests for Main Function ----
# =====================================================

# 1. Happy test

test_that("privacy_evaluation returns numeric value with seven numbers", {
  data_observed <- data.frame(
    x = rnorm(2000),
    y = sample(letters, 1000, replace = TRUE),
    z = rpois(2000, lambda = 3)
  )
  result <- privacy_evaluation(data_observed)
  expect_true(is.numeric(result))
  expect_equal(length(result), 7)

})

test_that("privacy_evaluation strata returns numeric value with seven numbers", {
  data_observed <- data.frame(
    x = rnorm(2000),
    y = sample(c('a', 'b', 'c'), 2000, replace = TRUE),
    z = rpois(2000, lambda = 3)
  )
  result <- privacy_evaluation(data_observed, strata = 'y')
  expect_true(is.numeric(result))
  expect_equal(length(result), 7)

})

# 2. Edge case test

test_that("privacy_evaluation gives a warning when applying the function on
          too small dataset", {
  data_observed <- data.frame(
    var1 = c(1, 2, 3),
    var2 = c(4, 5, 6)
  )
  expect_warning(privacy_evaluation(data_observed), "observations")

})

# =====================================================
# Test: validate_data ----
# =====================================================

### Error Message Tests ----
test_that("validate_data issues a warning for fewer than 1000 rows", {
  data <- data.frame(x = 1:999)
  expect_warning(validate_data(data), "dataframe should contain more than 1000 observations")
})

# =====================================================
# Test: split_data ----
# =====================================================

### Happy Path Tests ----
test_that("split_data correctly splits the dataset", {
  data <- data.frame(x = 1:100)
  splits <- split_data(data, 0.7)
  expect_equal(length(splits$train) + length(splits$holdout), 100)
  expect_true(abs(length(splits$train) / nrow(data) - 0.7) < 0.1)
})

# =====================================================
# Test: synthesize_data ----
# =====================================================

### Happy Path Tests ----
test_that("synthesize_data works for simple synthesis", {
  train_data <- data.frame(x = rnorm(100), y = rnorm(100))
  result <- synthesize_data(train_data)
  expect_type(result, "list")
  expect_true("syn" %in% names(result))
})


# =====================================================
# Test: handle_na ----
# =====================================================

### Happy Path Tests ----

test_that("handle_na replaces NA values with the specified value", {
  data <- data.frame(x = c(1, 2, NA, 4))
  result <- handle_na(data, 0)
  expect_false(any(is.na(result)))
  expect_equal(result$x[3], 0)
})

test_that("handle_na handles a dataset without NA values", {
  data <- data.frame(x = c(1, 2, 3, 4))
  result <- handle_na(data, 0)
  expect_equal(data, result)
})

# =====================================================
# Test: calculate_min_hamming_distance ----
# =====================================================

### Happy Path Tests ----

test_that("calculate_min_hamming_distance computes correct distances", {
  sds_train <- list(syn = data.frame(a = c(1, 2), b = c(3, 4)))
  train_data <- data.frame(a = c(1, 2), b = c(3, 5))
  holdout_data <- data.frame(a = c(2, 3), b = c(4, 5))
  result <- calculate_min_hamming_distance(sds_train, train_data, holdout_data)
  expect_equal(nrow(result), 2)
  expect_true(all(result$min_distance_train >= 0))
  expect_true(all(result$min_distance_holdout >= 0))
})

# =====================================================
# Test: calculate_privacy_metrics ----
# =====================================================

### Happy Path Tests ----
test_that("calculate_privacy_metrics computes correct metrics", {
  DCR <- data.frame(
    min_distance_train = c(1, 2, 3),
    min_distance_holdout = c(2, 3, 1)
  )
  result <- calculate_privacy_metrics(DCR)
  expect_equal(length(result), 7)
  expect_true(all(names(result) %in% c(
    "share_training_data", "n_syn", "n_closer", "n_farther",
    "n_equal", "mean_distance_train", "mean_distance_holdout"
  )))
})



