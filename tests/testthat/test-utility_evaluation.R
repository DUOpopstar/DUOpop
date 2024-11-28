# =====================================================
# Happy Path Tests for Main Function utility_evaluation ----
# =====================================================

### Happy Path Tests ----

# Correct input: Verifies that the function works correctly with a valid
# synthetic dataset (synds) and an observed dataset, and returns the correct
# 1- and 2-dimensional utility visualizations.

test_that("utility_evaluation() works with correct input and returns 1-dim result", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- data.frame(a = 1:3, b = 4:6)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)
  utility_data <- utility_evaluation(syn_object, data_observed)
  dim1 <- utility_data$`1-dim`

  #check if the 1-dim utility metrics exist
  expect_true(exists("dim1"))

})

test_that("utility_evaluation aligns factor levels with autofix = TRUE", {
  # Create observed data
  observed_data <- data.frame(
    a = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    b = 1:3
  )
  # Create synthetic data with mismatched factor levels
  synthetic_data <- data.frame(
    a = factor(c("A", "B", "C"), levels = c("A", "C", "B")),
    b = c("3", "1", "2")
  )
  # Create synthetic data object
  syn_object <- structure(list(syn = synthetic_data, m = 1), class = "synds")
  # Run utility_evaluation with autofix = TRUE
  result <- utility_evaluation(syn_object, observed_data, autofix = TRUE)
  # Check that the result is a list
  expect_type(result, "list")
  # Check that the plot is included in the result
  expect_true("plot" %in% names(result))
  # Check that the aligned factor levels are identical
  expect_identical(levels(result$data_observed$a), levels(result$syn_object$syn$a))
})

test_that("utility_evaluation() works with correct input and returns 2-dim result", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- data.frame(a = 1:3, b = 4:6)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)
  utility_data <- utility_evaluation(syn_object, data_observed)
  dim2 <- utility_data$`2-dim`

  #check if the 2-dim utility metrics exist
  expect_true(exists("dim2"))

})

test_that("utility_evaluation() works with correct input and returns plot", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- data.frame(a = 1:3, b = 4:6)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)
  utility_data <- utility_evaluation(syn_object, data_observed)
  utility_plot <- utility_data$`plot`

  #check if the utility plot exists
  expect_true(exists("utility_plot"))

})

### Error Message Tests ----


test_that("utility_evaluation() error when data_observed is not a data frame", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- list(a = 1:3, b = 4:6)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)
  expect_error(utility_evaluation(syn_object, data_observed),
               regexp = "should be a data frame")
})


# Invalid syn_object: Tests whether the function throws an error if the
# synthetic object is not of type 'synds'.

test_that("utility_evaluation() error when syn_object is not of type 'synds'", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  data_observed <- data.frame(a = 1:3, b = 4:6)
  class(data_observed) <- "data.frame"
  expect_error(utility_evaluation(syn_object, data_observed),
               regexp = "should be a synds object")
})

# Unequal number of columns: Verifies whether the function throws an error if
# the number of columns in the synthetic and observed datasets does not match.

test_that("utility_evaluation() error when unequal number of columns", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- data.frame(a = 1:3, b = 4:6, c = 2:4)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)
  data_observed$c <- as.factor(data_observed$c)

  expect_error(utility_evaluation(syn_object, data_observed),
               regexp = "number of columns are not similar")
})

# Unequal column names: Verifies whether the function throws an error if
# the column names in the synthetic and observed datasets do not match.

test_that("utility_evaluation() error when unequal column names", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- data.frame(a = 1:3, x = 4:6)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$x <- as.factor(data_observed$x)

  expect_error(utility_evaluation(syn_object, data_observed),
               regexp = "column names of the two data frames are not similar")
})

# Unequal factor levels: Checks whether the function throws an error if the
# number of factor levels between the synthetic and observed datasets does not match.

test_that("utility_evaluation() error when unequal number of factor levels", {
  syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
  syn_object$syn$a <- as.factor(syn_object$syn$a)
  syn_object$syn$b <- as.factor(syn_object$syn$b)
  class(syn_object) <- "synds"
  data_observed <- data.frame(a = 1:3, b = c(4,5,5))
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)

  expect_error(utility_evaluation(syn_object, data_observed),
               regexp = "number of factor levels of variable")
})

# =====================================================
# Tests for Helper Functions ----
# =====================================================

## Helper Function 1: check_df_data_observed ----

### Happy Path Tests ----


### Error Message Tests ----

# Invalid data_observed: Tests whether the function throws an error if
# data_observed is not of type data.frame.

test_that("check_df_data_observed() error when data_observed is not of type data frame", {
  data_observed <- list(a = 1:3, b = 4:6)
  data_observed$a <- as.factor(data_observed$a)
  data_observed$b <- as.factor(data_observed$b)
  expect_error(check_df_data_observed(data_observed),
               regexp = "should be a data frame")
})

## Helper Function 2:  check_columns ----

### Happy Path Tests ----

### Error Message Tests ----



## Helper Function 3:  check_synds_syn_object ----

### Happy Path Tests ----

### Error Message Tests ----


## Helper Function 4:  check_factor_level_count ----

### Happy Path Tests ----

### Error Message Tests ----


## Helper Function 5:  synchronize_na_levels ----

### Happy Path Tests ----

### Error Message Tests ----


## Helper Function 6:  run_utility_tests ----

### Happy Path Tests ----

### Error Message Tests ----


## Helper Function 7:  plot_utility_data ----

### Happy Path Tests ----

### Error Message Tests ----





