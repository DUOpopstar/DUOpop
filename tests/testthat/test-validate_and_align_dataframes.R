# =====================================================
# Happy Path Tests for Main Function validate_and_align_dataframes
# =====================================================

test_that("validate_and_align_dataframes correctly adjusts synthetic data to match observed data", {
  # Prepare observed data
  observed_data <- data.frame(
    a = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    b = 1:3
  )

  # Prepare synthetic data with differences that should be corrected
  synthetic_data <- data.frame(
    a = factor(c("A", "B", "C"), levels = c("A", "B", "C")),  # Different level order
    b = c("1", "2", "3")  # b is of type character instead of integer
  )

  # Run the function to adjust synthetic data
  synthetic_data_adjusted <- validate_and_align_dataframes(observed_data, synthetic_data)

  # Expected adjusted synthetic data
  expected_synthetic_data <- data.frame(
    a = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    b = 1:3
  )

  # Ensure the data types match
  expected_synthetic_data$b <- as.integer(expected_synthetic_data$b)

  # Check that the adjusted synthetic data matches the expected data
  expect_equal(synthetic_data_adjusted, expected_synthetic_data)
})

# =====================================================
# Tests for Helper Functions ----
# =====================================================

## Helper Function 1: check_column_count ----
### Happy Path Tests ----

test_that("check_column_count works correctly", {
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(a = 7:9, b = 10:12)
  df3 <- data.frame(a = 1:3)
  expect_true(check_column_count(df1, df2))
  expect_false(check_column_count(df1, df3))
})

### Error Message Tests ----

test_that("check_column_count returns FALSE and correct message when column counts do not match", {
  # Create two data frames with different numbers of columns
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(a = 1:3, b = 4:6, c = 7:9)

  # Capture the message output and check the return value
  expect_message(
    result <- check_column_count(df1, df2),
    "The number of columns does not match between the dataframes."
  )
  expect_false(result)
})




## Helper Function 2: check_column_names ----
### Happy Path Tests ----

test_that("check_column_names works correctly", {
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(a = 7:9, b = 10:12)
  df3 <- data.frame(c = 1:3, d = 4:6)
  expect_true(check_column_names(df1, df2))
  expect_false(check_column_names(df1, df3))
})

### Error Message Tests ----

test_that("check_column_names returns FALSE and correct message when column names do not match", {
  # Create two data frames with different column names
  df1 <- data.frame(a = 1:3, b = 4:6)
  df2 <- data.frame(x = 1:3, y = 4:6)

  # Capture the message output and check the return value
  expect_message(
    result <- check_column_names(df1, df2),
    "The column names do not match between the dataframes."
  )
  expect_false(result)
})


## Helper Function 3: check_data_types ----
### Happy Path Tests ----

test_that("check_data_types works correctly", {
  df1 <- data.frame(a = 1:3, b = as.character(4:6))
  df2 <- data.frame(a = 7:9, b = as.character(10:12))
  df3 <- data.frame(a = 1:3, b = 4:6)
  expect_true(check_data_types(df1, df2))
  expect_false(check_data_types(df1, df3))
})

### Error Message Tests ----

test_that("check_data_types returns FALSE and correct message when data types do not match", {
  # Create two data frames with different data types
  df1 <- data.frame(
    a = 1:3,                      # integer
    b = c("x", "y", "z"),         # character
    c = factor(c("A", "B", "C"))  # factor
  )

  df2 <- data.frame(
    a = as.numeric(4:6),          # numeric
    b = c(TRUE, FALSE, TRUE),     # logical
    c = factor(c("D", "E", "F"))  # factor
  )

  # Capture the message output and check the return value
  expect_message(
    result <- check_data_types(df1, df2),
    "The data types do not match between the dataframes."
  )
  expect_false(result)
})



## Helper Function 4: compare_factor_levels ----
### Happy Path Tests ----

test_that("check_factor_levels works correctly", {
  df1 <- data.frame(a = factor(c("A", "B", "C")), b = 1:3)
  df2 <- data.frame(a = factor(c("A", "B", "C")), b = 4:6)
  df3 <- data.frame(a = factor(c("A", "B")), b = 7:8)
  expect_true(compare_factor_levels(df1, df2))
  expect_false(compare_factor_levels(df1, df3))
})

### Error Message Tests ----

test_that("compare_factor_levels returns FALSE and correct message when factor levels do not match", {
  # Create two data frames with differing factor levels
  df1 <- data.frame(
    var1 = factor(c("A", "B", "C")),
    var2 = factor(c("X", "Y", "Z"))
  )

  df2 <- data.frame(
    var1 = factor(c("A", "B")),           # Missing level "C"
    var2 = factor(c("X", "Y", "Z", "W"))  # Extra level "W"
  )

  # Capture the messages and check the return value
  expect_message(
    result <- compare_factor_levels(df1, df2),
    "The number of levels does not match for the variable: var1"
  )
})


# Add additional helper functions following the same pattern











test_that("check_factor_level_order works correctly", {
  df1 <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 1:3)
  df2 <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)
  df3 <- data.frame(a = factor(c("A", "B", "C"), levels = c("C", "B", "A")), b = 7:9)
  expect_true(check_factor_level_order(df1, df2))
  expect_false(check_factor_level_order(df1, df3))
})


test_that("adjust_factor_levels_order adjusts identical levels with different order", {
  observed_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("C", "B", "A")), b = 1:3)
  synthetic_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)

  adjusted_syn_data <- adjust_factor_levels_order(observed_data, synthetic_data)
  expect_equal(levels(observed_data$a), levels(adjusted_syn_data$a))
})

test_that("adjust_factor_levels_order warns when levels are not identical in content", {
  observed_data <- data.frame(a = factor(c("A", "B")), b = 7:8)
  synthetic_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)

  expect_message(adjust_factor_levels_order(observed_data, synthetic_data),
                 regexp = "Skipping adjustment for variable")
})

test_that("adjust_factor_levels_order errors when there are no factor levels to match", {
  observed_data <- data.frame(b = 7:8)
  synthetic_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)

  expect_error(adjust_factor_levels_order(observed_data, synthetic_data),
               "No common factors between observed and synthetic data.")
})










## Helper Function 5: check_factor_level_order ----
### Happy Path Tests ----

test_that("check_factor_level_order works correctly", {
  df1 <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 1:3)
  df2 <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)
  df3 <- data.frame(a = factor(c("A", "B", "C"), levels = c("C", "B", "A")), b = 7:9)
  expect_true(check_factor_level_order(df1, df2))
  expect_false(check_factor_level_order(df1, df3))
})


### Error Message Tests ----

test_that("check_factor_level_order returns FALSE and correct message when factor level orders do not match", {
  # Create two data frames with the same factor levels but in different orders
  df1 <- data.frame(
    var1 = factor(c("A", "B", "C"), levels = c("A", "B", "C"))
  )

  df2 <- data.frame(
    var1 = factor(c("A", "B", "C"), levels = c("C", "B", "A"))
  )

  # Capture the message output and check the return value
  expect_message(
    result <- check_factor_level_order(df1, df2),
    "The order of levels does not match for the variable: var1"
  )
  expect_false(result)
})




## Helper Function 6: adjust_factor_levels_order ----
### Happy Path Tests ----

test_that("adjust_factor_levels_order adjusts identical levels with different order", {
  observed_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("C", "B", "A")), b = 1:3)
  synthetic_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)

  adjusted_syn_data <- adjust_factor_levels_order(observed_data, synthetic_data)
  expect_equal(levels(observed_data$a), levels(adjusted_syn_data$a))
})


### Error Message Tests ----
test_that("adjust_factor_levels_order warns when levels are not identical in content", {
  observed_data <- data.frame(a = factor(c("A", "B")), b = 7:8)
  synthetic_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)

  expect_message(adjust_factor_levels_order(observed_data, synthetic_data),
                 regexp = "Skipping adjustment for variable")
})

test_that("adjust_factor_levels_order errors when there are no factor levels to match", {
  observed_data <- data.frame(b = 7:8)
  synthetic_data <- data.frame(a = factor(c("A", "B", "C"), levels = c("A", "B", "C")), b = 4:6)

  expect_error(adjust_factor_levels_order(observed_data, synthetic_data),
               "No common factors between observed and synthetic data.")
})


## Helper Function 7: sync_data_classes ----
### Happy Path Tests ----

test_that("sync_data_classes correctly synchronizes data types between data frames", {
  # Create data1 with various data types
  data1 <- data.frame(
    a = factor(c("A", "B", "C")),                    # factor
    b = 1:3,                                         # integer
    c = c("x", "y", "z"),                            # character
    d = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03"))  # Date
  )

  # Create data2 with different data types that can be converted
  data2 <- data.frame(
    a = c("A", "B", "C"),                            # character, to be converted to factor
    b = c("1", "2", "3"),                            # character, to be converted to integer
    c = c("u", "v", "w"),                            # character, same as data1
    d = as.character(c("2022-01-01", "2022-01-02", "2022-01-03"))  # character, to be converted to Date
  )

  # Apply the function
  data2_adjusted <- sync_data_classes(data1, data2)

  # Check that the classes of data2_adjusted match those of data1
  expect_equal(sapply(data2_adjusted, class), sapply(data1, class))

  # Check that the values have been correctly converted
  expect_equal(data2_adjusted$a, data1$a)
  expect_equal(data2_adjusted$b, data1$b)
  expect_equal(data2_adjusted$c, data2$c)  # 'c' was already character
  expect_equal(data2_adjusted$d, data1$d)
})

### Error Message Tests ----

test_that("sync_data_classes handles conversion errors and produces warnings when conversion fails", {
  # Create data1 with various data types
  data1 <- data.frame(
    a = factor(c("A", "B", "C")),                    # factor
    b = 1:3                                         # integer
  )

  # Create data2 with incompatible data types
  data2 <- data.frame(
    a = c("A", "B", "C"),
    b = c("1", "2", "3")
  )

  # Apply the function and capture warnings

    data2_adjusted <- sync_data_classes(data1, data2)


  # Check that the function outputs messages about conversions
  expect_message(
    data2_adjusted <- sync_data_classes(data1, data2),
    "Converting column 'a' from character to factor"
  )
  expect_message(
    data2_adjusted <- sync_data_classes(data1, data2),
    "Converting column 'b' from character to integer"
  )

})

















