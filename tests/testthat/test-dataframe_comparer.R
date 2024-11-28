library(testthat)

## 1. Happy Path: Test met geldige standaardinvoer

# Test for basic functionality with a single data frame
test_that("dataframe_comparer works with a single data frame", {
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c("x", "y", "", "y", "z"))
  result <- dataframe_comparer(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), ncol(df))
  expect_true(all(result$variable_names == names(df)))
  expect_true(all(result$type == c("numeric", "character")))
  expect_equal(unname(result$count_NA), c(1, 0))
  expect_equal(unname(result$count_empty_values), c(0, 1))
})

# Test for identical data frames
test_that("dataframe_comparer works with identical data frames", {
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c("x", "y", "", "y", "z"))
  result <- dataframe_comparer(df, df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), ncol(df))
  expect_true(all(result$variable_name == names(df)))
})

# Test for agreement between real_data_type and syn_data_type
test_that("dataframe_comparer finds identical data types in identical data frames", {
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c("x", "y", "", "y", "z"))
  result <- dataframe_comparer(df, df)

  expect_true(all(result$real_data_type == result$syn_data_type))
})

# Test for agreement on non-factor level agreement
test_that("dataframe_comparer correctly labels level agreement for non-factor columns", {
  df <- data.frame(a = c(1, 2, NA, 4, 5), b = c("x", "y", "", "y", "z"))
  result <- dataframe_comparer(df, df)

  expect_equal(unname(result$level_agreement), c("Non-Factor", "Non-Factor"))
})

## 2. Edge Cases: Test met grensgevallen en ongebruikelijke invoer

# Test for handling different data frames with matching variable names
test_that("dataframe_comparer works with different data frames but same variable names", {
  df1 <- data.frame(
    a = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
    b = factor(c("red", "green", "blue"), levels = c("red", "green", "blue"))
  )

  df2 <- data.frame(
    a = factor(c("low", "medium", "high"), levels = c("high", "medium", "low")),
    b = factor(c("red", "green", "blue"), levels = c("blue", "green", "red"))
  )

  result <- dataframe_comparer(df1, df2)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), ncol(df1))
  expect_true(all(result$variable_name == names(df1)))
})

# Test for data type agreement in different data frames
test_that("dataframe_comparer finds matching data types in different data frames", {
  df1 <- data.frame(
    a = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
    b = factor(c("red", "green", "blue"), levels = c("red", "green", "blue"))
  )

  df2 <- data.frame(
    a = factor(c("low", "medium", "high"), levels = c("high", "medium", "low")),
    b = factor(c("red", "green", "blue"), levels = c("blue", "green", "red"))
  )

  result <- dataframe_comparer(df1, df2)

  expect_true(all(result$real_data_type == result$syn_data_type))
})

# Test for level mismatch in factor columns
test_that("dataframe_comparer detects level order mismatch", {
  df1 <- data.frame(
    a = factor(c("low", "medium", "high"), levels = c("low", "medium", "high")),
    b = factor(c("red", "green", "blue"), levels = c("red", "green", "blue"))
  )

  df2 <- data.frame(
    a = factor(c("low", "medium", "high"), levels = c("high", "medium", "low")),
    b = factor(c("red", "green", "blue"), levels = c("blue", "green", "red"))
  )

  result <- dataframe_comparer(df1, df2)

  expect_true(result$level_agreement[2] == "Level Match, Order Mismatch")
})

# Test for empty data frames
test_that("dataframe_comparer handles empty data frames", {
  df <- data.frame()
  result <- dataframe_comparer(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)  # No columns to compare
})

# Test for single column data frame
test_that("dataframe_comparer handles single column data frames", {
  df <- data.frame(a = 1:5)
  result <- dataframe_comparer(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_true(result$variable_names == "a")
})

## 3. Error Handling: Test foutafhandeling bij ongeldige invoer + 4. Error Messages: Controleer op duidelijke foutmeldingen

# Test for error when non-data frame is provided
test_that("dataframe_comparer errors on non-data frame input", {
  expect_error(dataframe_comparer(123), "real_data must be a data frame")
  expect_error(dataframe_comparer(data.frame(a = 1), 123), "syn_data must be a data frame")
})

# Test for error when column names don't match
test_that("dataframe_comparer errors when column names do not match", {
  df1 <- data.frame(a = 1:5, b = 6:10)
  df2 <- data.frame(x = 1:5, y = 6:10)

  expect_error(dataframe_comparer(df1, df2), "The column names of the two data frames do not match")
})
