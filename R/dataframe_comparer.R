# dataframe_comparer function in another file

#' @title Compare Data Frames
#'
#' @description This function compares two data frames in terms of column types, count of NA values,
#' count of empty values, and number of unique values. If only one data frame is provided,
#' it returns these metrics for that data frame alone.
#'
#' @param real_data A data frame to be analyzed.
#' @param syn_data An optional second data frame for comparison with `real_data`.
#' If not provided, the function only analyzes `real_data`.
#' @return A data frame summarizing the comparison, including variable types, counts of NA values,
#' empty values, unique values, and basic statistics (min, median, mean, max) for numeric columns.
#' If `syn_data` is provided, the comparison will include both data frames.
#' @export
#' @examples
#' # Analyzing a single data frame:
#' real_data <- data.frame(a = factor(c("low", "medium", "high")),
#' b = factor(c("red", "green", "blue"), levels = c("red", "green", "blue")),
#' c = factor(c("low", "medium", "high")),
#' d = factor(c("yes", "no", "yes")),
#' e = c(1.0, 2.5, 3.1),  # Numeric variable
#' f = c("apple", "banana", "cherry"),  # String variable
#' g = c(NA, NA, 5),  # Column with many NAs
#' h = c("", "", "filled")  # Column with many empty values
#' )
#' dataframe_comparer(real_data)
#'
#' # Comparing two data frames:
#' syn_data <- data.frame(
#' a = factor(c("low", "medium", "high")),  # Exact Match
#' b = factor(c("blue", "red", "green"),
#' levels = c("blue", "red", "green")),  # Level Match, Order Mismatch
#' c = factor(c("low", "medium", "very high")),  # Level Mismatch
#' d = c("yes", "no", "yes"),  # Non-Factor
#' e = c(1.0, 2.7, 3.0),  # Numeric variable with slight differences
#' f = c("apple", "banana", "citrus"),  # String variable with a difference
#' g = c(NA, 10, NA),  # Column with different NAs
#' h = c("", "empty", "")  # Column with different empty values
#' )
#' dataframe_comparer(real_data , syn_data)
dataframe_comparer <- function(real_data, syn_data = NULL) {
  # Ensure inputs are data frames.
  validate_dataframe_comparer_inputs(real_data, syn_data)

  # Analyze one data frame or prepare both for comparison;
  # validate column names if `syn_data` is provided
  comparison_result <- compare_dataframes(real_data, syn_data)

  if (!is.null(comparison_result)) {
    return(comparison_result)  # Return single data frame analysis
  }

  # Compare factor levels between `real_data` and `syn_data`
  level_agreement <- calculate_level_agreement(real_data, syn_data)

  # Compute statistics for both data frames
  real_data_stats <- calculate_stats(real_data)
  syn_data_stats <- calculate_stats(syn_data)


  # Combines stats from `real_data` and `syn_data` into a single tibble, side by side.
  comparison_result <- combine_comparison_results(real_data_stats, syn_data_stats, level_agreement)

  return(comparison_result)  # Return final comparison result
}
