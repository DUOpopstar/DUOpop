#' @title Validate and Align Two Data Frames
#'
#' @description
#' Validates and aligns two data frames by checking various properties.
#'
#' @param observed_data The observed data frame.
#' @param synthetic_data The synthetic data frame.
#' @return
#' An adjusted version of \code{synthetic_data}.
#'
#' @examples
#' \dontrun{
#' observed_data <- data.frame(
#'   a = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
#'   b = 1:3
#' )
#'
#' synthetic_data <- data.frame(
#'   a = factor(c("A", "B", "C"), levels = c("A", "C", "B")),
#'   b = c("3", "1", "2")
#' )
#'
#' synthetic_data_adjusted <- validate_and_align_dataframes(observed_data, synthetic_data)
#' }
#'
#' @export
validate_and_align_dataframes <- function(observed_data, synthetic_data) {

  # Check if the number of columns matches between the two dataframes
  check_column_count(observed_data, synthetic_data)
  # Check if the column names match between the two dataframes
  check_column_names(observed_data, synthetic_data)
  # Check if the data types match between the two dataframes
  check_data_types(observed_data, synthetic_data)
  # Check if the number of levels matches for factor variables
  compare_factor_levels(observed_data, synthetic_data)
  # Check if the order of levels matches for factor variables
  check_factor_level_order(observed_data, synthetic_data)
  # Synchronize column class types between the two dataframes
  synthetic_data_sync <- sync_data_classes(observed_data, synthetic_data)
  # Adjust factor levels in synthetic data to match observed data
  synthetic_data_adjusted <- adjust_factor_levels_order(observed_data, synthetic_data_sync)

  # Return the adjusted synthetic_data dataframe
  return(synthetic_data_adjusted)

}
