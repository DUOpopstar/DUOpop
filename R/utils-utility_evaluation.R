# =====================================================
# Helper Function 1: check_df_data_observed ----
# =====================================================

#' @title Check if data_observed is a data frame
#'
#' @param data_observed The observed data from which the synthesized data was created, with class dataframe
#' @return An error if data_observed is not a data frame.
#' @keywords internal
check_df_data_observed <- function(data_observed){
  if (!is.data.frame(data_observed)) {
    stop("`data_observed` should be a data frame.
       Change the class from data_observed to a data frame", call. = FALSE)
  }
}
# =====================================================
# Helper Function 2: check_columns ----
# =====================================================

#' @title Check if columns are equal (number of columns, column names)
#'
#' @param syn_object The synthetic data object as synthesized by using synthpop, with class synds
#' @param data_observed The observed data from which the synthesized data was created, with class dataframe
#' @return An error if the number of columns or column names are not similar between syn_object and data_observed.
#' @keywords internal
check_columns <- function(syn_object, data_observed){
  if(ncol(syn_object$syn) != ncol(data_observed)) {
    stop("The number of columns are not similar between syn_object$syn and data_observed.
    The number of columns needs to be equal to run utility_evaluation()")
  } else if(!setequal(names(syn_object$syn), names(data_observed))) {
    stop("The column names of the two data frames are not similar. The column names
         need to be equal to run utility_evaluation()")
  }
}

# =====================================================
# Helper Function 3: check_synds_syn_object ----
# =====================================================

#' @title Check if syn_object is a synds object
#'
#' @param syn_object The synthetic data object as synthesized by using synthpop, with class synds
#' @return An error if syn_object is not of class synds.
#' @keywords internal
check_synds_syn_object <- function(syn_object){
  if (!inherits(syn_object, "synds")) {
    stop("`syn_object` should be a synds object.
         Change the class of the syn_object to synds", call. = FALSE)
  }
}

# =====================================================
# Helper Function 4: check_factor_level_count ----
# =====================================================

#' Check Consistency of Factor Level Counts Between Datasets
#'
#' Compares the number of levels for all factor variables between an observed dataset and a synthetic dataset contained within a `syn_object`.
#' If discrepancies are found, it throws an error listing the variables with inconsistent levels.
#'
#' @param syn_object A list containing a synthetic dataset under the element `syn`.
#' @param data_observed A data frame containing the original observed data.
#' @return Returns `NULL` invisibly if all factor levels are consistent; otherwise, it stops with an error message.
#' @examples
#' \dontrun{
#' # Create example observed data
#' data_observed <- data.frame(
#'   var1 = factor(c("A", "B", "C")),
#'   var2 = factor(c("D", "E", "F"))
#' )
#'
#' # Create a synthetic dataset with matching factor levels
#' syn_object <- list(
#'   syn = data.frame(
#'     var1 = factor(c("A", "B", "C")),
#'     var2 = factor(c("D", "E", "F"))
#'   )
#' )
#'
#' # This should pass without errors
#' check_factor_level_count(syn_object, data_observed)
#'
#' # Modify the synthetic dataset to have inconsistent factor levels
#' syn_object$syn$var2 <- factor(c("D", "E", "E"))
#'
#' # This will throw an error
#' check_factor_level_count(syn_object, data_observed)
#' }
#' @keywords internal
check_factor_level_count <- function(syn_object, data_observed) {
  # Identify all factor variables in data_observed
  factor_columns <- names(data_observed)[sapply(data_observed, is.factor)]

  # Get the number of levels for each factor variable in both datasets
  num_levels_observed <- sapply(data_observed[factor_columns], nlevels)
  num_levels_synthetic <- sapply(syn_object$syn[factor_columns], nlevels)

  # Check if the number of levels matches for each factor variable
  levels_match <- num_levels_observed == num_levels_synthetic

  # Generate an error message if there are discrepancies
  all(levels_match) || stop(
    paste(
      "The number of factor levels of variable(s)",
      paste(factor_columns[!levels_match], collapse = ", "),
      "is not equal between the two datasets"
    )
  )

  # Return invisibly if all checks pass
  invisible(NULL)
}

# =====================================================
# Helper Function 5: synchronize_na_levels ----
# =====================================================

### Helper functies voor utility_evaluation.R
#' Synchronize NA Levels and Handle Missing Values in Datasets
#'
#' This function synchronizes factor levels between `observed_data` and `synthetic_data`,
#' replaces NA values with -999 in numeric variables of `synthetic_data`,
#' and converts logical variables to factors under specific conditions.
#'
#' @param observed_data A data frame containing the observed data.
#' @param synthetic_data A data frame containing the synthetic data.
#'
#' @return A list containing the modified `observed_data` and `synthetic_data`.
#' \describe{
#'   \item{observed_data}{The modified observed data frame.}
#'   \item{synthetic_data}{The modified synthetic data frame.}
#' }
#'
#' @details The function performs the following operations:
#' \enumerate{
#'   \item Synchronizes factor levels for factor or character variables that contain NA values.
#'   \item Replaces NA values with -999 in numeric variables of `synthetic_data` that contain NA values.
#'   \item Converts logical variables to factors in both datasets if the variable is logical in
#'         `observed_data`, contains NA in `synthetic_data`, and does not contain NA in `observed_data`.
#' }
#' @keywords internal
synchronize_na_levels <- function(observed_data, synthetic_data) {
  # Identify common columns between the two datasets
  common_cols <- intersect(names(observed_data), names(synthetic_data))

  # Part 1: Synchronize factor levels for factor or character variables with NAs
  # Check if columns are factor or character in synthetic_data
  is_factor_or_char <- sapply(synthetic_data[common_cols], function(col) is.factor(col) || is.character(col))

  # Check if there are NAs in either dataset for the columns
  has_na_in_either <- sapply(common_cols, function(col) {
    any(is.na(observed_data[[col]])) || any(is.na(synthetic_data[[col]]))
  })

  # Select columns to adjust
  cols_to_adjust <- common_cols[is_factor_or_char & has_na_in_either]

  # Apply addNA(as.factor()) to selected columns in both datasets
  observed_data[cols_to_adjust] <- lapply(observed_data[cols_to_adjust], function(col) addNA(as.factor(col)))
  synthetic_data[cols_to_adjust] <- lapply(synthetic_data[cols_to_adjust], function(col) addNA(as.factor(col)))

  # Part 2: Replace NAs with -999 in numeric variables of synthetic_data
  # Check if columns are numeric in synthetic_data
  is_numeric <- sapply(synthetic_data[common_cols], is.numeric)

  # Check if there are NAs in synthetic_data for the numeric columns
  has_na_in_synthetic <- sapply(synthetic_data[common_cols], function(col) any(is.na(col)))

  # Select numeric columns to adjust
  numeric_cols_to_adjust <- common_cols[is_numeric & has_na_in_synthetic]

  # Replace NAs with -999 in synthetic_data
  synthetic_data[numeric_cols_to_adjust] <- lapply(synthetic_data[numeric_cols_to_adjust], function(col) {
    col[is.na(col)] <- -999
    return(col)
  })

  # Part 3: Convert logical variables to factors under specific conditions
  # Check if columns are logical in observed_data
  is_logical_in_observed <- sapply(observed_data[common_cols], is.logical)

  # Check if columns have NAs in synthetic_data but not in observed_data
  has_na_in_synthetic_but_not_in_observed <- sapply(common_cols, function(col) {
    any(is.na(synthetic_data[[col]])) && !any(is.na(observed_data[[col]]))
  })

  # Select columns to convert
  cols_to_convert <- common_cols[is_logical_in_observed & has_na_in_synthetic_but_not_in_observed]

  for (col in cols_to_convert) {
    # Convert to factor in both datasets
    observed_data[[col]] <- as.factor(observed_data[[col]])
    synthetic_data[[col]] <- as.factor(synthetic_data[[col]])

    # Ensure both datasets have the same levels, including NA
    combined_levels <- unique(c(levels(observed_data[[col]]), levels(synthetic_data[[col]]), NA))

    observed_data[[col]] <- factor(observed_data[[col]], levels = combined_levels, exclude = NULL)
    synthetic_data[[col]] <- factor(synthetic_data[[col]], levels = combined_levels, exclude = NULL)
  }

  # Return the modified datasets as a list
  list(observed_data = observed_data, synthetic_data = synthetic_data)
}

# =====================================================
# Helper Function 6: run_utility_tests ----
# =====================================================

#' @title Run utility tests
#' @param syn_object The synthetic data object as synthesized by using synthpop, with class synds
#' @param data_observed The observed data from which the synthesized data was created, with class dataframe
#' @param utility_stat Parameter in the function compare.synds {synthpop}
#' @param ngroups Parameter in the function utility.tables {synthpop}
#' @return A list containing 1- and 2-dimensional utility results.
#' @keywords internal
run_utility_tests <- function(syn_object, data_observed, utility_stat = "S_pMSE", ngroups = 25) {
  utility_data <- list(
    "1-dim" = compare.synds(syn_object, data_observed, utility.stats = utility_stat),
    "2-dim" = utility.tables(syn_object, data_observed, ngroups = ngroups)
  )
  return(utility_data)
}

# =====================================================
# Helper Function 7: plot_utility_data ----
# =====================================================

#' @title Create a utility plot
#'
#' @param utility_data List resulting from running the function run_utility_tests()
#' @return A utility plot.
#' @keywords internal
plot_utility_data <- function(utility_data, colors = c("darkgreen", "lightgreen", "yellow", "orange", "red")) {
  plot <- utility_data[["2-dim"]][["utility.plot"]] +
    scale_fill_stepsn(
      colors = colors,
      breaks = c(3, 10, 30, 100),
      trans = "pseudo_log",
      limits = c(0, 300)
    )
  return(plot)
}




# =====================================================
