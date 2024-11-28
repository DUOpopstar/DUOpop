# =====================================================
# Helper Function: check_column_count ----
# =====================================================

#' Check if the number of columns matches between two dataframes
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @return Returns TRUE if the number of columns matches, otherwise FALSE
#' @keywords internal
check_column_count <- function(df1, df2) {
  if (ncol(df1) != ncol(df2)) {
    message("The number of columns does not match between the dataframes.")
    return(FALSE)
  }
  return(TRUE)
}

# =====================================================
# Helper Function: check_column_names ----
# =====================================================

#' Check if the column names match between two dataframes
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @return Returns TRUE if the column names match, otherwise FALSE
#' @keywords internal
check_column_names <- function(df1, df2) {
  if (!identical(names(df1), names(df2))) {
    message("The column names do not match between the dataframes.")
    return(FALSE)
  }
  return(TRUE)
}

# =====================================================
# Helper Function: check_data_types ----
# =====================================================

#' Check if the data types match between two dataframes
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @return Returns TRUE if the data types match, otherwise FALSE
#' @keywords internal
check_data_types <- function(df1, df2) {
  if (!identical(sapply(df1, class), sapply(df2, class))) {
    message("The data types do not match between the dataframes.")
    return(FALSE)
  }
  return(TRUE)
}

# =====================================================
# Helper Function: compare_factor_levels ----
# =====================================================

#' Check if the number of levels matches for factor variables
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @return Returns TRUE if the number of levels matches for factor variables, otherwise FALSE
#' @keywords internal
compare_factor_levels <- function(df1, df2) {
  factor_vars <- intersect(names(df1)[sapply(df1, is.factor)], names(df2)[sapply(df2, is.factor)])
  mismatch <- sapply(factor_vars, function(var) {
    if (length(levels(df1[[var]])) != length(levels(df2[[var]]))) {
      message(paste0("The number of levels does not match for the variable: ", var))
      return(FALSE)
    }
    return(TRUE)
  })
  return(all(mismatch))
}

# =====================================================
# Helper Function: check_factor_level_order ----
# =====================================================

#' Check if the order of levels matches for factor variables
#'
#' @param df1 First dataframe
#' @param df2 Second dataframe
#' @return Returns TRUE if the order of levels matches for factor variables, otherwise FALSE
#' @keywords internal
check_factor_level_order <- function(df1, df2) {
  factor_vars <- intersect(names(df1)[sapply(df1, is.factor)], names(df2)[sapply(df2, is.factor)])
  mismatch <- sapply(factor_vars, function(var) {
    if (!identical(levels(df1[[var]]), levels(df2[[var]]))) {
      message(paste0("The order of levels does not match for the variable: ", var))
      return(FALSE)
    }
    return(TRUE)
  })
  return(all(mismatch))
}

# =====================================================
# Helper Function: sync_data_classes ----
# =====================================================

#' Synchronize column class types between two dataframes
#'
#' Synchronize column classes between two dataframes to match the first.
#'
#' @param data1 A dataframe.
#' @param data2 A dataframe with the same number of columns as `data1`.
#' @return A modified dataframe where columns have the same class types as
#'         their corresponding columns in `data1`.
#' @examples
#' \dontrun{
#' # Voorbeelddataframes met een datumkolom
#'df1 <- data.frame(
#'  a = factor(1:3),
#'  b = 1:3,
#'  c = letters[1:3],
#'  d = as.Date(c("2022-01-01", "2022-01-02", "2022-01-03")))
#'df2 <- data.frame(
#'  a = as.character(1:3),
#'  b = c('1', '2', '3'),
#'  c = LETTERS[4:6],
#'  d = as.character(c("2022-01-01", "2022-01-02", "2022-01-03")))  # Datum als karakterstring
#' df2_adjusted <- sync_data_classes(df1, df2)
#' }
#' @import purrr
#' @keywords internal
sync_data_classes <- function(data1, data2) {
  # Get the column names to use in the message
  column_names <- names(data1)

  # Map columns from data2 to those from data1 without modifying names
  data2_sync <- purrr::map2(data1, data2, ~{
    target_class <- class(.x)
    original_class <- class(.y)

    # Check if the class of both columns matches
    if (original_class != target_class) {
      # Print message with the column name, original class, and new class
      column_name <- names(data1)[which(sapply(data1, identical, .x))]
      message(sprintf("Converting column '%s' from %s to %s",
                      column_name, original_class, target_class))

      # Convert the column to the correct class
      .y <- switch(target_class,
                   "factor" = as.factor(.y),
                   "integer" = as.integer(.y),
                   "numeric" = as.numeric(.y),
                   "character" = as.character(.y),
                   "Date" = as.Date(.y, origin = "1970-01-01"),
                   .y) # Return original if class not supported
    }
    # Return the modified column
    .y
  })

  # Convert the list back to a dataframe with original column names
  data2_sync <- as.data.frame(data2_sync)
  names(data2_sync) <- column_names

  # Return the modified dataframe
  return(data2_sync)
}

# =====================================================
# Helper Function: adjust_factor_levels_order ----
# =====================================================

#' Adjust factor levels in observed data to match synthetic data
#'
#' @param observed_data Dataframe needing factor level adjustment
#' @param synthetic_data Reference dataframe for factor levels
#' @return Dataframe with adjusted factor levels and prints adjustments made.
#' @examples
#' \dontrun{
#' df1 <- data.frame(a = factor(c("A", "B", "C", "A"), levels = c("A", "B", "C")), b = 1:4)
#' df3 <- data.frame(a = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")), b = 1:4)
#' adjust_factor_levels_order(df1, df3)
#' }
#' @keywords internal
adjust_factor_levels_order <- function(observed_data, synthetic_data) {
  # Factor vars in observed and synthetic data
  factor_obs <- purrr::map_lgl(observed_data, is.factor) %>%
    which() %>%
    names()

  factor_syn <- purrr::map_lgl(synthetic_data, is.factor) %>%
    which() %>%
    names()

  # Common factors between dataframes
  common_factors <- intersect(factor_obs, factor_syn)

  # No common factors error
  if (length(common_factors) == 0) {
    stop("No common factors between observed and synthetic data.")
  }

  # Loop through common factors
  for (var in common_factors) {
    # Check if levels are identical in content and number

    if (setequal(levels(observed_data[[var]]), levels(synthetic_data[[var]])) &&
        length(levels(observed_data[[var]])) == length(levels(synthetic_data[[var]]))) {
      if (!identical(levels(observed_data[[var]]), levels(synthetic_data[[var]]))) {
        # Print a message about the adjustment
        cat(sprintf(
          "Adjusting factor levels for variable '%s':\n- observed: %s\n- synthetic: %s\n",
          var,
          paste(levels(observed_data[[var]]), collapse = ", "),
          paste(levels(synthetic_data[[var]]), collapse = ", ")
        ))
        # Adjust levels in synthetic_data
        synthetic_data[[var]] <- factor(synthetic_data[[var]],
                                       levels = levels(observed_data[[var]]))
      }
    } else {
      # Print a message about unmatched levels
      message(sprintf(
        "Skipping adjustment for variable '%s': Number or content of levels do not match.\n- Observed: %s\n- Synthetic: %s\n",
        var,
        paste(levels(observed_data[[var]]), collapse = ", "),
        paste(levels(synthetic_data[[var]]), collapse = ", ")
      ))
    }
  }



  return(synthetic_data)
}
