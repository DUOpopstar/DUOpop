#' Calculate Basic Statistics for a Data Frame
#'
#' This helper function calculates various statistics for a given data frame,
#' including variable types, counts of unique values, NA values, empty values,
#' and basic statistics (min, median, mean, max) for numeric columns.
#'
#' @param df A data frame to analyze.
#' @return A tibble summarizing the statistics for each variable in the data frame.
#' @keywords internal
#' @importFrom purrr map map_dbl
#' @importFrom stats median na.omit
calculate_stats <- function(df) {
  tibble::tibble(
    variable_names = names(df),
    type = sapply(df, class),
    count_unique_values =  sapply(df, function(x) length(unique(x[!is.na(x) & x != ""]))),
    unique_values = sapply(df, function(x) {
      vals <- unique(na.omit(x))
      if (length(vals) <= 7) {
        paste(vals, collapse = ", ")
      } else {
        "Not Applicable"
      }
    }),
    min_median_mean_max = sapply(df, function(x) {
      if (is.numeric(x)) {
        stats <- c(
          Min = min(x, na.rm = TRUE),
          Median = median(x, na.rm = TRUE),
          Mean = round(mean(x, na.rm = TRUE), 1),
          Max = max(x, na.rm = TRUE)
        )
        paste(names(stats), stats, sep = " = ", collapse = "; ")
      } else {
        "Not Applicable"
      }
    }),
    count_NA = sapply(df, function(x) sum(is.na(x))),
    count_filled_values = sapply(df, function(x) sum(!is.na(x))),
    count_empty_values = sapply(df, function(x) sum(x == "", na.rm = TRUE)),
    percentage_NA = sapply(df, function(x) sprintf("%.2f %%", sum(is.na(x)) / length(x) * 100))
  )
}

#' Calculate Level Agreement for Factor Variables
#'
#' This helper function compares the factor levels of variables in two data frames
#' and returns a description of their agreement.
#'
#' @param real_data A data frame containing the original variables.
#' @param syn_data A data frame containing the synthetic or comparison variables.
#' @return A vector describing the level agreement for each variable:
#' "Exact Match" if levels are identical, "Level Match, Order Mismatch" if levels match but order differs,
#' "Level Mismatch" if levels differ, and "Non-Factor" if the variable is not a factor in either data frame.
#' @keywords internal
calculate_level_agreement <- function(real_data, syn_data) {
  sapply(names(real_data), function(column_name) {
    real_col <- real_data[[column_name]]
    syn_col <- syn_data[[column_name]]

    if (is.factor(real_col) && is.factor(syn_col)) {
      if (identical(levels(real_col), levels(syn_col))) {
        return("Exact Match")
      } else if (setequal(levels(real_col), levels(syn_col))) {
        return("Level Match, Order Mismatch")
      } else {
        return("Level Mismatch")
      }
    } else {
      return("Non-Factor")
    }
  })
}

# utils-data-preparation.R

#' Check if Inputs are Data Frames
#'
#' This helper function checks whether the provided inputs are data frames
#' and stops execution with an error message if they are not.
#'
#' @param real_data The main data frame that must be checked.
#' @param syn_data An optional second data frame to check. If NULL, it is ignored.
#' @keywords internal


validate_dataframe_comparer_inputs <- function(real_data, syn_data = NULL) {
  if (!is.data.frame(real_data)) {
    stop("real_data must be a data frame.")
  }
  if (!is.null(syn_data) && !is.data.frame(syn_data)) {
    stop("syn_data must be a data frame.")
  }
}


#' Compare Two Data Frames or Analyze a Single Data Frame
#'
#' This helper function either analyzes a single data frame or compares two data frames.
#' If two data frames are provided, it ensures they have matching column names and the same number of columns.
#'
#' @param real_data The main data frame to be analyzed or compared.
#' @param syn_data An optional second data frame for comparison. If NULL, only `real_data` is analyzed.
#' @return A tibble containing the analysis results for either one or both data frames.
#' @keywords internal
compare_dataframes <- function(real_data, syn_data = NULL) {
  if (is.null(syn_data)) {
    # If only one dataframe is provided, analyze it
    return(calculate_stats(real_data))
  } else {
    # Ensure column names match
    if (!all(names(real_data) == names(syn_data))) {
      stop("The column names of the two data frames do not match.")
    }

    # Ensure both dataframes have the same number of columns
    if (ncol(real_data) != ncol(syn_data)) {
      stop("The number of columns in the two data frames do not match.")
    }

    # Return NULL as placeholder if further processing is done later in dataframe_comparer
    return(NULL)
  }
}



#' Combine Comparison Results
#'
#' This helper function combines the statistical results from two data frames
#' (real and synthetic) side by side into a single tibble.
#'
#' @param real_data_stats A tibble containing statistics for the real data frame.
#' @param syn_data_stats A tibble containing statistics for the synthetic data frame.
#' @param level_agreement A vector describing the level agreement between the data frames.
#' @return A tibble that combines the results of both data frames side by side.
#' @keywords internal
combine_comparison_results <- function(real_data_stats, syn_data_stats, level_agreement) {
  tibble::tibble(
    variable_name = real_data_stats$variable_names,
    real_data_type = real_data_stats$type,
    syn_data_type = syn_data_stats$type,
    level_agreement = level_agreement,
    real_data_count_unique_values = real_data_stats$count_unique_values,
    syn_data_count_unique_values = syn_data_stats$count_unique_values,
    real_data_unique_values = real_data_stats$unique_values,
    syn_data_unique_values = syn_data_stats$unique_values,
    real_data_min_median_mean_max = real_data_stats$min_median_mean_max,
    syn_data_min_median_mean_max = syn_data_stats$min_median_mean_max,
    real_data_count_NA = real_data_stats$count_NA,
    syn_data_count_NA = syn_data_stats$count_NA,
    real_data_count_filled_values = real_data_stats$count_filled_values,
    syn_data_count_filled_values = syn_data_stats$count_filled_values,
    real_data_count_empty_values = real_data_stats$count_empty_values,
    syn_data_count_empty_values = syn_data_stats$count_empty_values,
    real_data_percentage_NA = real_data_stats$percentage_NA,
    syn_data_percentage_NA = syn_data_stats$percentage_NA
  )
}
