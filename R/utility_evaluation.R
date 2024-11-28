#' Utility test
#'
#' This function measures the utility of a synthetic dataset by generating
#' 1- and 2-dimensional (visual) assessments, using the SpMSE
#'
#' @param syn_object The synthetic data object as synthesized by using synthpop, with class synds
#' @param data_observed The observed data from which the synthesized data was created, with class dataframe
#' @param colors Five colors that will be used used in the utility plot.
#' @param ngroups Parameter in the function utility.tables {synthpop}
#' @param utility_stat Parameter in the function compare.synds {synthpop}
#' @param autofix Logical; if TRUE, automatically aligns and validates the synthetic and observed dataframes.

#' @return A list of 1- and 2-dimensional utility visuals
#' @examples
#' # Generate synthetic data and measure the utility
#' syn_object <- list(syn = data.frame(a = 1:3, b = 4:6), m = 1)
#' syn_object$syn$a <- as.factor(syn_object$syn$a)
#' syn_object$syn$b <- as.factor(syn_object$syn$b)
#' class(syn_object) <- "synds"
#' data_observed <- data.frame(a = 1:3, b = 4:6)
#' data_observed$a <- as.factor(data_observed$a)
#' data_observed$b <- as.factor(data_observed$b)
#' utility_evaluation(syn_object, data_observed)
#'
#' # Example usage of utility_evaluation with mismatched factor levels
#' observed_data <- data.frame(
#'   a = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
#'   b = 1:3
#' )
#' synthetic_data <- data.frame(
#'   a = factor(c("A", "B", "C"), levels = c("A", "C", "B")),
#'   b = c("3", "1", "2")
#' )
#' syn_object <- structure(list(syn = synthetic_data, m = 1), class = "synds")
#' # Align factor levels using autofix
#' result <- utility_evaluation(syn_object, observed_data, autofix = TRUE)
#' # Align factor levels using autofix
#' result <- utility_evaluation(syn_object, observed_data, autofix = TRUE)
#' @import
#' purrr
#' tibble
#' synthpop
#' dplyr
#' ggplot2
#'
#' @export
utility_evaluation <- function(syn_object, data_observed,
                               colors = c("darkgreen", "lightgreen", "yellow", "orange", "red"),
                               ngroups = 25, utility_stat = "S_pMSE", autofix = FALSE){

  # validate inputs
  check_df_data_observed(data_observed)
  check_columns(syn_object, data_observed)
  check_synds_syn_object(syn_object)
  check_factor_level_count(syn_object, data_observed)


  # synchronize_na_levels
  data_list <- synchronize_na_levels(data_observed, syn_object$syn)
  data_observed <- data_list$observed_data
  syn_object$syn <- data_list$synthetic_data


  # Optionally autofix dataframes
  if (autofix) {
    syn_object$syn <- validate_and_align_dataframes(data_observed, syn_object$syn)
  }

  # Validate the input
  #validate_inputs(syn_object, data_observed)

  # Run utility tests
  utility_data <- run_utility_tests(syn_object, data_observed, utility_stat, ngroups)

  # Create utility plot
  plot <- plot_utility_data(utility_data, colors)

  # Add plot to utility data list
  utility_data[["plot"]] <- plot

  return(utility_data)
}
