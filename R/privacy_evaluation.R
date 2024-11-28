#' @title Privacy Evaluation Metric
#'
#' @description
#' Computes privacy metrics for synthetic data.
#' The function can perform either simple or stratified data synthesis based on the provided parameters.
#'
#' @param data_observed The real dataset.
#' @param strata (Optional) A column name or vector of column names for stratification.
#' @param split_ratio Ratio for train-test split, default is 0.5.
#' @param na_value Value to replace NA's with, default is -1.
#' @param ... Additional arguments passed to the synthesis function (e.g., `minstratumsize` for `syn.strata`).
#'
#' @return Privacy metrics.
#' @export
#'
#' @examples
#' library(ggplot2)
#' data_observed <- data.frame(head(diamonds, 2000))
#' # Without stratification
#' privacy_evaluation(data_observed = data_observed)
#' # With stratification
#' data_observed <- data.frame(head(diamonds, 4500))
#' privacy_evaluation(data_observed = data_observed, strata = "cut")
#' @import dplyr
#' @importFrom synthpop syn syn.strata
#' @import ggplot2
privacy_evaluation <- function(data_observed, strata = NULL, split_ratio = 0.5, na_value = -1, ...) {

  # Validate the data
  validate_data(data_observed)

  # Split the dataset into train and holdout sets
  splits <- split_data(data_observed, split_ratio)
  train_data <- splits$train
  holdout_data <- splits$holdout

  # Data synthesis
  sds_train <- synthesize_data(train_data, strata = strata, ...)

  # Replace NA's
  train_data <- handle_na(train_data, na_value)
  holdout_data <- handle_na(holdout_data, na_value)
  sds_train[["syn"]] <- handle_na(sds_train[["syn"]], na_value)

  # Calculate the minimum Hamming distance
  DCR <- calculate_min_hamming_distance(
    sds_train = sds_train,
    train_data = train_data,
    holdout_data = holdout_data
  )

  # Calculate privacy metrics
  metrics <- calculate_privacy_metrics(DCR)

  return(metrics)
}
