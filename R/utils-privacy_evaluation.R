# =====================================================
# Helper Function: validate_data ----
# =====================================================

#' @title Validate the Dataset
#'
#' @description
#' Validates the dataset by checking if it meets certain criteria.
#' Issues a warning if the dataset contains fewer than 1000 observations.
#' Stops execution if the dataset has zero observations.
#'
#' @param data The dataset to validate.
#'
#' @return Throws an error or warning if validation fails, otherwise returns NULL.
#' @keywords internal
validate_data <- function(data) {
  if (nrow(data) < 1000) {
    warning("Warning: dataframe should contain more than 1000 observations")
  }

  if (nrow(data) == 0) {
    stop("Error: you cannot use this function on a dataframe with zero observations")
  }
}

# =====================================================
# Helper Function: split_data ----
# =====================================================

#' @title Split Dataset into Training and Holdout Sets
#'
#' @description
#' Splits the dataset into training and holdout sets based on the provided split ratio.
#'
#' @param data The dataset to be split.
#' @param split_ratio The ratio for splitting the dataset into train and holdout sets. Default is 0.5.
#'
#' @return A list containing two dataframes: \code{train} and \code{holdout}.
#' @keywords internal
split_data <- function(data, split_ratio) {
  split <- sample(c(rep(0, floor(split_ratio * nrow(data))),
                    rep(1, nrow(data) - floor(split_ratio * nrow(data)))))
  train <- data[split == 0, ]
  holdout <- data[split == 1, ]
  return(list(train = train, holdout = holdout))
}

# =====================================================
# Helper Function: synthesize_data ----
# =====================================================

#' @title Synthesize Data (Simple or Stratified)
#'
#' @description
#' Synthesizes data using either simple synthesis or stratified synthesis based on the provided `strata` parameter.
#'
#' @param train_data The training dataset to be synthesized.
#' @param strata (Optional) A column name or vector of column names for stratification. If `NULL`, simple synthesis is performed.
#' @param ... Additional arguments passed to the synthesis functions (e.g., `minstratumsize` for `syn.strata`).
#'
#' @return A synthesized dataset object returned by the synthesis function.
#' @importFrom synthpop syn syn.strata
synthesize_data <- function(train_data, strata = NULL, ...) {
  if (is.null(strata)) {
    # Simple synthesis using 'syn' function
    sds_train <- syn(train_data, ...)
  } else {
    # Stratified synthesis using 'syn.strata' from synthpop
    sds_train <- syn.strata(train_data, strata = strata, ...)
  }
  return(sds_train)
}

# =====================================================
# Helper Function: handle_na ----
# =====================================================

#' @title Replace NA Values in the Dataset
#'
#' @description
#' Replaces all NA values in the dataset with a specified value.
#'
#' @param data The dataset in which NA values should be replaced.
#' @param na_value The value to replace NA values with.
#'
#' @return A dataframe where all NA values have been replaced by \code{na_value}.
#' @keywords internal
handle_na <- function(data, na_value) {
  data[is.na(data)] <- na_value
  return(data)
}

# =====================================================
# Helper Function: calculate_min_hamming_distance ----
# =====================================================

#' @title Calculate Minimum Hamming Distance
#'
#' @description
#' Calculates the minimum Hamming distance between each synthetic observation and the observations in both the training and holdout datasets.
#'
#' @param sds_train List that contains the synthetic dataframe.
#' @param train_data Training data.
#' @param holdout_data Holdout data.
#'
#' @return A dataframe with the minimum Hamming distances to the training and holdout data for each synthetic observation.
#' @keywords internal
calculate_min_hamming_distance <- function(sds_train, train_data, holdout_data) {

  # Initialize a data frame to store results
  min_hamming_distances <- data.frame(
    min_distance_train = numeric(nrow(sds_train[["syn"]])),
    min_distance_holdout = numeric(nrow(sds_train[["syn"]]))
  )

  # Loop through each row of sds_train[["syn"]]
  for (i in seq_len(nrow(sds_train[["syn"]]))) {
    row <- sds_train[["syn"]][i, ]

    # Calculate Hamming distance for train_data
    train_distances <- rowSums(train_data != row[rep(1, nrow(train_data)), ])
    min_hamming_distances$min_distance_train[i] <- min(train_distances)

    # Calculate Hamming distance for holdout_data
    holdout_distances <- rowSums(holdout_data != row[rep(1, nrow(holdout_data)), ])
    min_hamming_distances$min_distance_holdout[i] <- min(holdout_distances)
  }

  return(min_hamming_distances)
}

# =====================================================
# Helper Function: calculate_privacy_metrics ----
# =====================================================

#' @title Calculate Privacy Metrics
#'
#' @description
#' Calculates various privacy metrics based on the distance comparisons between synthetic and observed datasets.
#'
#' @param DCR A dataframe containing distance comparisons between synthetic and observed datasets.
#'
#' @return A named vector containing various privacy metrics.
#' @keywords internal
calculate_privacy_metrics <- function(DCR) {
  metrics <- c(
    share_training_data =
      (sum(DCR$min_distance_train < DCR$min_distance_holdout) +
         0.5 * sum(DCR$min_distance_train == DCR$min_distance_holdout)) / nrow(DCR),
    n_syn = nrow(DCR),
    n_closer = sum(DCR$min_distance_train < DCR$min_distance_holdout),
    n_farther = sum(DCR$min_distance_train > DCR$min_distance_holdout),
    n_equal = sum(DCR$min_distance_train == DCR$min_distance_holdout),
    mean_distance_train = mean(DCR$min_distance_train),
    mean_distance_holdout = mean(DCR$min_distance_holdout)
  )
  return(metrics)
}
