# Helper function for error handling
check_input_not_empty <- function(data, keys = NULL, target = NULL, data_name = deparse(substitute(data))) {
  if(!is.data.frame(data)) {
    stop(sprintf("Input data must be a data frame: '%s'.", data_name))
  }

  if (nrow(data) == 0) {
    stop(sprintf("Input data set '%s' is empty.", data_name))
  }

  if (!is.null(target)) {
    if (!(target %in% names(data))) {
      stop(sprintf("Target column '%s' is not present in data set '%s'.", target, data_name))
    }

    if (all(is.na(data[[target]]) | data[[target]] == "")) {
      stop(sprintf("Target column '%s' in data set '%s' contains only NA or empty string values.", target, data_name))
    }
  }

  if (!is.null(keys)) {
    if (!all(keys %in% names(data))) {
      stop(sprintf("One or more key columns are missing in data set '%s'.", data_name))
    }

    if (any(sapply(data[keys], function(col) all(is.na(col) | col == "")))) {
      stop(sprintf("One or more key columns in data set '%s' contain only NA or empty string values.", data_name))
    }
  }
}

#helper function
make_na_value <- function(col) {
  if (is.factor(col)) {
    # Add factor level for NA
    if (!"<NA>" %in% levels(col)) {
      levels(col) <- c(levels(col), "<NA>")
    }
    col[is.na(col)] <- "<NA>"
  } else if (is.numeric(col) && !is.integer(col)) {
    col[is.na(col)] <- NA_real_
  } else if (is.integer(col)) {
    col[is.na(col)] <- NA_integer_
  } else if (is.logical(col)) {
    col[is.na(col)] <- NA
  } else {
    col[is.na(col)] <- "<NA>"
  }
  return(col)
}

#' @title Calculate BaseCAP
#' @description
#' Computes the baseline attribute disclosure (BaseCAP) for a target variable \code{t}
#' based solely on its marginal distribution in the original data set.
#'
#' BaseCAP represents the minimum level of information disclosure possible,
#' assuming an attacker has no knowledge of quasi-identifiers (\code{q})
#' and only knows the overall distribution of the target variable.
#'
#' It is defined as the sum of squared relative frequencies of all target values:
#' \deqn{\text{BaseCAP} = \sum_{t \in T} p_t^2}
#' where \eqn{p_t} is the relative frequency of target value \eqn{t}.
#'
#' @param orig_data A data frame containing the original data set.
#' @param target A character string specifying the name of the target column (\code{t}).
#' @param na_as_value Logical; if \code{TRUE} (default), treat \code{NA} as a separate target category/value.
#'
#' @return A single numeric value representing BaseCAP.
#' @details
#' When \code{na_as_value = TRUE} (default), NA values are treated as a distinct category.
#' This distinction is important when \code{NA} represents a true category (e.g., "not applicable") rather than a missing value.
#' When \code{na_as_value = FALSE}, any row containing NA in the target
#' column will have a BaseCAP contribution of zero but is still included in the average.
#'
#' Empty strings ("") are treated as values.
#'
#' @examplesIf requireNamespace("synthpop", quietly = TRUE)
#' vars <- c("age", "sex", "marital", "income", "ls", "smoke")
#' orig_data <- synthpop::SD2011[, vars]
#' BaseCAP <- calculate_basecap(orig_data, target = "marital")
#'
#' @export
#'

calculate_basecap <- function(orig_data, target, na_as_value = TRUE) {
  #error handling
  check_input_not_empty(data = orig_data, target = target)

  target_values <- orig_data[[target]]

  p_t <- table(target_values, useNA = if (na_as_value) "ifany" else "no") / length(target_values)

  basecap <- sum(p_t^2)
  return(basecap)
}

#' @title Calculate CAPo
#' @description
#' Calculates the average probability that an attacker, knowing the quasi-identifiers
#' (\code{q}), can correctly guess the target variable (\code{t}) in the original data set.
#'
#' CAPo measures the maximum information disclosure, assuming full access to the original data.
#' For each combination \eqn{(q, t)} of values from \code{keys} (\eqn{q}) and \code{target} (\eqn{t}), let:
#' \deqn{n_{qt} = \text{number of rows with combination } (q,t)}
#' \deqn{n_q = \text{number of rows with key combination } q}
#'
#' For each combination \eqn{q,t}:
#' \deqn{\text{CAPo}_{qt} = \frac{n_{qt}}{n_q}}
#'
#' The function then returns the weighted mean across all records:
#' \deqn{\text{CAPo} = \text{weighted mean}(\text{CAPo}(q,t)) \text{ for all } (q,t) \in \text{Original data}}.
#' Equivalently:
#' \deqn{\text{CAPo} = \sum_{q,t} \text{CAPo}_{qt} \times \frac{n_{qt}}{N}}
#' \deqn{\text{or: CAPo} = \sum_{q,t} \frac{n_{qt}}{n_q} \times \frac{n_{qt}}{N}}
#'
#' @param orig_data A data frame containing the original data set.
#' @param keys A character vector of column names representing quasi-identifiers (\code{q}).
#' @param target A character string specifying the name of the target column (\code{t}).
#' @param na_as_value Logical; if \code{TRUE} (default), treat \code{NA} as a separate category/value in both target and key columns.
#'
#' @return A single numeric value representing CAPo.
#'
#' @details
#' When \code{na_as_value = TRUE} (default), NA values are treated as a distinct category.
#' This distinction is important when \code{NA} represents a true category (e.g., "not applicable") rather than a missing value.
#' When \code{na_as_value = FALSE}, any row containing NA in either the target
#' or any key column will have a CAPo contribution of zero but is still included in the average.
#'
#' Empty strings ("") are treated as values.
#'
#' @examplesIf requireNamespace("synthpop", quietly = TRUE)
#' vars <- c("age", "sex", "marital", "income", "ls", "smoke")
#' orig_data <- synthpop::SD2011[, vars]
#' CAPo <- calculate_capo(orig_data, keys = c("age", "sex"), target = "marital")
#'
#' @export
#'

calculate_capo <- function(orig_data, keys, target, na_as_value = TRUE) {
  #error handling
  check_input_not_empty(orig_data, keys, target)

  if (na_as_value) {
    orig_data[[target]] <- make_na_value(orig_data[[target]])
    for (k in keys) {
      orig_data[[k]] <- make_na_value(orig_data[[k]])
    }
  }

  qt_freq <- orig_data %>%
    count(across(all_of(c(keys, target))), name = "n_qt")

  q_freq <- orig_data %>%
    count(across(all_of(keys)), name = "n_q")

  merged <- qt_freq %>%
    left_join(q_freq, by = keys) %>%
    mutate(cap_qt = n_qt/ n_q)

  if (!na_as_value) {
    # Set cap_qt to 0 where either keys or target contains NA
    na_rows <- apply(merged[, c(keys, target)], 1, function(row) any(is.na(row)))
    merged$cap_qt[na_rows] <- 0
  }

  capo <- weighted.mean(merged$cap_qt, w = merged$n_qt)

  return(capo)
}

#' @title Calculate CAPs
#' @description
#' Computes the information disclosure, consisting of group-level information and attribution, in the synthetic data set
#' using the quasi-identifiers (\code{q}) and a known target variable (\code{t}),
#' assuming an attacker has access to the synthetic data set and knows the original
#' values of the quasi-identifiers.
#'
#' For each combination \eqn{(q, t)} of values from \code{keys} (\eqn{q}) and \code{target} (\eqn{t}), let:
#' \deqn{n_{qt}^{orig} = \text{number of rows in original data with } (q,t)}
#' \deqn{n_{qt}^{syn} = \text{number of rows in synthetic data with } (q,t)}
#' \deqn{n_q^{syn} = \text{number of rows in synthetic data with } q}
#'
#' For each combination \eqn{(q,t)} in the original data set:
#' \deqn{\text{CAPs}_{qt} = \frac{n_{qt}^{syn}}{n_q^{syn}}}
#'
#' The function then returns the weighted mean across all records (weighted by the original counts \eqn{n_{qt}^{orig}}):
#' \deqn{\text{CAPs} = \text{weighted mean}(\text{CAPs}_{qt}) \text{ for all } (q,t) \in \text{Original data}}
#' Equivalently:
#' \deqn{\text{CAPS} = \sum_{q,t} \text{CAPs}_{qt} \times \frac{n_{qt}^{orig}}{N_{orig}} }
#' \deqn{\text{or: CAPS} = \sum_{q,t} \frac{n_{qt}^{syn}}{n_q^{syn}} \times \frac{n_{qt}^{orig}}{N_{orig}}}
#'
#' @param orig_data A data frame containing the original data set.
#' @param syn_data A data frame containing the synthetic data set.
#' @param keys A character vector of column names representing quasi-identifiers (\code{q}).
#' @param target A character string specifying the name of the target column (\code{t}).
#' @param na_as_value Logical; if \code{TRUE} (default), treat \code{NA} as a separate category/value in both target and key columns.
#'
#'
#' @return A single numeric value representing CAPs.
#' @details
#' When \code{na_as_value = TRUE} (default), NA values are treated as a distinct category.
#' This distinction is important when \code{NA} represents a true category (e.g., "not applicable") rather than a missing value.
#' When \code{na_as_value = FALSE}, any row containing NA in either the target
#' or any key column will have a CAPs contribution of zero but is still included in the average.
#'
#' Empty strings ("") are treated as values.
#'
#' @examplesIf requireNamespace("synthpop", quietly = TRUE)
#' vars <- c("age", "sex", "marital", "income", "ls", "smoke")
#' orig_data <- synthpop::SD2011[, vars]
#' syn_data <- synthpop::syn(orig_data)$syn
#'
#' CAPs <- calculate_caps(orig_data, syn_data, keys = c("age", "sex"), target = "marital")
#'
#' @import purrr
#' @export
#'

calculate_caps <- function(orig_data, syn_data, keys, target, na_as_value = TRUE) {
  #error handling
  check_input_not_empty(orig_data, keys, target)
  check_input_not_empty(syn_data, keys, target)

  if (na_as_value) {
    orig_data[[target]] <- make_na_value(orig_data[[target]])
    syn_data[[target]] <- make_na_value(syn_data[[target]])

    for (k in keys) {
      orig_data[[k]] <- make_na_value(orig_data[[k]])
      syn_data[[k]] <- make_na_value(syn_data[[k]])
    }
  }

  qt_orig <- orig_data %>%
    count(across(all_of(c(keys, target))), name = "n_qt_orig")

  qt_syn <- syn_data %>%
    count(across(all_of(c(keys, target))), name = "n_qt_syn")

  q_syn <- syn_data %>%
    count(across(all_of(keys)), name = "n_q_syn")

  merged <- qt_orig %>%
    left_join(qt_syn, by = c(keys, target)) %>%
    left_join(q_syn, by = keys) %>%
    replace_na(list(n_qt_syn = 0, n_q_syn = 0))

  if (!na_as_value) {
    merged <- merged %>%
      mutate(
        has_na = purrr::pmap_lgl(select(., all_of(c(keys, target))), ~ any(is.na(c(...)))),
        cap_qt = ifelse(has_na, 0,
                        ifelse(n_q_syn > 0,
                               (n_qt_syn / n_q_syn),
                               0))
      )
  } else {
    merged <- merged %>%
      mutate(
        cap_qt = ifelse(n_q_syn > 0,
                        (n_qt_syn / n_q_syn),
                        0)
      )
  }

  caps <- weighted.mean(merged$cap_qt, w = merged$n_qt_orig)

  return(caps)

}

#' @title Calculate hCAPs
#' @description
#' hCAPs measures how much group-level information from the holdout data
#' is preserved in the synthetic data set.
#'
#' For each combination \eqn{(q, t)} of values from \code{keys} (\eqn{q}) and \code{target} (\eqn{t}), let:
#' \deqn{n_{qt}^{hold} = \text{number of rows in holdout data with } (q,t)}
#' \deqn{n_{qt}^{syn} = \text{number of rows in synthetic data with } (q,t)}
#' \deqn{n_q^{syn} = \text{number of rows in synthetic data with } q}
#' #'
#' For each combination \eqn{(q,t)} in the holdout data set:
#' \deqn{\text{hCAPs}_{qt} = \frac{n_{qt}^{syn}}{n_q^{syn}}}
#'
#' The function then returns the weighted mean across all records (weighted by the holdout counts \eqn{n_{qt}^{hold}}):
#' \deqn{\text{hCAPs} = \text{weighted mean}(\text{hCAPs}_{qt}) \text{ for all } (q,t) \in \text{Holdout data}}
#' Equivalently:
#' \deqn{\text{hCAPs} = \sum_{q,t} \text{hCAPs}_{qt} \times \frac{n_{qt}^{hold}}{N_{hold}}}
#' \deqn{\text{or: hCAPs} = \sum_{q,t} \frac{n_{qt}^{syn}}{n_q^{syn}} \times \frac{n_{qt}^{hold}}{N_{hold}}}
#'
#' @param holdout_data A data frame containing the out-of-sample (real) data not used during synthesis.
#' @param syn_data A data frame containing the synthetic data set.
#' @param keys A character vector of column names representing quasi-identifiers (\code{q}).
#' @param target A character string specifying the name of the target column (\code{t}).
#' @param na_as_value Logical; if \code{TRUE} (default), treat \code{NA} as a separate category/value in both target and key columns.
#'
#' @return A single numeric value representing hCAPs.
#' @details
#' When \code{na_as_value = TRUE} (default), NA values are treated as a distinct category.
#' This distinction is important when \code{NA} represents a true category (e.g., "not applicable") rather than a missing value.
#' When \code{na_as_value = FALSE}, any row containing NA in either the target
#' or any key column will have a hCAPs contribution of zero but is still included in the average.
#'
#' Empty strings ("") are treated as values.
#'
#' @examplesIf requireNamespace("synthpop", quietly = TRUE)
#' vars <- c("age", "sex", "marital", "income", "ls", "smoke")
#' orig_data <- synthpop::SD2011[, vars]
#'
#' set.seed(123)
#' idx <- sample(seq_len(nrow(orig_data)), size = 0.8*nrow(orig_data))
#' train_data <- orig_data[idx, ]
#' holdout_data <- orig_data[-idx, ]
#' syn_data <- synthpop::syn(train_data)$syn
#'
#' hCAPs <- calculate_hcaps(holdout_data, syn_data, keys = c("age", "sex"), target = "marital")
#'
#' @export
#'

calculate_hcaps <- function(holdout_data, syn_data, keys, target, na_as_value = TRUE) {
  #error handling
  check_input_not_empty(holdout_data, keys, target)
  check_input_not_empty(syn_data, keys, target)

  if (na_as_value) {
    holdout_data[[target]] <- make_na_value(holdout_data[[target]])
    syn_data[[target]] <- make_na_value(syn_data[[target]])

    for (k in keys) {
      holdout_data[[k]] <- make_na_value(holdout_data[[k]])
      syn_data[[k]] <- make_na_value(syn_data[[k]])
    }
  }

  qt_holdout <- holdout_data %>%
    count(across(all_of(c(keys, target))), name = "n_qt_holdout")

  qt_syn <- syn_data %>%
    count(across(all_of(c(keys, target))), name = "n_qt_syn")

  q_syn <- syn_data %>%
    count(across(all_of(keys)), name = "n_q_syn")

  merged <- qt_holdout %>%
    left_join(qt_syn, by = c(keys, target)) %>%
    left_join(q_syn, by = keys) %>%
    replace_na(list(n_qt_syn = 0, n_q_syn = 0))

  if (!na_as_value) {
    merged <- merged %>%
      mutate(
        has_na = purrr::pmap_lgl(select(., all_of(c(keys, target))), ~ any(is.na(c(...)))),
        hcap_qt = ifelse(has_na, 0,
                         ifelse(n_q_syn > 0,
                                (n_qt_syn / n_q_syn),
                                0))
      )
  } else {
    merged <- merged %>%
      mutate(
        hcap_qt = ifelse(n_q_syn > 0,
                         (n_qt_syn / n_q_syn),
                         0)
      )
  }

  hcaps <- weighted.mean(merged$hcap_qt, w = merged$n_qt_holdout)

  return(hcaps)
}

#helper function for creating plots in huisstijl without error messages
font_vinden_safe <- function() {
  if (requireNamespace("IPhuisstijl", quietly = TRUE)) {
    IPhuisstijl::font_vinden()
  } else {
    "sans"
  }
}

IP_kleuren_safe <- c("#007bc7", "#ffb612", "#673327", "#8fcae7", "#39870c", "#76d2b6")
