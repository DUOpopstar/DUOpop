
#' @title Harmonize Factor Levels between two Data Frames
#'
#' @description This function ensures that factor columns with matching names in
#' two data frames have the same set of levels. It updates the factors in both data frames
#' so that all levels present in either data frame are represented in both.
#'
#' @param data.x The first data frame.
#' @param data.y The second data frame.
#'
#' @return A list with two elements:
#' \describe{
#'  \item{data.x}{The first data frame, with harmonized factor levels.}
#'  \item{data.y}{The second data frame, with harmonized factor levels.}
#' }
#'
#' @details
#' The function checks for columns that are factors in both \code{data.x} and \code{data.y}
#' and harmonizes their levels. If a factor level is present in one data frame but not
#' in the other, it is added to both. Non-factor columns are left unchanged.
#'
#' Called by \code{\link{calculate_DCR}}.
#'
#' @examples
#' df1 <- data.frame(color = factor(c("red", "blue"), levels = c("red", "blue")))
#' df2 <- data.frame(color = factor(c("blue", "yellow"), levels = c("blue", "yellow")))
#' result <- harmonize_factor_levels(df1, df2)
#' levels(result$data.x$color)
#' levels(result$data.y$color)
#'
#' @export
#'
harmonize_factor_levels <- function(data.x, data.y) {
  if(!is.data.frame(data.x)) stop("Input 'data.x' must be a data frame.")
  if(!is.data.frame(data.y)) stop("Input 'data.y' must be a data frame.")
  if(ncol(data.x) == 0 || nrow(data.x) == 0) stop("Input 'data.x' is empty.")
  if(ncol(data.y) == 0 || nrow(data.y) == 0) stop("Input 'data.y' is empty.")

  for (col in colnames(data.x)) {
    if (is.factor(data.x[[col]]) && is.factor(data.y[[col]])) {
      all_levels <- union(levels(data.x[[col]]), levels(data.y[[col]]))
      data.x[[col]] <- factor(data.x[[col]], levels = all_levels)
      data.y[[col]] <- factor(data.y[[col]], levels = all_levels)
    }
  }
  return(list(data.x = data.x, data.y = data.y))
}

gower.fcn <- function(x, y, rng = NULL, KR.corr = KR.corr,na.as.distance) {
  nx <- length(x)
  ny <- length(y)
  #cx <- class(x) #DUOpop: changed because I still got errors when there was a difference in numeric and integer
  #cy <- class(y)
  cx <- typeof(x)
  cy <- typeof(y)
  if ((cx == "integer" && cy == "double") || (cx == "double" && cy == "integer")) {
    x <- as.numeric(x)
    y <- as.numeric(y)
    cx <- "double"
    cy <- "double"
  }
  delta <- matrix(1, nx, ny)
  if (!identical(cx, cy)) stop("objects x and y have different types")

  # # --- Calculate distance per data type ---
  # if (is.logical(x)) {
  #   dd <- abs(outer(X = x, Y = y, FUN = "-"))
  # } else if (is.character(x) || (is.factor(x) && !is.ordered(x))) {
  #   if (is.factor(x) && !identical(levels(x), levels(y))) stop("factors x and y have different levels")
  #   dd <- 1 - outer(x, y, FUN = "==")
  # } else if (is.ordered(x)) {
  #   x <- as.numeric(x)
  #   y <- as.numeric(y)
  #   if (is.null(rng) || is.na(rng)) rng <- max(x, y, na.rm = TRUE) - 1
  #   if (rng == 0) dd <- matrix(0, nx, ny)
  #   else dd <- abs(outer(X = x, Y = y, FUN = "-")) / rng
  # } else {
  #   if (is.null(rng) || is.na(rng)) rng <- max(x, y, na.rm = TRUE) - min(x, y, na.rm = TRUE)
  #   if (rng == 0) dd <- matrix(0, nx, ny)
  #   else dd <- abs(outer(X = x, Y = y, FUN = "-")) / rng
  # }
  #DUOpop: changed to handle empty vectors

  # --- Handle empty vectors ---
  if (nx == 0 || ny == 0) {
    dd <- matrix(0, nx, ny)
  } else {
    # --- Calculate distance per data type ---
    if (is.logical(x)) {
      dd <- abs(outer(x, y, FUN = "-"))
    } else if (is.character(x) || (is.factor(x) && !is.ordered(x))) {
      if (is.factor(x) && !identical(levels(x), levels(y))) stop("factors x and y have different levels")
      #Below added for our NA logic
      if (all(is.na(x)) && all(is.na(y))) {
        dd <- matrix(0, nx, ny)
        if (na.as.distance == TRUE) delta[,] <- 1
      } else {
        dd <- 1 - outer(x, y, FUN = "==")
      }
    }  else if (is.ordered(x)) {
      # Ordinal
      x_num <- as.numeric(x)
      y_num <- as.numeric(y)
      if (all(is.na(x_num)) && all(is.na(y_num))) {
        dd <- matrix(0, nx, ny)
        if (na.as.distance) delta[,] <- 1
      } else {
        if (KR.corr) {
          # Kaufman & Rousseeuw 1990
          L <- max(x_num, y_num, na.rm = TRUE)
          if (L == 1) {
            dd <- matrix(0, nx, ny)
          } else {
            zx <- (x_num - 1) / (L - 1)
            zy <- (y_num - 1) / (L - 1)
            dd <- abs(outer(zx, zy, "-"))
          }
        } else {
          # Original Gower 1971
          rng <- max(c(x_num, y_num), na.rm = TRUE) - min(c(x_num, y_num), na.rm = TRUE)
          if (rng == 0) {
            dd <- matrix(0, nx, ny)
          } else {
            dd <- abs(outer(x_num, y_num, "-")) / rng
          }
        }
      }
    } else { #numeric
      x_num <- as.numeric(x)
      y_num <- as.numeric(y)
      # --- All NA check ---
      if (all(is.na(x_num)) && all(is.na(y_num))) {
        dd <- matrix(0, nx, ny)
        if (na.as.distance == TRUE) delta[,] <- 1
      } else {
        if (is.null(rng) || is.na(rng)) rng <- max(c(x_num, y_num), na.rm = TRUE) - min(c(x_num, y_num), na.rm = TRUE)
        if (rng == 0) dd <- matrix(0, nx, ny)
        else dd <- abs(outer(x_num, y_num, FUN = "-")) / rng
      }
    }
  }


  # ---- DUOpop: How we want to treat NA ----
  if (na.as.distance == FALSE) {
    # Standard Gower: Ignore NA in averaging the distance over the row (delta = 0)
    delta[outer(is.na(x), is.na(y), FUN = "|")] <- 0
  } else {
    # Our wanted logic: NA has either distance 0 or 1 and counts in the row average (delta = 1)
    delta[,] <- 1
    both_na <- outer(is.na(x), is.na(y), FUN = "&")
    one_na  <- outer(is.na(x), is.na(y), FUN = "|") & !both_na
    dd[both_na] <- 0
    dd[one_na]  <- 1
  }

  list(dist = dd, delta = delta)
}

#' @title Custom Gower Distance Calculation
#'
#' @description This function computes pairwise Gower distances between observations in
#' one or two data sets, extending and modifying the implementation of \code{Gower.dist}
#' from the \strong{shipunov} package. It supports mixed data types
#' (numeric, ordinal, nominal, logical, and character) and provides flexible handling
#' of missing values and ordinal scaling.
#'
#' @param data.x A data frame, matrix, or vector containing observations (rows) and variables
#'      (columns). If a vector is supplied, will be treated as a single observation.
#' @param data.y Optional: a data frame, matrix, or vector. If supplied, distances are
#'      calculated between \code{data.x} and \code{data.y}; otherwise distances are calculated
#'      within \code{data.x}.
#' @param rngs Optional: numeric vector of pre-computed ranges for numeric variables.
#'      If \code{NULL}, ranges are calculated from the data.
#' @param KR.corr Logical; if \code{TRUE} (default), ordinal variables are scaled using the
#'      correction method of Kaufman & Rousseeuw (1990), where ordinal values are mapped to
#'      the unit interval \eqn{[0, 1]} based on their rank. If \code{FALSE}, the original Gower
#'      (1971) scaling is used where ordinal values are treated the same as numeric values.
#' @param na.as.distance Logical; determines how missing values (\code{NA}) are handled:
#'      \describe{
#'        \item{\code{FALSE}}{Standard Gower behavior: distances involving \code{NA} are excluded from averaging, effectively ignoring missing data.}
#'        \item{\code{TRUE} (default)}{\code{NA} values contribute to the distance computation. Pairs with one missing value are assigned a distance of 1; pairs with both values missing are assigned a distance of 0, and these comparisons count in the average.}
#'        }
#'
#' @details
#' This function is based on the Gower distance metric, which is designed to handle
#' mixed data types:
#' \itemize{
#'    \item Numeric variables are scaled to \eqn{[0,1]} by their range.
#'    \item Nominal and character variables are compared for their exact matches.
#'    \item Ordinal variables are treated either using Kaufman & Rousseeuw's (1990) correction (\code{KR.corr = TRUE}) or Gower's original scaling, which treats them the same as nominal variables.
#'    \item Logical variables are treated as binary factors.
#' }
#'
#' Called by \code{\link{calculate_min_gower}}.
#'
#' @return A distance object of class \code{dist} if only \code{data.x} is considered; otherwise, a numeric matrix of pairwise distances.
#'
#' @references
#' Gower, J. C. (1971). A General Coefficient of Similarity and Some of Its Properties. \emph{Biometrics}, 27(4), 857-871. doi:10.2307/2528823
#'
#' Kaufman, L., & Rousseeuw, P. J. (1990). \emph{Finding Groups in Data: An Introduction to Cluster Analysis}. Wiley, New York. ISBN 0-471-87876-6. doi:10.2307/2532178
#'
#' Shipunov, A. (2020). \emph{Miscellaneous Functions from Alexey Shipunov}. R package version 1.17.1. doi:10.32614/CRAN.package.shipunov
#'
#' @examples
#' # Mixed data frame with numeric, factor, and logical variables
#' df <- data.frame(
#'   height = c(150, 160, 170, NA),
#'   color = factor(c("red", "blue", "red", "green")),
#'   likes_coffee = c(TRUE, FALSE, TRUE, NA)
#' )
#'
#' # Compute pairwise Gower distances with default settings
#' custom_gower(df)
#'
#'
custom_gower <- function(data.x, data.y = data.x, rngs = NULL, KR.corr = TRUE, na.as.distance = TRUE) {

  if (!is.data.frame(data.x) && !is.matrix(data.x) && !is.atomic(data.x)) {
    stop("data.x must be a vector, matrix or dataframe")
  }
  if (!is.data.frame(data.y) && !is.matrix(data.y) && !is.atomic(data.y)) {
    stop("data.y must be a vector, matrix or dataframe")
  }
  if (is.vector(data.x) && !is.null(dim(data.y))) {
    if (length(data.x) != ncol(data.y)) {
      stop("data.x should be the same length as the number of columns in data.y")
    }
  }

  # --- DUOpop: Convert vectors automatically to 1 x p data.frame ---
  if (is.vector(data.x)) data.x <- t(as.data.frame(data.x))
  if (is.vector(data.y)) data.y <- t(as.data.frame(data.y))

  if (ncol(data.x) == 0 || ncol(data.y) == 0) {
    stop("data.x and data.y must have at least one column")
  }



  # --- Combine distances over all variabeles ---
  if (is.null(dim(data.x)) && is.null(dim(data.y))) { #in principe kom je hier niet omdat een vector nu 1xp data frame wordt, is een overblijfseltje uit de originele gower.dist
    out.gow <- gower.fcn(x = data.x, y = data.y, rng = rngs, KR.corr = KR.corr,na.as.distance)
    out <- (out.gow$dist * out.gow$delta) / out.gow$delta
  } else if (is.null(dim(data.x)) && !is.null(dim(data.y))) {
    p <- ncol(data.y)

    #if (length(data.x) != p) stop("data.x should be the same length as the number of columns in data.y") #DUopop: put this error at the top since it will no longer be thrown now that vectors are automatically converted to 1xp dataframes
    num <- array(0, c(1, nrow(data.y)))
    den <- array(0, c(1, nrow(data.y)))
    for (k in 1:p) {
      if (is.null(rngs)) rng.k <- NULL else rng.k <- rngs[k]
      out.gow <- gower.fcn(x = data.x[, k], y = data.y[, k], rng = rng.k, KR.corr = KR.corr,na.as.distance)
      n <- out.gow$dist * out.gow$delta
      n[is.na(n)] <- 0
      num[] <- num + n
      d <- out.gow$delta
      d[is.na(d)] <- 0
      den <- den + d
    }
    out <- num / den
  } else {
    p <- ncol(data.y)
    if (ncol(data.x) != p) stop("data.x and data.y must have the same number of columns")
    num <- array(0, c(nrow(data.x), nrow(data.y)))
    den <- array(0, c(nrow(data.x), nrow(data.y)))
    for (k in 1:p) {
      if (is.null(rngs)) rng.k <- NULL else rng.k <- rngs[k]
      out.gow <- gower.fcn(x = data.x[, k], y = data.y[, k], rng = rng.k, KR.corr = KR.corr,na.as.distance)
      n <- out.gow$dist * out.gow$delta
      n[is.na(n)] <- 0
      num[] <- num + n
      d <- out.gow$delta
      d[is.na(d)] <- 0
      den <- den + d
    }
    out <- num / den
  }

  if (!is.null(row.names(data.x))) row.names(out) <- row.names(data.x)
  if (!is.null(row.names(data.y))) colnames(out) <- row.names(data.y)
  if (identical(data.x, data.y)) out <- as.dist(out)
  out
}

#' Calculate Minimum Gower Distances for a Single Observation
#'
#' @description
#' Helper function that calls \code{\link{custom_gower}} to compute the Gower distance between a given observation (`row`) and all rows in
#' a comparison data set (`data.y`), and returns both the minimum distance and the index of the closest observation.
#'
#' @param row A single observation for which the minim Gower distance will be computed.
#' @param data.y A data frame against which `row` will be compared. Defaults to `data.x` (the original data set).
#' @param rngs Optional: numeric vector of pre-computed ranges for numeric variables.
#'      If \code{NULL}, ranges are calculated from the data.
#' @param KR.corr Logical; if \code{TRUE} (default), ordinal variables are scaled using the
#'      correction method of Kaufman & Rousseeuw (1990), where ordinal values are mapped to
#'      the unit interval \eqn{[0, 1]} based on their rank. If \code{FALSE}, the original Gower
#'      (1971) scaling is used where ordinal values are treated the same as numeric values.
#' @param na.as.distance Logical; determines how missing values (\code{NA}) are handled:
#'      \describe{
#'        \item{\code{FALSE}}{Standard Gower behavior: distances involving \code{NA} are excluded from averaging, effectively ignoring missing data.}
#'        \item{\code{TRUE} (default)}{\code{NA} values contribute to the distance computation. Pairs with one missing value are assigned a distance of 1; pairs with both values missing are assigned a distance of 0, and these comparisons count in the average.}
#'        }
#'
#' @details
#' Internally calls \code{\link{custom_gower}}. Is called by \code{\link{calculate_DCR}}.
#'
#'
#' @return  A list with two elements:
#' \describe{
#'  \item{min_distance}{A numeric vector giving the minimum Gower distance to any row in `data.y`.}
#'  \item{closest_row_index}{An integer vector giving the index (in `data.y`) to the closest observation.}
#' }
#'
#' @seealso \code{\link{custom_gower}}, \code{\link{calculate_DCR}}
#'
#' @references
#' Gower, J. C. (1971). A General Coefficient of Similarity and Some of Its Properties. \emph{Biometrics}, 27(4), 857-871. doi:10.2307/2528823
#'
#' Kaufman, L., & Rousseeuw, P. J. (1990). \emph{Finding Groups in Data: An Introduction to Cluster Analysis}. Wiley, New York. ISBN 0-471-87876-6. doi:10.2307/2532178
#'
calculate_min_gower <- function(row, data.y = data.x, rngs = NULL, KR.corr = TRUE, na.as.distance = TRUE){
  d = custom_gower(row, as.data.frame(data.y), na.as.distance = na.as.distance, KR.corr = KR.corr)

  min_distance <- apply(d, 1, min, na.rm = TRUE)
  closest_row_index <- apply(d, 1, which.min)

  return(list(min_distance = min_distance, closest_row_index = closest_row_index))
}

#' Calculate Real-to-Real Distance (RRD) for a Single Observation
#'
#' @description
#' Helper function that calls \code{\link{calculate_min_gower}} to compute the Real-to-Real Distance (RRD) for the `i`-th record in a data set.
#' RRD is defined as the minimum Gower distance between one original record and all other records in the same original
#' data set (excluding itself).
#'
#' @param i Integer index of the record in `x` for which the RRD should be computed.
#' @param x A data frame or matrix containing the original data set.
#' @param KR.corr Logical; if \code{TRUE} (default), ordinal variables are scaled using the
#'      correction method of Kaufman & Rousseeuw (1990), where ordinal values are mapped to
#'      the unit interval \eqn{[0, 1]} based on their rank. If \code{FALSE}, the original Gower
#'      (1971) scaling is used where ordinal values are treated the same as numeric values.
#' @param na.as.distance Logical; determines how missing values (\code{NA}) are handled:
#'      \describe{
#'        \item{\code{FALSE}}{Standard Gower behavior: distances involving \code{NA} are excluded from averaging, effectively ignoring missing data.}
#'        \item{\code{TRUE} (default)}{\code{NA} values contribute to the distance computation. Pairs with one missing value are assigned a distance of 1; pairs with both values missing are assigned a distance of 0, and these comparisons count in the average.}
#'        }
#'
#' @details
#' The RRD identifies whether a record is an intrinsic outlier within the original
#' data set. A high RRD indicates that the record has no close real neighbors and may
#' represent an isolated or unique individual. Such records require special attention
#' in privacy evaluations, especially if their SRD (Synthetic-to-Real Distance)
#' is low.
#'
#' Internally calls \code{\link{calculate_min_gower}}. Is called by \code{\link{calculate_DCR}}.
#'
#' @return A list with:
#' \describe{
#'  \item{RRD}{The minimum Gower distance from \code{x[i, ]} to all other records in \code{x}.}
#'  \item{closest_index_orig}{The index of the closest record in the original data set.}
#' }
#'
#' @seealso \code{\link{calculate_DCR}}, \code{\link{calculate_min_gower}}
#'
#' @references
#' Gower, J. C. (1971). A General Coefficient of Similarity and Some of Its Properties. \emph{Biometrics}, 27(4), 857-871. doi:10.2307/2528823
#'
#' Kaufman, L., & Rousseeuw, P. J. (1990). \emph{Finding Groups in Data: An Introduction to Cluster Analysis}. Wiley, New York. ISBN 0-471-87876-6. doi:10.2307/2532178
#'
calc_gow_rrd <- function (i, x, KR.corr = TRUE, na.as.distance = TRUE){
  result <- calculate_min_gower(row = x[i,], data.y = x[-i,], KR.corr = KR.corr, na.as.distance = na.as.distance)
  return(list(RRD = result$min_distance, closest_index_orig = result$closest_row_index))
}

#' Calculate Synthetic-to-Real Distance (SRD) for a Single Observation
#'
#' @description
#' Helper function that calls \code{\link{calculate_min_gower}} to compute the Seal-to-Real Distance (SRD) for the `i`-th record in a data set.
#' SRD is defined as the minimum Gower distance between one original record and all records in the synthetic data set.
#'
#' @param i Integer index of the record in `x` for which the RRD should be computed.
#' @param x A data frame or matrix containing the original data set.
#' @param y A data frame or matrix containing the synthetic data set.
#' @param KR.corr Logical; if \code{TRUE} (default), ordinal variables are scaled using the
#'      correction method of Kaufman & Rousseeuw (1990), where ordinal values are mapped to
#'      the unit interval \eqn{[0, 1]} based on their rank. If \code{FALSE}, the original Gower
#'      (1971) scaling is used where ordinal values are treated the same as numeric values.
#' @param na.as.distance Logical; determines how missing values (\code{NA}) are handled:
#'      \describe{
#'        \item{\code{FALSE}}{Standard Gower behavior: distances involving \code{NA} are excluded from averaging, effectively ignoring missing data.}
#'        \item{\code{TRUE} (default)}{\code{NA} values contribute to the distance computation. Pairs with one missing value are assigned a distance of 1; pairs with both values missing are assigned a distance of 0, and these comparisons count in the average.}
#'        }
#'
#' @details
#' SRD quantifies how closely a synthetic record resembles each real record.
#' Low SRD values indicate that the synthetic data contain a near-duplicate of
#' a real record.
#' When low SRD is combined with high RRD in the real data, this may indicate
#' a privacy concern due to overfitting or direct copying.
#'
#' Internally calls \code{\link{calculate_min_gower}}. Is called by \code{\link{calculate_DCR}}.
#'
#' @return A list with:
#' \describe{
#'  \item{SRD}{The minimum Gower distance from \code{x[i, ]} to all records in \code{y}.}
#'  \item{closest_index_syn}{The index of the closest record in the synthetic data set.}
#' }
#'
#' @seealso \code{\link{calculate_DCR}}, \code{\link{calculate_min_gower}}
#'
#' @references
#' Gower, J. C. (1971). A General Coefficient of Similarity and Some of Its Properties. \emph{Biometrics}, 27(4), 857-871. doi:10.2307/2528823
#'
#' Kaufman, L., & Rousseeuw, P. J. (1990). \emph{Finding Groups in Data: An Introduction to Cluster Analysis}. Wiley, New York. ISBN 0-471-87876-6. doi:10.2307/2532178
#'
calc_gow_srd <- function (i, x, y, KR.corr = TRUE, na.as.distance = TRUE){
  result <- calculate_min_gower(row = x[i,], data.y = y, KR.corr = TRUE, na.as.distance = TRUE)
  return(list(SRD = result$min_distance, closest_index_syn = result$closest_row_index))
}
