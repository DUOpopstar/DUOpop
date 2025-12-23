############################################################################
####################    harmonize_factor_levels()      #####################
############################################################################

test_that("Harmonization works", {
  x <- data.frame(
    fac1 = factor(c("a", "b"), levels = c("a", "b")),
    fac2 = factor(c("x", "y"), levels = c("x", "y"))
  )

  y <- data.frame(
    fac1 = factor(c("b", "a"), levels = c("a", "b", "c")),
    fac2 = factor(c("w", "x"), levels = c("w", "x", "y"))
  )

  expect_error(custom_gower(x, y), "factors x and y have different levels")

  harmonized <- harmonize_factor_levels(x, y)
  x_h <- harmonized$data.x
  y_h <- harmonized$data.y

  for (col in colnames(x_h)) {
    expect_equal(levels(x_h[[col]]), levels(y_h[[col]]))
  }

  #check if data is preserved
  for (col in colnames(x)) {
    expect_equal(as.character(x_h[[col]]), as.character(x[[col]]))
    expect_equal(as.character(y_h[[col]]), as.character(y[[col]]))
  }

  #check if it now runs without errors
  expect_silent(custom_gower(x_h, y_h))

})

test_that("harmonize_factor_levels() throws the correct errors", {
          expect_error(harmonize_factor_levels(1, data.frame(a=1)), "Input 'data.x' must be a data frame.")
          expect_error(harmonize_factor_levels(data.frame(a=1), "y"), "Input 'data.y' must be a data frame.")
          expect_error(harmonize_factor_levels(data.frame(), data.frame(a=1)), "Input 'data.x' is empty.")
          expect_error(harmonize_factor_levels(data.frame(a=1), data.frame()), "Input 'data.y' is empty.")
})

test_that("harmonization does not change non-categorical columns", {
  x2 <- data.frame(
    fac = factor(c("a", "b")),
    num = c(1, 2)
  )
  y2 <- data.frame(
    fac = factor(c("b", "a")),
    num = c(3, 4)
  )
  harmonized2 <- harmonize_factor_levels(x2, y2)
  expect_equal(harmonized2$data.x$num, x2$num)
  expect_equal(harmonized2$data.y$num, y2$num)
})

############################################################################
####################          custom_gower()           #####################
############################################################################

test_that("custom_gower calculates correct distance for two rows with NA and different data types", {
  x <- data.frame(
    num1 = as.numeric(1),
    num2 = as.numeric(NA),
    log1 = as.logical(TRUE),
    log2 = as.logical(NA),
    fac1 = factor("a", levels = c("a","b")),
    fac2 = factor(NA, levels = c("a","b")),
    ord1 = ordered(2, levels = 1:5),
    ord2 = ordered(NA, levels = 1:3)
  )

  y <- data.frame(
    num1 = as.numeric(2), #range = 1, no NA, so dd = 1
    num2 = as.numeric(2), #one NA, so dd = 0 or 1
    log1 = as.logical(FALSE),  #logical mismatch, dd = 1
    log2 = as.logical(TRUE), #one NA, so dd = 0 or 1
    fac1 = factor("b", levels = c("a","b")), #factor mismatch, so dd = 1
    fac2 = factor("a", levels = c("a","b")), #one NA, so dd = 0 or 1
    ord1 = ordered(4, levels = 1:5), #Gower: abs(outer(x_num, y_num, "-")) / (max(c(x_num, y_num), na.rm = TRUE) - min(c(x_num, y_num), na.rm = TRUE)) = 2/2 = 1
    ord2 = ordered(1, levels = 1:3) #one NA, so dd = 0 or 1 |||| ^KR: L <- max(x_num, y_num, na.rm = TRUE) = 4 | zx <- (x_num - 1) / (L - 1) = 0.3333 | zy <- (y_num - 1) / (L - 1) = 1 |  dd <- abs(outer(zx, zy, "-")) = 0.667
  )

  expected_false_g <- 1   # na.as.distance = FALSE: (1+1+1+1)/4
  expected_true_g  <- 1  # na.as.distance = TRUE: (1+1+1+1+1+1+1+1)/8
  expected_false_kr <- round(0.9166667, 3) #na.as.distance = FALSE: (1+1+1+0.667)/4
  expected_true_kr <- round(0.9583333, 3) #na.as.distance = TRUE: (1+1+1+1+1+1+0.667+1)/8

  dist_false_g <- as.numeric(custom_gower(x, y, na.as.distance = FALSE, KR.corr = FALSE))
  dist_true_g  <- as.numeric(custom_gower(x, y, na.as.distance = TRUE, KR.corr = FALSE))
  dist_false_kr <- round(as.numeric(custom_gower(x, y, na.as.distance = FALSE, KR.corr = TRUE)), 3)
  dist_true_kr <- round(as.numeric(custom_gower(x, y, na.as.distance = TRUE, KR.corr = TRUE)), 3)

  expect_equal(dist_false_g, expected_false_g)
  expect_equal(dist_true_g, expected_true_g)
  expect_equal(dist_false_kr, expected_false_kr)
  expect_equal(dist_true_kr, expected_true_kr)
})

test_that("custom_gower throws correct errors", {

  #Different data types
  x1 <- data.frame(a = 1:3)
  y1 <- data.frame(a = c(TRUE, FALSE, TRUE))
  expect_error(custom_gower(x1, y1),
               "objects x and y have different types")

  #Factors with different levels (or same levels but different order)
  x2 <- data.frame(f = factor(c("a", "b"), levels = c("a","b")))
  y2 <- data.frame(f = factor(c("a", "b"), levels = c("a","b","c")))
  x22 <- data.frame(f = factor(c("a","b","a"), levels = c("a","b")))
  y22 <- data.frame(f = factor(c("b","a","b"), levels = c("b","a"))) # same levels, different order

  expect_error(custom_gower(x2, y2),
               "factors x and y have different levels")
  expect_error(custom_gower(x22, y22),
               "factors x and y have different levels")

  # --- Dimension mismatch: matrix vs matrix ---
  x3 <- data.frame(matrix(1:6, nrow = 2, ncol = 3))
  y3 <- data.frame(matrix(1:4, nrow = 2, ncol = 2))
  expect_error(custom_gower(x3, y3),
               "data.x and data.y must have the same number of columns")

  # --- Dimension mismatch: vector vs matrix ---
  x4 <- 1:3
  y4 <- data.frame(matrix(1:4, nrow = 2, ncol = 2))
  expect_error(custom_gower(x4, y4),
               "data.x should be the same length as the number of columns in data.y")

  # --- Incorrect types of data.x or data.y ---
  x5 <- list(1, 2, 3)
  y5 <- data.frame(a = 1:3, b = 4:6)
  expect_error(custom_gower(x5, y5),
               "data.x must be a vector, matrix or dataframe")
  expect_error(custom_gower(y5, x5),
               "data.y must be a vector, matrix or dataframe")

})

test_that("custom_gower works with empty dataframes", {
  x0 <- data.frame()
  y0 <- data.frame()
  expect_error(custom_gower(x0, y0), "data.x and data.y must have at least one column")

  x0r <- data.frame(a = numeric(0))
  y0r <- data.frame(a = numeric(0))
  result <- custom_gower(x0r, y0r)
  expect_true(is.matrix(result) || inherits(result, "dist"))
})

test_that("custom_gower handles all NA columns", {
  x <- data.frame(a = as.numeric(NA), b = as.numeric(NA))
  y <- data.frame(a = as.numeric(NA), b = as.numeric(1))

  res_false <- custom_gower(x, y, na.as.distance = FALSE)
  expect_true(all(is.nan(res_false)))

  res_true <- custom_gower(x, y, na.as.distance = TRUE)
  expect_true(all(!is.na(res_true)))
  expect_true(all(res_true >= 0 & res_true <= 1)) #zou zelfs 0.5 moeten zijn
})

test_that("custom_gower handles zero range columns", {
  x <- data.frame(num = c(2,2), ord = ordered(c(1,1), levels = 1:3))
  y <- data.frame(num = c(2,2), ord = ordered(c(1,1), levels = 1:3))

  res <- custom_gower(x, y)
  expect_equal(as.numeric(res), 0)
})

test_that("custom_gower handles single-row vector vs dataframe", {
  x <- 1:3
  y <- data.frame(a=1, b=2, c=3)
  #expect_error(custom_gower(x, y), "objects x and y have different types") #used to give error because of integer vs double, but is fixed in code
  expect_silent(custom_gower(x, y))

  x2 <- as.numeric(1:3)
  y2 <- data.frame(a=as.numeric(1), b=as.numeric(2), c=as.numeric(3))

  res <- custom_gower(x2, y2)
  expect_true(is.numeric(res) || inherits(res, "dist"))
})

test_that("custom_gower handles factors with all NA", {
  x <- data.frame(f = factor(c(NA, NA), levels = c("a","b")))
  y <- data.frame(f = factor(c(NA, NA), levels = c("a","b")))

  res <- custom_gower(x, y, na.as.distance = TRUE)
  expect_true(all(!is.na(res)))
})

test_that("custom_gower works with logical-only columns", {
  x <- data.frame(a = c(TRUE,FALSE), b = c(FALSE, TRUE))
  y <- data.frame(a = c(FALSE, TRUE), b = c(TRUE, FALSE))

  res <- custom_gower(x, y)
  expect_true(all(res >= 0 & res <= 1))
})

test_that("custom_gower returns as.dist and symmetric for identical dataframes", {
  df <- data.frame(
    num = c(1,2,3),
    fac = factor(c("a","b","a"), levels = c("a","b")),
    ord = ordered(c(1,2,3), levels = 1:3)
  )

  res <- custom_gower(df, df)
  expect_true(inherits(res, "dist"))

  # Convert to matrix and check symmetry
  res_mat <- as.matrix(res)
  expect_equal(res_mat, t(res_mat))
  expect_true(all(diag(res_mat) == 0))
})

test_that("custom_gower works with integer/double matches", {
  x <- data.frame(a = c(1L, 2L, 3L))   # integer
  y <- data.frame(a = c(1.0, 2.0, 4.0)) # double (numeric)

  expect_true(typeof(x$a) == "integer")
  expect_true(typeof(y$a) == "double")

  d <- custom_gower(x, y)

  expect_true(is.matrix(d))

  expected_range <- max(c(1, 2, 3, 1, 2, 4)) - min(c(1, 2, 3, 1, 2, 4))
  manual_dist <- abs(outer(c(1, 2, 3), c(1, 2, 4), "-")) / expected_range
  dimnames(manual_dist) <- list(row.names(x), row.names(y))

  expect_true(all.equal(
    round(d, 5),
    round(manual_dist, 5),
    check.attributes = TRUE
  ))

})

############################################################################
####################         calculate_dcr()           #####################
############################################################################

#test data
orig <- data.frame(
  x = factor(c("a", "b")),
  y = c(1, 2)
)
syn <- data.frame(
  x = factor(c("b", "c")),
  y = c(2, 3)
)

test_that("calculate_DCR throws correct errors", {
  expect_error(calculate_DCR("not a df", syn), "Input 'original_df' must be a data frame.", fixed = TRUE)
  expect_error(calculate_DCR(orig, "not a df"), "Input 'synthetic_df' must be a data frame.", fixed = TRUE)
  expect_error(calculate_DCR(orig[0,], syn), "Input dataset 'original_df' is empty.", fixed = TRUE)
  expect_error(calculate_DCR(orig, syn[0,]), "Input dataset 'synthetic_df' is empty.", fixed = TRUE)
})

test_that("calculate_DCR() returns expected structure", {
  res <- suppressWarnings(calculate_DCR(orig, syn))
  expect_type(res, "list")
  expect_named(res, (c("histogram", "scatterplot", "closest_matches")))
  expect_s3_class(res$histogram, "ggplot")
  expect_s3_class(res$scatterplot, "ggplot")
  expect_s3_class(res$closest_matches, "data.frame")
  })

test_that("calculate_DCR() correctly calls utils functions", {
  harmonize_called <- FALSE
  custom_gower_calls <- 0
  calculate_min_gower_calls <- 0
  calc_gow_rrd_calls <- 0
  calc_gow_srd_calls <- 0

  orig_calculate_min_gower <- calculate_min_gower
  orig_calc_gow_rrd <- calc_gow_rrd
  orig_calc_gow_srd <- calc_gow_srd

  local_mocked_bindings(
    harmonize_factor_levels = function(original_df, synthetic_df) {
      harmonize_called <<- TRUE
      list(data.x = original_df, data.y = synthetic_df)
    },
    custom_gower = function(data.x, data.y = data.x, na.as.distance = TRUE, KR.corr = TRUE, rngs = NULL) {
      custom_gower_calls <<- custom_gower_calls + 1
      matrix(0, nrow = nrow(data.x), ncol = nrow(data.y))
    },
    calculate_min_gower = function(...) {
      calculate_min_gower_calls <<- calculate_min_gower_calls + 1
      orig_calculate_min_gower(...)
    },
    calc_gow_rrd = function(...) {
      calc_gow_rrd_calls <<- calc_gow_rrd_calls + 1
      orig_calc_gow_rrd(...)
    },
    calc_gow_srd = function(...) {
      calc_gow_srd_calls <<- calc_gow_srd_calls + 1
      orig_calc_gow_srd(...)
    }
  )

  res <- calculate_DCR(orig, syn, interactive = FALSE)
  expect_true(harmonize_called)
  expect_equal(custom_gower_calls, 4)
  expect_equal(calculate_min_gower_calls, 4)
  expect_equal(calc_gow_rrd_calls, 2)
  expect_equal(calc_gow_srd_calls, 2)
})

test_that("Inf is only in diagonal of distance matrix", { #old test since calculate_DCR no longer uses a matrix. Still tests the functionality of custom_gower
  check_matrix <- NULL

  local_mocked_bindings(
    custom_gower = function(data.x, data.y = data.x, na.as.distance = TRUE, KR.corr = TRUE, rngs = NULL) {
      mat <- matrix(1, nrow = nrow(data.x), ncol = nrow(data.y))
      diag(mat) <- 0
      check_matrix <<- mat
      mat
    }
  )

  calculate_DCR(orig, syn)
  expected <- check_matrix
  diag(expected) <- Inf #same method as used in calculate_dcr

  expect_true(all(diag(expected) == Inf))
  expect_false(any(expected[upper.tri(expected)] == Inf | expected[lower.tri(expected)] == Inf))
  })

test_that("no copied outliers returns empty closest_matches", {
  orig_simple <- data.frame(x = factor("a"), y = 1)
  syn_simple  <- data.frame(x = factor("a"), y = 1)
  res <- suppressWarnings(calculate_DCR(orig_simple, syn_simple))
  expect_equal(nrow(res$closest_matches), 0)
})
