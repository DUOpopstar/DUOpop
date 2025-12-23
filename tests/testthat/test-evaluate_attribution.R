############################################################################
####################       evaluate_attribution()      #####################
############################################################################

test_that("Correct output structure without holdout_df", {
  orig_df <- data.frame(age = c(30, 40, 30), sex = c("M", "F", "M"), income = c("low", "high", "high"))
  syn_df  <- data.frame(age = c(30, 40, 30), sex = c("M", "F", "M"), income = c("low", "high", "low"))

  local_mocked_bindings(
    calculate_basecap = function(...) 0.1, # not correct but not important for testing the structure
    calculate_capo    = function(...) 0.3, #
    calculate_caps    = function(...) 0.2, #
  )

  result <- evaluate_attribution(orig_df, syn_df, keys = c("age", "sex"), target = "income")

  expect_named(result, c("BaseCAP", "CAPo", "CAPs", "Abs_Reduction", "Rel_Reduction", "Normalized_Risk", "Risk_Plot"))
  expect_equal(result$Abs_Reduction, 0.3 - 0.2)
  expect_equal(result$Rel_Reduction, (0.3 - 0.2)/0.3)
  expect_equal(result$Normalized_Risk, (0.2-0.1)/(0.3-0.1))
})

test_that("Correct output structure with holdout_df", {

  orig_df <- data.frame(age = c(30, 40), sex = c("M", "F"), income = c("low", "high"))
  syn_df  <- orig_df
  holdout_df <- orig_df
  train_df <- orig_df

  local_mocked_bindings(
    calculate_basecap = function(...) 0.1, #not the correct numbers but not important for this test
    calculate_capo    = function(...) 0.3,
    calculate_caps    = function(...) 0.2,
    calculate_hcaps   = function(...) 0.15
  )

  result <- evaluate_attribution(orig_df, syn_df, keys = c("age", "sex"), target = "income", train_df, holdout_df)

  expect_named(result, c("BaseCAP", "CAPo", "CAPs", "hCAPs", "Upperbound_Attr", "Undesired_Attr", "Abs_Attr_Reduction", "Rel_Attr_Reduction", "Risk_Plot"))
  expect_equal(result$Upperbound_Attr, 0.3 - 0.15)
  expect_equal(result$Undesired_Attr, 0.2 - 0.15)
  expect_equal(result$Abs_Attr_Reduction, (0.3 - 0.15) - (0.2 - 0.15))
  expect_equal(result$Rel_Attr_Reduction, 1 - (0.2 - 0.15) / (0.3 - 0.15))
})

test_that("Edge case: CAPo equals hCAPs", {
  orig_df <- data.frame(age = 1, sex = "M", income = "high")
  syn_df  <- orig_df
  holdout_df <- orig_df
  train_df <- orig_df

  local_mocked_bindings(
    calculate_basecap = function(...) 0.2,
    calculate_capo    = function(...) 0.8,
    calculate_caps    = function(...) 0.8,
    calculate_hcaps   = function(...) 0.8,
  )

  result <- evaluate_attribution(orig_df, syn_df, c("age", "sex"), "income",train_df, holdout_df)

  expect_equal(result$Rel_Attr_Reduction, 0)
  expect_equal(result$Abs_Attr_Reduction, 0)
})

test_that("Edge case: CAPo equals BaseCAP", {
  orig_df <- data.frame(age = 1, sex = "M", income = "high")
  syn_df  <- orig_df

  local_mocked_bindings(
    calculate_basecap = function(...) 0.3,
    calculate_capo    = function(...) 0.3,
    calculate_caps    = function(...) 0.2
  )

  result <- evaluate_attribution(orig_df, syn_df, c("age", "sex"), "income")

  expect_equal(result$Normalized_Risk, 0)
})

# ──────────────────────────────
# Error handling
# ──────────────────────────────
test_that("evaluate_attribtion() throws correct errors for invalid input", {
  # 1. orig_data is no data.frame
  expect_error(
    evaluate_attribution(orig_df = list(q = 1:5), syn_df = data.frame(q = c(1:5), income = c(1:5)), keys = "q", target = "income"),
    regexp = "Input data must be a data frame: 'orig_df'\\."
  )

  # 2. Empty synthetic data set
  expect_error(
    evaluate_attribution(orig_df = data.frame(q = c(1:5), income = c(1:5)), syn_df = data.frame(), keys = "q", target = "income"),
    regexp = "Input data set 'syn_df' is empty\\."
  )

  # 3. Target column missing in holdout_df
  df_missing_target <- data.frame(q = 1:5)
  expect_error(
    evaluate_attribution(orig_df = data.frame(q = 1:5, income = 1:5), syn_df = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income", train_df = data.frame(q = c(1:5), income = c(1:5)), holdout_df = data.frame(q = 1:5)),
    regexp = "Target column 'income' is not present in data set 'holdout_df'\\."
  )

  # 4. Target column contains only NAs or empty strings (in orig_df)
  df_only_na_or_empty <- data.frame(q = rep("A", 5), income = c(NA, NA, "", NA, ""))
  expect_error(
    evaluate_attribution(orig_df = df_only_na_or_empty, syn_df = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "Target column 'income' in data set 'orig_df' contains only NA or empty string values\\."
  )

  # 5. Key column missing (in syn_df)
  df_missing_key <- data.frame(income = 1:5)
  expect_error(
    evaluate_attribution(orig_df = data.frame(q = 1:5, income = 1:5), syn_df = df_missing_key, keys = "q", target = "income"),
    regexp = "One or more key columns are missing in data set 'syn_df'\\."
  )

  # 6. Key column contains only NAs or empty strings (in train_df)
  df_key_only_na_or_empty <- data.frame(q = c(NA, NA, "", NA, ""), income = 1:5)
  expect_error(
    evaluate_attribution(orig_df = data.frame(q = 1:5, income = 1:5), syn_df = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income", train_df = df_key_only_na_or_empty, holdout_df = data.frame(q = c(1:5), income = c(1:5))),
    regexp = "One or more key columns in data set 'train_df' contain only NA or empty string values\\."
  )

  # 7. Only train_df is supplied and not holdout_df
  expect_error(
    evaluate_attribution(orig_df = data.frame(q = c(1:5), income = c(1:5)), syn_df = data.frame(q = c(1:5), income = c(1:5)), keys = "q", target = "income", train_df = data.frame(q = c(1:5), income = c(1:5))),
    regexp = "If you provide a train_df, you must also provide a holdout_df\\."
  )

  # 8. Only a holdout_df is supplied and not a train_df
  expect_error(
    evaluate_attribution(orig_df = data.frame(q = c(1:5), income = c(1:5)), syn_df = data.frame(q = c(1:5), income = c(1:5)), keys = "q", target = "income", holdout_df = data.frame(q = c(1:5), income = c(1:5))),
    regexp = "If you provide a holdout_df, you must also provide a train_df\\."
  )
})

############################################################################
####################        calculate_basecap()        #####################
############################################################################

test_that("Correct calculation on known distribution of factor target", {
  df <- data.frame(target = c("A", "A", "B", "B", "B", "C"))
  expected <- (2/6)^2 + (3/6)^2 + (1/6)^2
  result <- calculate_basecap(df, "target")
  expect_equal(result, expected)
})

test_that("Works with numeric target", {
  df <- data.frame(target = c(1, 1, 2, 2, 3))
  expected <- (2/5)^2 + (2/5)^2 + (1/5)^2
  result <- calculate_basecap(df, "target")
  expect_equal(result, expected)
})

test_that("BaseCAP excludes NA when na_as_value = FALSE", {
  df <- data.frame(target = c("A", "A", "B", "B", NA, NA))
  expected <- (2/6)^2 + (2/6)^2
  result <- calculate_basecap(df, "target", na_as_value = FALSE)
  expect_equal(result, expected)
})

test_that("BaseCAP includes NA when na_as_value = TRUE", {
  df <- data.frame(target = c("A", "A", "B", "B", NA, NA))
  expected <- (2/6)^2 + (2/6)^2 + (2/6)^2
  result <- calculate_basecap(df, "target", na_as_value = TRUE)
  expect_equal(result, expected)
})

test_that("Empty string is treated as a value", {
  df <- data.frame(target = c("", "", "A", "A", "B"))
  expected <- (2/5)^2 + (2/5)^2 + (1/5)^2
  result <- calculate_basecap(df, "target")
  expect_equal(result, expected)
})

test_that("Edge case: BaseCAP = 1 when only one unique target value", {
  df <- data.frame(target = rep("X", 10))
  result <- calculate_basecap(df, "target")
  expect_equal(result, 1)
})

test_that("Edge case: BaseCAP = 1/n when all values unique", {
  df <- data.frame(target = paste0("v", 1:10))
  result <- calculate_basecap(df, "target")
  expect_equal(result, 10 * (1/10)^2)
})

# ──────────────────────────────
# Error handling
# ──────────────────────────────
test_that("calculate_basecap() throws correct errors for invalid input", {
  # 1. holdout_data is no data.frame
  expect_error(
    calculate_basecap(orig_data = list(q = 1:5), target = "income"),
    regexp = "Input data must be a data frame: 'orig_data'\\."
  )

  # 1. Empty data set
  expect_error(
    calculate_basecap(data.frame(), "income"),
    regexp = "Input data set 'orig_data' is empty\\."
  )

  # 2. Target column missing
  df_missing_target <- data.frame(age = 1:5)
  expect_error(
    calculate_basecap(df_missing_target, "income"),
    regexp = "Target column 'income' is not present in data set 'orig_data'\\."
  )

  # 3. Target column contains only NAs or empty strings
  df_only_na_or_empty <- data.frame(income = c(NA, NA, "", NA, ""))
  expect_error(
    calculate_basecap(df_only_na_or_empty, "income"),
    regexp = "Target column 'income' in data set 'orig_data' contains only NA or empty string values\\."
  )
})

############################################################################
####################         calculate_capo()          #####################
############################################################################

test_that("Correct calculation with na_as_value = FALSE/TRUE", {
  df <- data.frame(
    q = c("A", "A", NA, "B", "B", NA),
    t = c("X", NA, "X", "Y", "Y", NA)
  )

  result_false <- calculate_capo(df, keys = "q", target = "t", na_as_value = FALSE)
  expected_false <- (1*0.5 + 1*0 + 2*1 + 1*0 + 1*0)/6 #2.5/6

  result_true <- calculate_capo(df, keys = "q", target = "t", na_as_value = TRUE)
  expected_true <-  (1*0.5 + 1*0.5 + 2*1 + 1*0.5 + 1*0.5)/6 #4/6

  expect_equal(result_false, expected_false)
  expect_equal(result_true, expected_true)
})

test_that("Empty string is treated as a value", {
  df <- data.frame(
    q = c("A", "A", "", "B", "B", ""),
    t = c("X", "", "X", "Y", "Y", "")
  )
  result <- calculate_capo(df, keys = "q", target = "t")
  expected <- (1*0.5 + 1*0.5 + 2*1 + 1*0.5 + 1*0.5)/6
  expect_equal(result, expected)
})

test_that("Edge case: calculate_capo() returns 1 for constant target", {
  df <- data.frame(q = c(1,1,2,2), t = rep("A", 4))
  expect_equal(calculate_capo(df, keys = "q", target = "t"), 1)
})

test_that("Edge case: calculate_capo() returns 1 when all (q,t) combos occur once", {
  df <- data.frame(
    q = 1:5,
    t = letters[1:5]
  )
  expect_equal(calculate_capo(df, keys = "q", target = "t"), 1)
})

test_that("Edge case: calculate_capo() returns 1 when data set has one row", {
  df <- data.frame(q = "A", t = "B")
  expect_equal(calculate_capo(df, keys = "q", target = "t"), 1)
})

# ──────────────────────────────
# Error handling
# ──────────────────────────────
test_that("calculate_capo() throws correct errors for invalid input", {
  # 1. orig_data is no data.frame
  expect_error(
    calculate_capo(orig_data = list(q = 1:5), keys = "q", target = "income"),
    regexp = "Input data must be a data frame: 'orig_data'\\."
  )

  # 2. Empty data set
  expect_error(
    calculate_capo(data.frame(), keys = "q", target = "income"),
    regexp = "Input data set 'orig_data' is empty\\."
  )

  # 3. Target column missing
  df_missing_target <- data.frame(q = 1:5)
  expect_error(
    calculate_capo(df_missing_target, keys = "q", target = "income"),
    regexp = "Target column 'income' is not present in data set 'orig_data'\\."
  )

  # 4. Target column contains only NAs or empty strings
  df_only_na_or_empty <- data.frame(q = rep("A", 5), income = c(NA, NA, "", NA, ""))
  expect_error(
    calculate_capo(df_only_na_or_empty, keys = "q", target = "income"),
    regexp = "Target column 'income' in data set 'orig_data' contains only NA or empty string values\\."
  )

  # 5. Key column missing
  df_missing_key <- data.frame(income = 1:5)
  expect_error(
    calculate_capo(df_missing_key, keys = "q", target = "income"),
    regexp = "One or more key columns are missing in data set 'orig_data'\\."
  )

  # 6. Key column contains only NAs or empty strings
  df_key_only_na_or_empty <- data.frame(q = c(NA, NA, "", NA, ""), income = 1:5)
  expect_error(
    calculate_capo(df_key_only_na_or_empty, keys = "q", target = "income"),
    regexp = "One or more key columns in data set 'orig_data' contain only NA or empty string values\\."
  )
})

############################################################################
####################         calculate_caps()          #####################
############################################################################

test_that("Correct calculation with na_as_value = FALSE/TRUE", {
  orig <- data.frame(
    q = c("A", "A", NA, "B", "B", NA),
    t = c("X", NA, "X", "Y", "Y", NA)
  )

  syn <- data.frame(
    q = c("A", "A", "B", "B", NA, NA),
    t = c("X", "X", "Y", NA, "Y", NA)
  )

  result_false <- calculate_caps(orig, syn, keys = "q", target = "t", na_as_value = FALSE)
  expected_false <-  1/3
  # (A, X) CAP_qt: 2/2, n_qt_orig/N_orig = 1/6 -> 2/2 * 1/6 = 1/6
  # (A, NA) CAP_qt: 0
  # (NA, X) CAP_qt: 0
  # (B, Y) CAP_qt: 1/2, n_qt_orig/N_orig = 2/6 -> 1/1 * 2/6 = 1/6
  # (NA, NA) CAP_qt = 0
  # CAPs = 1/6 + 0 + 0 + 2/6 + 0 = 0.5

  result_true <- calculate_caps(orig, syn, keys = "q", target = "t", na_as_value = TRUE)
  expected_true <- 5/12
  # (A, X) CAP_qt: 2/2, n_qt_orig/N_orig = 1/6 -> 2/2 * 1/6 = 1/6
  # (A, NA) CAP_qt: 0/2
  # (NA, X) CAP_qt: 0/2
  # (B, Y): CAP_qt: 1/2, n_qt_orig/N_orig = 2/6 -> 1/2 * 2/6 = 1/6
  # (NA, NA): CAP_qt: 1/2, n_qt_orig/N_orig = 1/6 -> 1/2 * 1/6 = 1/12
  # CAPs = 1/6 + 0 + 0 + 1/6 + 1/12 = 5/12

  expect_equal(result_false, expected_false)
  expect_equal(result_true, expected_true)

})

test_that("Empty string is treated as value", {
  orig <- data.frame(
    q = c("A", "A", "", "B", "B", ""),
    t = c(1, "", 1, 2, 2, "")
  )

  syn <- data.frame(
    q = c("A", "A", "B", "B", "", ""),
    t = c(1, 1, 2, "", 2, "")
  )

  result <- calculate_caps(orig, syn, keys = "q", target = "t")
  expected <- 5/12
  expect_equal(result, expected)

})

test_that("Edge case: calculate_caps() returns 0 when (q,t) combos in orig missing in syn", {
  orig <- data.frame(q = c("A", "B"), t = c("X", "Y"))
  syn <- data.frame(q = c("A","B"), t = c("Y", "X"))

  result <- calculate_caps(orig, syn, keys = "q", target = "t")

  expect_true(result == 0)
})

test_that("Edge case: original and synthetic data set are identical", {
  orig <- data.frame(q = c("A", "A", "B"), t = c("X", "Y", "Y"))

  result_caps <- calculate_caps(orig, orig, keys = "q", target = "t", na_as_value = FALSE)
  result_capo <- calculate_capo(orig, keys = "q", target = "t", na_as_value = FALSE)

  expect_equal(result_caps, result_capo)
})

# ──────────────────────────────
# Error handling
# ──────────────────────────────
test_that("calculate_caps() throws correct errors for invalid input", {
  # 1. orig_data is no data.frame
  expect_error(
    calculate_caps(orig_data = list(q = 1:5), syn_data = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "Input data must be a data frame: 'orig_data'\\."
  )

  # 2. Empty syn_data
  expect_error(
    calculate_caps(orig_data = data.frame(q = 1:5, income = 1:5), syn_data = data.frame(), keys = "q", target = "income"),
    regexp = "Input data set 'syn_data' is empty\\."
  )

  # 3. Target column missing in orig_data
  df_missing_target <- data.frame(q = 1:5)
  expect_error(
    calculate_caps(orig_data = df_missing_target, syn_data = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "Target column 'income' is not present in data set 'orig_data'\\."
  )

  # 4. Target column only NA or "" in syn_data
  df_only_na_or_empty_syn <- data.frame(q = rep("A", 5), income = c(NA, NA, "", NA, ""))
  expect_error(
    calculate_caps(orig_data = data.frame(q = 1:5, income = 1:5), syn_data = df_only_na_or_empty_syn, keys = "q", target = "income"),
    regexp = "Target column 'income' in data set 'syn_data' contains only NA or empty string values\\."
  )

  # 5. Key column missing in orig_data
  df_missing_key <- data.frame(income = 1:5)
  expect_error(
    calculate_caps(orig_data = df_missing_key, syn_data = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "One or more key columns are missing in data set 'orig_data'\\."
  )

  # 6. Key column only NA or "" in syn_data
  df_key_only_na_or_empty_syn <- data.frame(q = c(NA, NA, "", NA, ""), income = 1:5)
  expect_error(
    calculate_caps(orig_data = data.frame(q = 1:5, income = 1:5), syn_data = df_key_only_na_or_empty_syn, keys = "q", target = "income"),
    regexp = "One or more key columns in data set 'syn_data' contain only NA or empty string values\\."
  )
})

############################################################################
####################         calculate_hcaps()         #####################
############################################################################

test_that("Correct calculation with na_as_value = FALSE", {
  holdout <- data.frame(
    q = c("A", "A", NA, "B", "B", NA),
    t = c("X", NA, "X", "Y", "Y", NA)
  )

  syn <- data.frame(
    q = c("A", "A", "B", "B", NA, NA),
    t = c("X", "X", "Y", NA, "Y", NA)
  )

  result_false <- calculate_hcaps(holdout, syn, keys = "q", target = "t", na_as_value = FALSE)
  expected_false  <- 1/3

  result_true <- calculate_hcaps(holdout, syn, keys = "q", target = "t", na_as_value = TRUE)
  expected_true <- 5/12

  expect_equal(result_false, expected_false)
  expect_equal(result_true, expected_true)

})


test_that("Empty string is treated as value", {
  holdout <- data.frame(
    q = c("A", "A", "", "B", "B", ""),
    t = c(1, "", 1, 2, 2, "")
  )

  syn <- data.frame(
    q = c("A", "A", "B", "B", "", ""),
    t = c(1, 1, 2, "", 2, "")
  )

  result_true <- calculate_hcaps(holdout, syn, keys = "q", target = "t")
  expected <- 5/12
  expect_equal(result_true, expected)

})

test_that("Edge case: calculate_hcaps() returns 0 when (q,t) combos in holdout missing in syn", {
  holdout <- data.frame(q = c("A", "B"), t = c("X", "Y"))
  syn <- data.frame(q = c("A","B"), t = c("Y", "X"))

  result <- calculate_hcaps(holdout, syn, keys = "q", target = "t")

  expect_true(result == 0)
})

test_that("Edge case: original, synthetic , and holdout data sets are identical", {
  data <- data.frame(q = c("A", "A", "B"), t = c("X", "Y", "Y"))

  result_capo <- calculate_capo(data, keys = "q", target = "t")
  result_caps <- calculate_caps(data, data, keys = "q", target = "t")
  result_hcaps <- calculate_hcaps(data, data, keys = "q", target = "t")

  expect_equal(result_capo, result_caps)
  expect_equal(result_capo, result_hcaps)
  expect_equal(result_hcaps, result_caps)
})

# ──────────────────────────────
# Error handling
# ──────────────────────────────
test_that("calculate_hcaps() throws correct errors for invalid input", {
  # 1. holdout_data is no data.frame
  expect_error(
    calculate_hcaps(holdout_data = list(q = 1:5), syn_data = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "Input data must be a data frame: 'holdout_data'\\."
  )

  # 2. Empty syn_data
  expect_error(
    calculate_hcaps(holdout_data = data.frame(q = 1:5, income = 1:5), syn_data = data.frame(), keys = "q", target = "income"),
    regexp = "Input data set 'syn_data' is empty\\."
  )

  # 3. Target column missing in holdout_data
  df_missing_target <- data.frame(q = 1:5)
  expect_error(
    calculate_hcaps(holdout_data = df_missing_target, syn_data = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "Target column 'income' is not present in data set 'holdout_data'\\."
  )

  # 4. Target column only NA or "" in syn_data
  df_only_na_or_empty_syn <- data.frame(q = rep("A", 5), income = c(NA, NA, "", NA, ""))
  expect_error(
    calculate_hcaps(holdout_data = data.frame(q = 1:5, income = 1:5), syn_data = df_only_na_or_empty_syn, keys = "q", target = "income"),
    regexp = "Target column 'income' in data set 'syn_data' contains only NA or empty string values\\."
  )

  # 5. Key column missing in holdout_data
  df_missing_key <- data.frame(income = 1:5)
  expect_error(
    calculate_hcaps(holdout_data = df_missing_key, syn_data = data.frame(q = 1:5, income = 1:5), keys = "q", target = "income"),
    regexp = "One or more key columns are missing in data set 'holdout_data'\\."
  )

  # 6. Key column only NA or "" in syn_data
  df_key_only_na_or_empty_syn <- data.frame(q = c(NA, NA, "", NA, ""), income = 1:5)
  expect_error(
    calculate_hcaps(holdout_data = data.frame(q = 1:5, income = 1:5), syn_data = df_key_only_na_or_empty_syn, keys = "q", target = "income"),
    regexp = "One or more key columns in data set 'syn_data' contain only NA or empty string values\\."
  )
})

############################################################################
####################      helper: make_na_value()      #####################
############################################################################

test_that("make_na_value() keeps data type", {
  numeric_col <- c(1.5, NA, 3.2)
  integer_col <- c(1L, NA_integer_, 3L)
  logical_col <- c(TRUE, NA, FALSE)
  character_col <- c("a", NA, "b")
  factor_col <- factor(c("x", NA, "y"))

  # Numeric
  new_numeric <- make_na_value(numeric_col)
  expect_true(is.numeric(new_numeric))
  expect_equal(new_numeric[2], NA_real_)

  # Integer
  new_integer <- make_na_value(integer_col)
  expect_true(is.integer(new_integer))
  expect_equal(new_integer[2], NA_integer_)

  # Logical
  new_logical <- make_na_value(logical_col)
  expect_true(is.logical(new_logical))
  expect_equal(new_logical[2], NA)

  # Character
  new_character <- make_na_value(character_col)
  expect_true(is.character(new_character))
  expect_equal(new_character[2], "<NA>")

  # Factor
  new_factor <- make_na_value(factor_col)
  expect_true(is.factor(new_factor))
  expect_equal(levels(new_factor), c("x", "y", "<NA>"))
  expect_equal(as.character(new_factor[2]), "<NA>")
})
