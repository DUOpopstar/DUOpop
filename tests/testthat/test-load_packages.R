# =====================================================
# Happy Path Tests for Main Function load_packages
# =====================================================

test_that("load_packages installs and loads packages", {

  result <- load_packages("tidyverse", "dplyr", "tidyr")

  expect_true("tidyverse" %in% result$loaded)
  expect_true("dplyr" %in% result$loaded)
  expect_true("tidyr" %in% result$loaded)

  expect_length(result$failed, 0)

  expect_true("tidyverse" %in% .packages())
  expect_true("dplyr" %in% .packages())
  expect_true("tidyr" %in% .packages())

})

# =====================================================
# Error Handling Tests for Main Function load_packages
# =====================================================

test_that("Non existent package is not installed", {
  result <- suppressWarnings(load_packages(nonexistentpackage123))

  expect_false("nonexistentpackage123" %in% result$loaded)
  expect_true("nonexistentpackage123" %in% result$failed)

})

# =====================================================
# Happy Path Tests for Main Function load_packages
# =====================================================
