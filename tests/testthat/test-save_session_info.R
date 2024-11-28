### Tests voor save_session_info()

### Happy Path:
# Correct input: verifies that the function saves the correct
# session-information to a logfile with a timestamp in the filename.

test_that("the function saves a logfile if the input is correct", {
  # specify path
  path <- temp_dir <- gsub("\\", "/", tempdir(), fixed = TRUE)
  if (!grepl("/$", temp_dir)) {temp_dir <- paste0(temp_dir, "/")}

  # specify file_name
  file_name <- "version2.0"

  # Generate a timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # Add timestamp to the file name
  modified_file_name <- paste0(file_name, "_", timestamp, ".txt")

  # modified path
  modifiedpath <- paste0(path, "/", modified_file_name)

  # run save_session_info()
  save_session_info(file_name = file_name, path = path)

  # Check if the file exists in the correct location
  expect_true(file.exists(modifiedpath), info = paste("File does not exsits in:", modifiedpath))

}
)


### Edge Case:
# Empty path: test that the function results in an error message if
# no logfile is specified

test_that("no path gives an error message", {
  expect_error(save_session_info(file_name = "version2.0"),
               regexp = "is missing")
}
)



### Error Handling
# Path does not exsist: tests that the functions results in an
# errormessage if the logfile does not exsists.

test_that(" that the functions results in an errormessage if the logfile does not exsists.", {
  expect_error(save_session_info(file_name = "version2.0", path  = "a"),
               regexp = "specified path does not exist")
}
)

