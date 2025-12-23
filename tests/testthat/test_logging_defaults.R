test_that("print_nothing logs nothing",{

  expect_output(print_nothing("some type", "some data"),NA)

})

test_that("print_targets only prints target updates",{

  expect_output(print_targets("progress", "\"target\":\"target_column\""),"target:target_column")
  expect_output(print_targets("progress", '"encoding-feature":"target_column"'),NA)

  expect_output(print_targets("progress",sprintf("'target':'%s'",'target')))
  expect_silent(print_targets("progress", '"encoding-feature":"target"'))

})

test_that("print_info only prints info",{

  expect_output(print_info("info",'"encoding-type":"mean encode"'),'\\n"encoding-type":"mean encode"')
  expect_output(print_info("progress", '"encoding-feature":"target_column"'),NA)


  expect_output(print_info("info","'encoding-type':'PCA-reduced'"),"\\n'encoding-type':'PCA-reduced'")

})
