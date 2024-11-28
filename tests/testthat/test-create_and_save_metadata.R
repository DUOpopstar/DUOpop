# =====================================================
# Happy Path Tests for Main Function ----
# =====================================================
test_that("metadata is written to xlsx file",{
  test_data = data.frame(
    full_lower_case=c(1,2,3,2,1)%>% as.integer()
    ,ThisIsAFactor=factor(c("A","B","C","B","A"))
    ,numeric_data=c(1.1,2.2,3.3,2.2,1.1)
  )

  extract_is_called <-FALSE
  write_is_called<-FALSE


  with_mocked_bindings(
    create_and_save_metadata("/path/to/file.xlsx",test_data),
    extract_names_and_types =function(data,lang){
      extract_is_called <<- TRUE
      expect_equal(data,test_data)
      expect_equal(lang,"nl")
      return(tibble(variable=c("a")))
    },
    write_to_excel = function(data,path){
      write_is_called<<-TRUE
      expect_named(data,c("variable"))
      expect_equal(path,"/path/to/file.xlsx")

    }
  )

  expect_true(extract_is_called)
  expect_true(write_is_called)
})

# =====================================================
# Tests for Helper Functions ----
# =====================================================

## Helper Function 1: extract_names_and_types ----

### Happy Path Tests: english output ----
test_that("datatype and names are extracted from dataframe", {
  #expected input is a dataframe
  test_data = data.frame(
    full_lower_case=c(1,2,3,2,1)%>% as.integer()
    ,ThisIsAFactor=factor(c("A","B","C","B","A"))
    ,numeric_data=c(1.1,2.2,3.3,2.2,1.1)
  )

  result <- extract_names_and_types(test_data)

  expected_result <- tibble(
    Variable=c("Full_lower_case", "ThisIsAFactor","Numeric_data")
    ,`Data type` = c("Integer","Categorical","Numeric")
  )

  expect_equal(result,expected_result)

})

### Happy Path Tests: Dutch output----
test_that("datatype and names are extracted from dataframe with dutch data types", {
  #expected input is a dataframe
  test_data = data.frame(
    full_lower_case=c(1,2,3,2,1)%>% as.integer()
    ,ThisIsAFactor=factor(c("A","B","C","B","A"))
    ,numeric_data=c(1.1,2.2,3.3,2.2,1.1)
  )

  result <- extract_names_and_types(test_data,lang="nl")

  expected_result <- tibble(
    Variable=c("Full_lower_case", "ThisIsAFactor","Numeric_data")
    ,`Data type` = c("Natuurlijk getal","Categorisch","Numeriek")
  )

  expect_equal(result,expected_result)

})
