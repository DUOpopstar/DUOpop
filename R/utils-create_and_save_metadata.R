# =====================================================
# Helper Function: get_dictionary ----
# =====================================================
#' @title Data Type Translation
#' @description Creates a vector of language specific data type names.
#'
#' @param lang Language in which the data types need to be saved. Either "en" for English or "nl" for Dutch.
#' @keywords internal
#' @return Vector with translation of data types.
get_datatype_translation<-function(lang){
  switch(lang,
         "en"=dict_r_type_english,
         "nl"=dict_r_type_dutch
  )
}

# =====================================================
# Helper Function: write_to_excel ----
# =====================================================
#' @title write to excel
#' @description wrapper of write_xlsx to enable unit testing
#' @keywords internal
#' @return nothing
write_to_excel<-function(data,path) write_xlsx(data,path)


# =====================================================
# Helper Function: extract_names_and_types ----
# =====================================================
#' @title extract names and types
#' @description extract the names of the fields and their datatypes from 'data'
#' @param data The data to extract the names and types of.
#' @keywords internal
#' @return nothing
extract_names_and_types <- function(data,lang="en")
  tibble(
    Variable = names(data) %>% tools::toTitleCase()
    ,`Data type` = sapply(data,function (x)(get_datatype_translation(lang)[class(x)])) %>% as.vector
  )


dict_r_type_english <-
  c(
    "factor" = "Categorical"
    ,"integer" = "Integer"
    ,"numeric" = "Numeric"
  )


dict_r_type_dutch <-
  c(
    "factor" = "Categorisch"
    ,"integer" = "Natuurlijk getal"
    ,"numeric" = "Numeriek"
  )
