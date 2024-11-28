#' @title create and save metadata
#' @description Creates a tibble for the metadata, based on the datatype of 'data'.
#'  It also saves it to an excel file.
#' @param data the data of which the metadata needs to be saved
#' @param file_path the file to save the metadata to.
#'
#' @return nothing
#' @export

#' @examples
#' \dontrun{
#' test_data = data.frame(
#' full_lower_case=c(1,2,3,2,1)%>% as.integer()
#' ,ThisIsAFactor=factor(c("A","B","C","B","A"))
#' ,numeric_data=c(1.1,2.2,3.3,2.2,1.1)
#' )
#' create_and_save_metadata("/path/to/file.xlsx",test_data)
#' }
#'
#'
#' @importFrom writexl write_xlsx
#' @import tibble
#'
#'
create_and_save_metadata <- function(file_path,data) {

  extract_names_and_types(data,lang="nl")%>%
    write_to_excel(file_path)
}
