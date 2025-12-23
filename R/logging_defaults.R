
print_nothing <- function(type, data){

}

#'@title print targets
#'@description prints the target when encoding or synthesising.
#'@importFrom jsonlite fromJSON
print_targets <- function(type, data){

  if(type == "progress"){
    l <- fromJSON(sprintf("[{%s}]",gsub("'","\"",data)))
    if ("target" %in% names(l))
    {
      print(sprintf("target:%s",l$target))
    }

  }
}

#'@title print info
#'@description prints info about the encoding and synthesis process
#'This can be things such as which encoding and parameters are used.
print_info <- function(type,data){

  if(type == "info"){
    l <- fromJSON(sprintf("[{%s}]",gsub("'","\"",data)))
    cat(sprintf("\n%s",data))
    #print(sprintf("encoding-type:%s",l$`encoding-type`))
  }

}

#'@title print performance
#'@description prints messages about the performance of the synthesis process
#'This can be things such as the time (in minutes) it takes to synthesise a column,
#' or how effective PCA was for a feature-target combination.
print_performance <- function(type,data){
  if (type == "performance"){
    cat(sprintf("\n%s",data))
  }

}

#'@title print progress
#'@description prints messages about the progress of the synthesis process.
#'This shows which columns are being encoded and are synthesised.
#'Can be usefull for debugging.
print_progress<-function(type,data){
  if(type == "progress"){
    cat(sprintf("\n%s",data))
  }
}

#'@title print info and progress
#'@description comination of \link[print_info]{print_info} and \link[print_progress]{print_progress}
print_info_progress <- function(type,data){
  print_info(type,data)
  print_progress(type,data)
}

#'@title print info and progress
#'@description prints all log messages.
print_all <- function(type,data){
  cat(sprintf("\n%s",data))
}
