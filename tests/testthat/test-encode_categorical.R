
get_generate_encoding_testdata <- function(){
  return(data.frame(
    feature = c("A","A","B"),
    target_num =c(1,2,3),
    target_cat= c("C","D","D")
  ))
}


test_that("mean encoding is calculated correctly for numeric example",{

  test_data <- get_generate_encoding_testdata()
  result <- mean_encode(test_data$feature, test_data$target_num)

  expect_equal(result$feature_level,c("A","B"))
  expect_equal(result[["1"]],c(1.5,3))
})

test_that("pca encoding is calculated correctly for numeric example",{

  test_data <- get_generate_encoding_testdata()

  expected_input_for_pca <- t(tibble(
    #x = c("A","B"),
    pca_encoding_C = c(1,0),
    pca_encoding_D = c(1,1)
  ))

  expected_encoding <-tibble(
    feature_level = c("A","B"),
    V1 = c(42,10),
    V2 = c(23,5)
  )

  pca_return_value = list(
    rotation = matrix(c(42,23,
                        10,5), ncol = 2,nrow = 2,byrow=TRUE,
                      dimnames = list(c("PC1","PC2"),c("V1","V2"))),
    sdev = c(3,5)
  )

  with_mocked_bindings(result <- pca_encode(test_data$feature,test_data$target_cat),
                       prcomp= function(data,center,scale){
                         expect_equal(expected_input_for_pca,data)
                         expect_equal( center,TRUE)
                         expect_equal(scale,as.factor(c(FALSE,TRUE)))
                         return(pca_return_value)
                       })

  expect_equal(result$encoding,expected_encoding)
  expect_equal(result$sdev,pca_return_value$sdev)

})

test_that("deal with columns with zero variance",{
  test_data <- data.frame(
    feature = c("A","A"),
    target_cat= c("C","D")
  )
  result <- pca_encode(test_data$feature,test_data$target_cat)
  expect_equal(length(result),2)
})
test_that("duplicate rows have only effect on count values",{
  test_data <- data.frame(
    feature = c("A","A","A","B"),
    target_cat= c("C","C","D","D")
  )

  expected_input_for_pca <- t(tibble(
    #x = c("A","B"),
    pca_encoding_C = c(2,0),
    pca_encoding_D = c(1,1)
  ))

  pca_return_value = list(
    rotation = matrix(c(42,10,
                        23,5), ncol = 2,nrow = 2,byrow=TRUE,
                      dimnames = list(c("PC1","PC2"),c())),
    sdev = c(3,5)
  )

  with_mocked_bindings(result <- pca_encode(test_data$feature,test_data$target_cat),
                       prcomp= function(data,center,scale){
                         expect_equal(expected_input_for_pca,data)
                         expect_equal( center,TRUE)
                         expect_equal(scale,as.factor(c(TRUE,TRUE)))
                         return(pca_return_value)
                       })

})

test_that("rows with NA for feature value are removed before doing PCA",{
  test_data <- data.frame(
    feature = c("A",NA,"A","B"),
    target_cat= c("C","C","D","D")
  )

  expected_input_for_pca <- t(tibble(
    #x = c("A","B"),
    pca_encoding_C = c(1,0),
    pca_encoding_D = c(1,1)
  ))

  pca_return_value = list(
    rotation = matrix(c(42,10,
                        23,5), ncol = 2,nrow = 2,byrow=TRUE,
                      dimnames = list(c("PC1","PC2"),c())),
    sdev = c(3,5)
  )

  with_mocked_bindings(result <- pca_encode(test_data$feature,test_data$target_cat),
                       prcomp= function(data,center,scale){
                         expect_equal(expected_input_for_pca,data)
                         expect_equal( center,TRUE)
                         expect_equal(scale,as.factor(c(FALSE,TRUE)))
                         return(pca_return_value)
                       })
})


test_that("encoding is applied to column",{
    categorical_column = get_generate_encoding_testdata()$feature

    encoding <-tibble(
      feature_level = c("A","B"),
      PC1 = c(42,10),
      PC2 = c(23,5)
    )

    result <- apply_encoding_column(categorical_column,encoding)

    expected_result <- tibble(
      PC1 = c(42,42,10),
      PC2 = c(23,23,5)
    )

    expect_equal(result,expected_result)
})

test_that("NA values are preserved when applying encoding",{
  categorical_column = c("A","A",NA,"B")

  encoding <-tibble(
    feature_level = c("A","B"),
    PC1 = c(42,10),
    PC2 = c(23,5)
  )

  result <- apply_encoding_column(categorical_column,encoding)

  expected_result <- tibble(
    PC1 = c(42,42,NA,10),
    PC2 = c(23,23,NA,5)
  )

  expect_equal(result,expected_result)
})



test_that("encoding a categorical variable with more than 15 levels and a numeric target uses mean encoding unaltered",{


  test_data <- tibble(
    feature = map_chr(1:16,\(n) strrep("a",n)),
    target = c(1:16)
  )

  expected_result <- 1:21

  logger <- function(type,data){
    # the exact log messages are still subject to change
    #expect_equal(type,"info")
    #expect_equal(data,"'encoding-type':'mean encode'")
  }

  with_mocked_bindings(result <- get_encoding(test_data$feature,test_data$target,logger),
                       mean_encode = function(x,y){
                         expect_equal(x,test_data$feature)
                         expect_equal(y,test_data$target)
                         return(expected_result)
                       }
                       )

  expect_equal(result,expected_result)

})

test_that("encoding a categorical variable with more than 15 levels with a target having 30 levels or less uses PCA with all components",{
  test_data <- tibble(
    feature = map_chr(1:16,\(n) strrep("a",n)),
    target = as.factor(rep(map_chr(1:4,\(n) strrep("b",n)),4))
  )

  expected_result <- 1:21

  logger <- function(type,data){
    # the exact log messages are still subject to change
    #expect_equal(type,"info")
    #expect_equal(data,"'encoding-type':'PCA-full'")
  }

  with_mocked_bindings(result <- get_encoding(test_data$feature,test_data$target,logger),
                       pca_encode = function(x,y){
                         expect_equal(x,test_data$feature)
                         expect_equal(y,test_data$target)
                         return(list(encoding = expected_result, sdev = c(1:3)))
                       }
  )

  expect_equal(result,expected_result)
})

test_that("encoding a categorical variable with at least 30 levels with a target having at least 30 levels or less uses PCA with limited components",{
  test_data <- tibble(
    feature = as.factor(map_chr(1:31,\(n) strrep("a",n))),
    target = as.factor(map_chr(1:31,\(n) strrep("b",n)))
  )

  full_pca_encoding <- tibble(
    feature = map_chr(1:31,\(n) strrep("a",n)),
    PC1 = c(1:31),
    PC2 = c(1:31)*2,
    PC3 = c(1:31)*3,
  )
  sdevs <- c(1,2,3)

  expected_result <- tibble(
    feature = map_chr(1:31,\(n) strrep("a",n)),
    PC1 = c(1:31),
    PC2 = c(1:31)*2,
  )

  logger <- function(type,data){
    # the exact log messages are still subject to change
    #expect_equal(type,"info")
    #expect_equal(data,"'encoding-type':'PCA-redcued'")
  }
  with_mocked_bindings(result <- get_encoding(test_data$feature,test_data$target, logger = logger),
                       pca_encode = function(x,y){
                         expect_equal(x,test_data$feature)
                         expect_equal(y,test_data$target)
                         return(list(encoding = full_pca_encoding, sdev =sdevs))
                       }
                       ,number_of_principal_components = function(s){
                         expect_equal(s,sdevs)
                         return(2)
                       }
  )

  expect_equal(result,expected_result)
})

test_that("calculating the number of principle components to use works",{

  sdevs <- rep(1,100)
  result <- number_of_principal_components(sdevs)
  expect_equal(95,result)

  sdevs <- rep(1,100)*2
  result <- number_of_principal_components(sdevs)
  expect_equal(95,result)

  sdevs <- c(70,10,15,1,1,1,1,1)#70+10+15 =95, total = 100
  result <- number_of_principal_components(sdevs)
  expect_equal(3,result)

  sdevs <- c(70,10,15,1,1,1,1,1)*7
  result <- number_of_principal_components(sdevs)
  expect_equal(3,result)

  sdevs<- c(94,6)
  result <- number_of_principal_components(sdevs)
  expect_equal(result,2)

  sdevs<- c(95,5)
  result <- number_of_principal_components(sdevs)
  expect_equal(result,1)

  sdevs<- c(96,4)
  result <- number_of_principal_components(sdevs)
  expect_equal(result,1)

})

test_that("encoding a numeric column does nothing",{

  feature = tibble(a= c(1,2,3,4))

  logger <- function(type,data){
    # the exact log messages are still subject to change
    #expect_equal(type,"info")
    #expect_equal(data,"'encoding-type':'none'")
  }
  result1 <- encode_categorical(feature$a,feature$a,feature_name = "a",logger= logger)

  expect_equal(feature$a,result1$x$a)
  expect_equal(feature$a,result1$xp$a)

})

test_that("encoding a categorical variable with 15 or less levels does nothing",{

  feature <- tibble(a=c("A","B","C"))

  logger <- function(type,data){
    # the exact log messages are still subject to change
    #expect_equal(type,"info")
    #expect_equal(data,"'encoding-type':'none'")
  }

  result <- encode_categorical(feature$a,feature$a,feature_name = "a",logger=logger)

  expect_equal(result$x$a,feature$a)
  expect_equal(result$xp$a,feature$a)

  feature <- tibble(a=map_chr(1:15,\(n) strrep("a",n)))
  result2 <- encode_categorical(feature$a,feature$a,feature_name = "a",logger=logger)
  expect_equal(result2$x$a,feature$a)
  expect_equal(result2$xp$a,feature$a)

})

test_that("a categorical variable with at least 15 levels gets encoded",{
  test_data <- tibble(
    x = as.factor(map_chr(1:16,\(n) strrep("a",n))),
    xp = as.factor(map_chr(1:16,\(n) strrep("b",n))),
    target = as.factor(map_chr(1:16,\(n) strrep("b",n))),
  )

  encoding <-tibble(
    feature = map_chr(1:16,\(n) strrep("a",n)),
    PC1 = c(1:16),
    PC2 = c(1:16)*2,
  )

  encoded_column_x <-tibble(
    PC1 = c(1:16)*3,
    PC2 = c(1:16)*4)

  encoded_column_xp <-tibble(
    PC1 = c(1:16)*5,
    PC2 = c(1:16)*6)


  logger <- function(type,data){
    # the exact log messages are still subject to change
  }

  with_mocked_bindings(result<-encode_categorical(test_data$x,test_data$xp,test_data$target,"name_of_feature",logger=logger),
                       get_encoding=function(x,y,logger){
                         expect_equal(x,test_data$x)
                         expect_equal(y,test_data$target)
                         return(encoding)
                       },
                       apply_encoding_column = function(c,enc){
                         expect_equal(enc,expected=encoding)
                         if(identical(c,test_data$x) ){
                           return(encoded_column_x)
                         }
                         if(identical(c,test_data$xp)  ){
                           return(encoded_column_xp)
                         }

                         return(1)
                       })

  expect_equal(result$x$name_of_feature_PC1,expected=encoded_column_x$PC1)
  expect_equal(result$x$name_of_feature_PC2,expected=encoded_column_x$PC2)
  expect_equal(result$xp$name_of_feature_PC1,expected=encoded_column_xp$PC1)
  expect_equal(result$xp$name_of_feature_PC2,expected=encoded_column_xp$PC2)
})

test_that("creating a pca encoding works for different sizes",{
  test_data <- tibble(
    x = as.factor(rep(map_chr(1:8,\(n) strrep("a",n)),2)),
    y = as.factor(map_chr(1:16,\(n) strrep("b",n))),
  )

  res <- pca_encode(test_data$x,test_data$y)
  print(res$encoding)
  expect_equal(ncol(res$encoding),9)
  expect_equal(nrow(res$encoding),8)

  res <- pca_encode(test_data$y,test_data$x)
  expect_equal(length(res$encoding),9)
  expect_equal(nrow(res$encoding),16)

  test_data <- tibble(
    x = as.factor(c("A","B","C","A")),
    y = as.factor(c("X","Y","Z","W"))
  )
  res <- pca_encode(test_data$y,test_data$x)
})

test_that("encoding can handle empty string values", {
  test_data <- tibble(
    x = as.factor(c("a","b","c")),
    y = as.factor(c("A", "","C")),
  )

  res <- pca_encode(test_data$x,test_data$y)
  expect_equal(length(res$sdev),3)
})

