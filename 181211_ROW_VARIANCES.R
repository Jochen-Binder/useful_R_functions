RowVar <- function(x, ...) {
  'This function takes a data set consisting of numeric variables and calculates the 
  variances across each row'
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}