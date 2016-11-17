######################################################################################
# JB's AGNOSTIC FACTOR ANALYSIS 
# easier selection from all possible factyor analyses for weighted and unweighted data
######################################################################################

FACTOR.SCORES <- function(FA.2, df) {
  
  'This function takes the results from factanal as an R object and a data set for which the 
  factor analysis has been performed.
  
  Also, the data set should contain a variable called "weights" in which the respective 
  case wheights should be stored. If no wheigts are used, this variable consitst of "1s".'
  
  #########################
  # Load necessary packages
  library(mice)
  
  #########################################
  # Calculating factor scores from loadings
  
  ###################
  # Deriving loadings
  loadings <- FA.2$loadings
  
  #########################################
  # Deriving efa.vars from the factor model
  efa.vars <- names(FA.2$uniquenesses)
  
  ########################
  # Calculating raw scores
  L <- as.matrix(loadings[efa.vars,])
  V <- as.matrix(df[ ,efa.vars])
  raw.scores <- (V%*%L)
  
  #########################################
  # Rescaling raw scores into the range 1-5
  scores <- apply(raw.scores, 2, function(x) (5-1)/(max(x, na.rm=T)-min(x, na.rm=T))*(x-max(x,na.rm=T))+5)
  
  ################################
  # Appending scores to data frame
  scores <- as.data.frame(scores)
  df <- cbind(df, scores)
  rm(L,V,raw.scores)
  
  return(scores)
  
}