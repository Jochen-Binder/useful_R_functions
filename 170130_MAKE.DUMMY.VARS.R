######################################################################################
# MAKE DUMMY VARIABLES
# Uses Ward hierarchical cluster analysis to determine the suitable number of clusters
######################################################################################

MAKE.DUMMY.VARS <- function(dataset, 
                            categorical.vars=c(""),
                            redundant = TRUE) {
  
  'This function performs turns categorical variables (factors with more than one level)
  into single dummy variables.
  
  The function takes the dataset as well as the set of categorical variables for which 
  dummy variables are needed.
  
  The function returns a new data set with the appended dummy variables'
  
  #########################
  # Load necessary packages
  #library(hclust)
  
  ################
  # Defining input
  dfc <- dataset
  m.fmla <- as.formula(paste(paste(paste(" ~ "), categorical.vars, collapse= "+"), "-1", collapse=""))
  
  dummy <- as.data.frame(model.matrix(m.fmla, data=dfc))
  dfc <- cbind(dfc, dummy)
  
  dfc[ ,names(dummy)] <- lapply(dfc[ ,names(dummy)], factor)
  
  if(redundant==TRUE){dfc <- dfc} else {dfc <- dfc[,1:length(names(dfc))-1]}
  
  
  return(dfc)
}