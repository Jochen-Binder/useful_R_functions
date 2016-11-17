######################################################################################
# SIMPLE VARIABLE IMPUTATION
# Uses Ward hierarchical cluster analysis to determine the suitable number of clusters
######################################################################################

IMPUTE.VARS <- function(dataset, 
                        vars = c(""),
                        deselect = 0) {
    
    'This function performs an imputation of all variables that are introduced except
    for those who are excluded in "deselct".
    
    The function takes the dataset as well as the metric ("metric.vars") variables.
    
    The function returns the dataset with the imputed variables'


    #########################
    # Load necessary packages
    library(mice)
    
    ####################
    # Imputing variables
    
    ##########################################################
    # Create subset of variables (for imputation or prediction)
    data <- dataset  
    Dat1 <- subset(dataset, select=vars) 
    ini <- mice(Dat1, maxit=0, pri=F)
    pred <- ini$pred
    
    ########################################
    # Variables not to be used as predictors
    pred[,c()] <- deselect
    
    ##########################################################
    # Cancel prediction method for variables not to be imputed
    meth <- ini$meth
    #meth[efa.vars] <- "" 
    
    #########################
    # Imputation and data set
    imp <- mice(Dat1, m=1, maxit=10, meth=meth, pred=pred) 
    data.imp <- complete(imp, "long", include=FALSE)
    data.imp <- data.frame(data.imp)
    #rm(Dat1, pred, ini, meth, imp)
    
    ############################################################
    # Exchanging old evaluation variables with imputed variables
    data.new <- cbind(data[,!names(data) %in% vars], 
                 data.imp[,names(data.imp) %in% vars])
    
    return(data.new)
}