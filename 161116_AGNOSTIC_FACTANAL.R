######################################################################################
# JB's AGNOSTIC FACTOR ANALYSIS 
# easier selection from all possible factyor analyses for weighted and unweighted data
######################################################################################

AGNOSTIC.FACTANAL <- function(df, efa.vars, min.var.no=3, f.no=1) {
  
      'This function takes a data set and avector of variables in this data set for which the 
      factor analysis should be performed.
      
      Also, the data set should contain a variable called "weights" in which the respective 
      case wheights should be stored. If no wheigts are used, this variable consitst of "1s".
  
      Additionally, the function takes the minimum number of variables (min.var.no) and the
      minimum number of initial factors (f.no) which should be included in the analysis'
  
      #########################
      # Load necessary packages
      library(survey)
  
      #################################
      # Empty data frame for results
      efa.results <- data.frame(name=character(),
                                factor.no=integer(),
                                var.no=integer(),
                                var.loadings=integer(), 
                                max.uniqueness=integer(),
                                model.p.val=integer(),
                                min.eigenvalue=integer())
      
      #################################################################################
      # Subset the data frame according to the selected evaluation variables (efa.vars)
      df[ ,efa.vars] <- lapply(df[ ,efa.vars], as.numeric)
      
      ###########################################################################################
      # Create list of all possible combinations of efa.vars feasible for factor analysis (min 3)
      varlist<- c(); for(j in min.var.no:length(efa.vars)){varlist <- c(varlist, combn(efa.vars, m=j, simplify = F))}
      
      ########################
      # Iterating over varlist
      for(k in 1:length(varlist)){
        #################################
        # Define new "efa.vars" as "vars"
        vars <- unlist(varlist[k])
        
        ####################################################
        # Update survey design and formula with new efa.vars
        d <- svydesign(~0, data = na.omit(df[ ,vars]), weights=na.omit(df[ ,c(vars, "weights")])[["weights"]])
        fmla <- as.formula(paste(paste(" ~ "), paste(vars, collapse= "+")))
        
        ################################################################################
        # Iterating over number of factors (upper bound: max number of factors possible)
        p <- length(vars)
        f <- f.no
        dof <- p*(p+1)/2 - (((p*f)+(f*(f+1)/2)+p)-f^2)
        
        while(dof >= 0){
            #############################################################
            # Run WEIGHTED FACTOR ANALYSIS to determine number of factors
            FA.2 <- try(factanal(covmat=as.matrix(svyvar(fmla, design=d)), factors = f, 
                                 rotation = "varimax", n.obs=nrow(na.omit(df[ ,vars]))), silent=TRUE)
            
            ##################
            # Model identifier
            name <- paste("Variables:", paste(vars, collapse="/"), "||", "F.No:", i, sep="/")
            
            ################
            # Factor numbers
            factor.no <- tryCatch(FA.2$factors, error=function(err) NA)
            
            ##################
            # Variable numbers
            var.no <- tryCatch(length(vars), error=function(err) NA)
            
            ###################
            # Deriving loadings
            loadings <- tryCatch(FA.2$loadings, error=function(err) NA)
            
            ########################
            # Calculating raw scores
            L <- tryCatch(as.matrix(loadings[vars,]), error=function(err) NA)
            
            ###################################################
            # Calculating variances of loadings for each factor
            var.loadings <- tryCatch(apply(L, 2, var), error=function(err) NA)
            
            #####################
            # Averaging variances
            var.loadings <- tryCatch(mean(var.loadings), error=function(err) NA)
            
            #############################
            # Deriving maximal uniqueness
            max.uniqueness <- tryCatch(max(FA.2$uniquenesses), error=function(err) NA)
            
            ####################################
            # Deriving adequacy of factor number
            model.p.val <- tryCatch(if(FA.2$dof == 0){
                model.p.val <- 0
            } else {model.p.val <- FA.2$PVAL}, error = function(err) {
                model.p.val <- NA
                return(model.p.val)})
            
            #########################
            # Calculating eigenvalues
            min.eigenvalue <-  tryCatch(min(apply(L, 2, function(x) sum(x^2))), error=function(err) NA)
            
            model.results <- data.frame(name, factor.no, var.no, var.loadings, max.uniqueness, model.p.val, min.eigenvalue, row.names=NULL)
            efa.results <- rbind(efa.results, model.results)
            rm(model.results, loadings, L, var.loadings, max.uniqueness, model.p.val, min.eigenvalue)
            
            percent_done <- round(k/length(varlist)*100,2)
            print(paste("Percent calculated: ", percent_done, sep=""))
            
            f <- f + 1
            dof <- p*(p+1)/2 - ((p*f+f*(f+1)/2+p)-f^2)
        }


    }
      return(efa.results)
      rm(efa.results)
}