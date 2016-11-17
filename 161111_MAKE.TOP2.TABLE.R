######################################################################################
# CREATE TOP2 VALUES, mean_median, AND MEDIANS for COMPARISON OF RESULTS
######################################################################################

MAKE.TOP2.TABLE <- function(dataset = dfc, 
                            vars = cluster.vars,
                            split = c("Country", "cluster.k.means"),
                            mean_median = c("Q7"),
                            median = FALSE,
                            weighted = TRUE) {
    
    'This function creates TOP2-Values (in percentages) for a given set of metric variables.
    across a set of given splits. For factor variables, the percentages are calculated.
    
    The function takes the dataset as well as the respectiv variables, the desired split variables.
    
    Metric variables, for which the mean or the median should be calculated instead of the
    Top2 values are defined via "mean_median". The "median" indicator indicates whether the median 
    should be calculated ("median = TRUE"). The mean is the default.
    
    The function returns a table (as data frame) of the results per split and calculation variables.'
    

    #########################
    # Load necessary packages
    library(plyr)
    
    suppressWarnings(rm(result))

    d <- dataset
    
    if(weighted==T){
        d$weights <- d$weights
    } else {d$weights <- 1}
    
    d$evalobject <- 0
    d$n <- NA
    
    # Columns you want to group by
    grp_cols <- c(split)
    
    # Convert character vector to list of symbols
    dots_results <- lapply(grp_cols, as.symbol)
    
    for(i in vars){
        if(is.factor(d[[i]])==T) {
            
            d$evalobject[d[i] == 1] <- 1
            d$evalobject[d[i] == 0] <- 0
            
        } else if ((i %in% mean_median)==T){
            
            d$evalobject <- as.numeric(d[[i]])
            #d$evalobject <- d$evalobjdect
            
        } else {
            
            d$evalobject[d[i] >= 4] <- 1
            d$evalobject[d[i] < 4] <- 0
            d$n[is.na(d[i])==F] <- 1
        
            }
        
            if(median==T & (i %in% mean_median)==T){
                
                results.n_T2 <- d %>% group_by_(.dots = dots_results) %>% summarise(T2 = median(evalobject))
                results.n_base <- d %>% group_by_(.dots = dots_results) %>% summarise(base = 1)
                
            } else {
                
                results.n_T2 <- d %>% group_by_(.dots = dots_results) %>% summarise(T2 = sum(na.omit(weights*evalobject)))
                results.n_base <- d %>% group_by_(.dots = dots_results) %>% summarise(base = sum(na.omit(weights*n)))
                
            }
        
        results <- merge(results.n_T2, results.n_base)
        
        
        results[[i]] <- round(results$T2/results$base, digits = 2)
        results$T2 <- NULL
        results$base <- NULL
        
        if(exists("result") == F) {
            result <- results
        } else {
            result <- merge(result, results)
        }
        rm(results)
    }  
    return(result)
}
    
