######################################################################################
# Ward Cluster Analysis
# Uses Ward hierarchical cluster analysis to determine the suitable number of clusters
######################################################################################

WARD.CLUSTER <- function(dataset, 
                         vars = c(""),
                         d.metric = "gower") {
    
    'This function performs a hierarchical Ward cluster analysis, plots the dendrogram 
    and returns an R object hclust object.
    
    The function takes the dataset as well as the metric ("metric.vars") and 
    the categorical variables ("categorical.vars") for which the cluster analysis is to
    be performed'
    
    #########################
    # Load necessary packages
    #library(hclust)
    
    ################
    # Defining input
    dfc <- dataset
    cluster.vars <- vars
    
    ########################################################################
    # Calculatying the distance matix based on the selected cluster variables
    dist <- daisy(dfc[,cluster.vars], metric=d.metric)
    
    ###########################################################
    # WARD CLUSTERING (FINDING CLUSTER NUMBERS) on imputed data
    
    # Ward Cluster Analysis
    w.cluster <- hclust(dist, method="ward.D")
    
    #################
    # Plot Dendrogram
    dend <- as.dendrogram(w.cluster)
    plot(dend)
    #plot(cut(dend, h=10)$upper)
    return(w.cluster)
}