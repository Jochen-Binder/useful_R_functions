################################################################################
############################### Wordclouds ####################################
################################################################################

.libPaths( c( .libPaths(), "C:/Program Files/R/R-3.2.2/library") )

################################################################################
######################## Load necessary packages ###############################
################################################################################

library(Rcpp)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tm)

Sys.setenv(LANG = "en")

################################################################################
##################### Set paths and global variables ###########################
################################################################################

split_list <- c( "Known_DE", "Unknown_DE", "Cluster1_DE", "Cluster2_DE", "Cluster3_DE", "Cluster4_DE", "Cluster5_DE", 
                 "Known_US", "Unknown_US", "Cluster1_US","Cluster2_US", "Cluster3_US", "Cluster4_US" )

min_freq = 1                       # Set the minimum term frequency for display

################################################################################
########################### Setup input file paths #############################
################################################################################

# Parts for the input file string
fp_1 <- "//datenhd/Projekte$/2-Externe_Projekte/P16-0167_Porsche_Classic/Protokolle/4._Auswertung/3._Wordclouds"
fp_2 <- "/wordcloud_text/"
fp_3 <- ".csv"

# Parts for the output file string
fp_4 <- "//datenhd/Projekte$/2-Externe_Projekte/P16-0167_Porsche_Classic/Protokolle/4._Auswertung/3._Wordclouds/visuals"
fp_5 <- "/"
fp_6 <- "_wordcloud.png"


################################################################################
############################### Set the colors #################################
################################################################################

known_color <- rgb(0,0,0,max=255)
unknown_color <- rgb(127,127,127,max=255)
CL1_DE_color <- rgb(0,0,0,max=255)
CL2_DE_color <- rgb(89,89,89,max=255)
CL3_DE_color <- rgb(127,127,127,max=255)
CL4_DE_color <- rgb(191,191,191,max=255)
CL5_DE_color <- rgb(163,194,224,max=255)
CL1_US_color <- rgb(0,0,0,max=255)
CL2_US_color <- rgb(127,127,127,max=255)
CL3_US_color <- rgb(51,102,153,max=255)
CL4_US_color <- rgb(163,194,224,max=255)



################################################################################
######################### Make square word clouds ##############################
################################################################################

i <- 0

i <- i+1

switch( split_list[i] ,
        "Known_DE" = {wc_color = c( known_color )}
        ,
        "Known_US" = {wc_color = c( known_color )}
        ,
        "Unknown_DE" = {wc_color = c( unknown_color )}
        ,
        "Unknown_US" = {wc_color = c( unknown_color )}
        ,
        "Cluster1_DE" = {wc_color = c( CL1_DE_color )}
        ,
        "Cluster2_DE" = {wc_color = c( CL2_DE_color )}
        ,
        "Cluster3_DE" = {wc_color = c( CL3_DE_color )}
        ,
        "Cluster4_DE" = {wc_color = c( CL4_DE_color )}
        ,
        "Cluster5_DE" = {wc_color = c( CL5_DE_color )}
        ,
        "Cluster1_US" = {wc_color = c( CL1_US_color )}
        ,
        "Cluster2_US" = {wc_color = c( CL2_US_color )}
        ,
        "Cluster3_US" = {wc_color = c( CL3_US_color )}
        ,
        "Cluster4_US" = {wc_color = c( CL4_US_color )}
        ,
        # Default
        {wc_color = c( known_color )}
)


input_fp <- paste( fp_1 , fp_2 , split_list[i] , fp_3 , sep = "")
output_fp <- paste( fp_4 ,  fp_5 , fp_2 , split_list[i] , fp_6 , sep = "")

df <- read.table( input_fp , header = TRUE , sep = ";", quote='"')
df <- df[order(-df$freq ) , ]

print(paste("Wordcloud for: ", split_list[i], sep=""))
wordcloud2( df , size = 1 , rotateRatio = 0 , shape = "diamond" , color=wc_color ,
            shuffle = TRUE , hoverFunction = NULL)
