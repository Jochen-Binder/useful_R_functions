varRename <- function(df, Namedict='Namedict', sheet='Namedict'){
        library(xlsx)
        lookup <- read.xlsx(paste(Namedict,'.xlsx',sep=""),1, sheetName=sheet)
        newvars <- lookup[,2]
        oldvars <- lookup[,1]
        
        
        for(i in 1:length(names(df))) {
          for(z in 1:length(newvars)) {
            if(as.character(colnames(df)[i]) == as.character(oldvars[z])) {
              colnames(df)[i] = as.character(newvars[z])
            }
          }
        }
        return(df)
}