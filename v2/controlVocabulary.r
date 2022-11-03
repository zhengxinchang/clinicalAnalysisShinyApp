

if(! suppressWarnings(require(readxl))){print("can not load package readxl")}
if(! suppressWarnings(require(tidyverse))){print("can not load package tidyverse")}





initial_data <- jsonlite::fromJSON("http://bigd.big.ac.cn/amltest/dataAnalysis/initialData")




getControlVocabulary<- function(xlsx,sheet,name){
  colsnames <- names(initial_data)
  
  res <- list()
  for (i in colsnames){
    
    res[[i]]<- unique(initial_data[[i]])
    
  }

  write(res,file = "aa.txt",)
}