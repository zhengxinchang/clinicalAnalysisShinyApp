
if ( ! suppressWarnings (require(dplyr))){print("Can not load labrary dplyr...")}
if ( ! suppressWarnings (require(jsonlite))){print("Can not load labrary jsontile...")}
if ( ! suppressWarnings (require(stringr))){print("Can not load labrary stringr...")}


source("./data_prepar.r",encoding = "utf-8")

a<- data_prepare()
