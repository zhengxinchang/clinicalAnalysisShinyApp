
#' help function for downloading ggplot2 objects
#'
#' @param ... 
#' @param width 
#' @param height 
#'
#' @return
#' @export
#'
#' @examples
download_gg_device <- function(..., width, height) {
  grDevices::pdf(..., width = width, height = height)
}


# table1
output$baseline_downloads <- downloadHandler(
  filename = function() {
    paste("Table1", "html", sep = ".")
  },
  content = function(file) {
    cat(reactive_table1(), file = file)
  }
)  


# clinical outocme tables and figures
output$outcome_downloads <- downloadHandler(
  filename = function() {
    paste("ClinicalOutcome", "zip", sep = ".")
  },
  content = function(file) {
    
    # from https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    #go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    Inudction_outcome <- "induction_outcome.html"
    cat(reactive_short_outcome(), file = Inudction_outcome)
    
    Survival_outcome <- "survival_outcome.pdf"
      
    ggsave(Survival_outcome, plot = reactive_long_outcome(), device = "pdf")
    
    #create the zip file
    zip(file,c(Inudction_outcome,Survival_outcome))
  }
)  


# glance adverse event 
output$adverse_event_downloads <- downloadHandler(
  filename = function() {
    paste("RelapseDeathTransplant", "html", sep = ".")
  },
  content = function(file) {

    cat(reactive_adverse_event(), file = file)
    
  }
)  


# 副作用
output$sideeffects_downloads <- downloadHandler(
  filename = function() {
    paste("SideEffectsAndCosts", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    sideeffects <- "SideEffects.html"
    costs <- "Costs.html"
    
    cat(reactive_sideeffect(), file = sideeffects)
    cat(reactive_cost(), file = costs)
    
    zip(file,c(sideeffects,costs))
    
    
  }
)  

# 性别诱导缓解
output$sex_indction_downloads <- downloadHandler(
  filename = function() {
    paste("Indution_Sex", "zip", sep = ".")
  },
  content = function(file) {
    
    # from https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    #go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    sexInduction<- reactive_risk_induction()
    filenames<- c()
    IndNames  <- names(sexInduction)
    
    for(indname in IndNames){
      
      treatNames <-names(sexInduction[[indname]])
      for(treat in treatNames) {
        fname <- paste(indname,treat,".html",sep="_")
        # browser()
        cat(sexInduction[[indname]][[treat]],file = fname)
        filenames<- c(filenames,fname)
      } 
    }
    
    zip(file,filenames)
  }
)  

# 危险度与诱导缓解聊城
output$risk_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_Risk", "zip", sep = ".")
  },
  content = function(file) {
    
    # from https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    #go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    objInduction<- reactive_risk_induction()
    filenames<- c()
    IndNames  <- names(objInduction)
    
    for(indname in IndNames){
      
      treatNames <-names(objInduction[[indname]])
      for(treat in treatNames) {
        fname <- paste(indname,treat,".html",sep="_")
        # browser()
        cat(objInduction[[indname]][[treat]],file = fname)
        filenames<- c(filenames,fname)
      } 
    }
    
    zip(file,filenames)
  }
)  


# FAB与诱导缓解疗程
output$fab_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_FAB", "zip", sep = ".")
  },
  content = function(file) {
    
    # from https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    #go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    objInduction<- reactive_FAB_induction()
    filenames<- c()
    IndNames  <- names(objInduction)
    
    for(indname in IndNames){
      
      treatNames <-names(objInduction[[indname]])
      for(treat in treatNames) {
        fname <- paste(indname,treat,".html",sep="_")
        # browser()
        cat(objInduction[[indname]][[treat]],file = fname)
        filenames<- c(filenames,fname)
      } 
    }
    
    zip(file,filenames)
  }
)  

# 基因mutation 诱导缓解疗程
output$mut_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_GeneMuts", "zip", sep = ".")
  },
  content = function(file) {
    
    # from https://stackoverflow.com/questions/43916535/download-multiple-csv-files-with-one-button-downloadhandler-with-r-shiny
    #go to a temp dir to avoid permission issues
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    objInduction<- reactive_GeneMut_induction()
    filenames<- c()
    IndNames  <- names(objInduction)
    
    for(indname in IndNames){
      
      treatNames <-names(objInduction[[indname]])
      for(treat in treatNames) {
        fname <- paste(indname,treat,".html",sep="_")
        # browser()
        cat(objInduction[[indname]][[treat]],file = fname)
        filenames<- c(filenames,fname)
      } 
    }
    
    zip(file,filenames)
  }
)  

# age induction
output$age_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_Age", "pdf", sep = ".")
  },
  content = function(file) {
    ggsave(file, plot = reactive_Age_induction(), device = "pdf")
  }
)  



# wbc_induction_downloads
output$wbc_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_WBC", "pdf", sep = ".")
  },
  content = function(file) {
    ggsave(file, plot = reactive_Wbc_induction(), device = "pdf")
  }
)  

# anc_induction_downloads
output$anc_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_ANC", "pdf", sep = ".")
  },
  content = function(file) {
    ggsave(file, plot = reactive_Anc_induction(), device = "pdf")
  }
)  


# plt_induction_downloads
output$plt_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Induction_Plt", "pdf", sep = ".")
  },
  content = function(file) {
    ggsave(file, plot = reactive_Plt_induction(), device = "pdf")
  }
)  

# hb_induction_downloads
output$hb_induction_downloads <- downloadHandler(
  filename = function() {
    paste("Inuduction_Hb", "pdf", sep = ".")
  },
  content = function(file) {
    ggsave(file, plot = reactive_Hb_induction(), device = "pdf")
  }
)  

# sex_survival_downloads
output$sex_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_Sex", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_sex_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_Sex_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# age_survival_downloads
output$age_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_Age", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_age_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_Age_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# wbc_survival_downloads
output$wbc_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_WBC", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_wbc_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_WBC_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# anc_survival_downloads
output$anc_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_ANC", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_anc_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_ANC_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# plt_survival_downloads
output$plt_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_PLT", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_plt_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_PLT_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# hb_survival_downloads
output$hb_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_HB", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_hb_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_HB_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# risk_survival_downloads
output$risk_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_Risk", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_risk_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_Risk_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# fab_survival_downloads
output$fab_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_FAB", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_fab_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_FAB_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  


# mut_survival_downloads
output$mut_survival_downloads <- downloadHandler(
  filename = function() {
    paste("Survival_GeneMutations", "zip", sep = ".")
  },
  content = function(file) {
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    list_plot<- reactive_genemut_survival()
    filenames<- c()
    survNames  <- names(list_plot)
    
    for(survname in survNames){
      gg_file_name <- paste0("Survival_GeneMutations_",survname,".pdf")
      ggsave(gg_file_name, plot = list_plot[[survname]], device = "pdf")
      filenames<- c(gg_file_name,filenames)
    }
    zip(file,filenames)
  }
)  

