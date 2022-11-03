
# LOAD PACKAGES
library(shiny)
library(shiny.router)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(table1)
library(ggpubr)
library(survminer)
library(cowplot)
library(readxl)
library(Hmisc)
library(Gmisc)

# Style all the table output using htmlTable's theme handler
htmlTable::setHtmlTableTheme(css.rgroup = "")
#options(htmlTable.cat=T)

# supress all warnings information
options(warn = -1)

setwd("E:/projects/git_repo/aml_database_analysis_shinyapp/v2")



source("./data_prepar.r",encoding = "utf-8")

data_list <- data_prepare()

unified_data <- data_list$unified_data

# DEFINE COMMON VARS ---------------------------------------------



hospital_names_chn = c("所有医院" = 0,
                       "苏州儿童医院" = 1,
                       "安徽省立医院" = 2,
                       "徐州市儿童医院" = 3,
                       "山东大学齐鲁医院" = 4,
                       "广西医科大学附属第一医院" = 5,
                       "复旦大学附属儿科医院" = 6,
                       "浙江大学附属儿童医院" = 7,
                       "郑州大学第一附属医院" = 8,
                       "中南大学湘雅医院" = 9,
                       "广州市妇女儿童医疗中心" = 10,
                       "安徽医科大学附属第二医院" = 11,
                       "上海交通大学附属新华医院" = 12,
                       "开封市儿童医院" = 13)

function(input,output){
  
  gen_corr_table_mutation<-function(dat,by,name,rowvar,header="")
    a1 <- list()
    label(dat[[rowvar]]) <- name
    a1[[name]] <-getTable1Stats(dat[[rowvar]])
    t<-mergeDesc(a1) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = header, tfoot = " ")
    print(t)
    
  }
  
  
  gen_corr_table<-function(dat,by,name,rowvar,header=""){
    a1 <- list()
    label(dat[[rowvar]]) <- name
    a1[[name]] <-
      getTable1Stats(dat[[rowvar]])
    t<-mergeDesc(a1) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = header, tfoot = " ")
    print(t)
    
  }
  
  
  getTable1Stats <- function(x, digits = 0, ...){
    getDescriptionStatsBy(x = x,
                          by = unified_data$Treatment,
                          digits = digits,
                          continuous_fn = describeMedian,
                          header_count = TRUE,
                          statistics = TRUE,
                          show_all_values = TRUE,
                          ...)
    }
  
  
  myggbox<-function(dats,x,y,xlab,ylab,title,comp,my_palette){
    
    p<-ggpubr::ggboxplot(dats,x=x,y=y,fill=x, palette = my_palette ,add = 'jitter',
                         shape=x,ylab =ylab,
                         xlab = xlab,title=title) +
      ggpubr::stat_compare_means(comparisons = comp,method = 'wilcox',paired = F)
    return(p)
  }
  
  
  # Galance -----------------------------------------------------------------
  
  
  output$Table1 <- renderUI({
    #filter hospital
    if(input$base_line_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$base_line_hospital)
    }
    
    # https://cran.rstudio.com/web/packages/Gmisc/vignettes/Descriptives.html#the-basics-of-getdescriptionstatsby
    
    getTable1Stats <- function(x, digits = 0, ...){
      getDescriptionStatsBy(x = x,
                            by = unified_data$Treatment,
                            digits = digits,
                            continuous_fn = describeMedian,
                            header_count = TRUE,
                            statistics = TRUE,
                            show_all_values = TRUE,
                            ...)
    }
    
    t1 <- list()
    label(unified_data$sex_category) <- "Sex"
    t1[["Sex"]] <-
      getTable1Stats(unified_data$sex_category)
    
    label(unified_data$age) <- "Age"
    units(unified_data$age) <- "Years"
    t1[["Age"]] <-
      getTable1Stats(unified_data$age)
    
    label(unified_data$wbc) <- "White Blood Cell Counts"
    units(unified_data$age) <- "10^9/L"
    t1[["White Blood Cell Counts&dagger;"]] <- 
      getTable1Stats(unified_data$wbc)
    
    label(unified_data$plt) <- "Platelets"
    units(unified_data$plt) <- "10^9/L"
    t1[["Platelets&dagger;"]] <- 
      getTable1Stats(unified_data$plt)
    
    label(unified_data$anc) <- "Absolute Neutrophil Counts"
    units(unified_data$anc) <- "10^9/L"
    t1[["Absolute Neutrophil Counts&dagger;"]] <- 
      getTable1Stats(unified_data$anc)
    
    label(unified_data$hb) <- "Hemoglobin"
    units(unified_data$hb) <- "10^9/L"
    t1[["Hemoglobin&dagger;"]] <- 
      getTable1Stats(unified_data$hb)
    
    unified_data$aml_risk<-factor(unified_data$aml_risk,levels = c(1,2,3,4),labels = c("Low","Mid","High","unclassify"))
    label(unified_data$aml_risk) <- "AML Risk Group"
    t1[["AML Risk Group"]] <- 
      getTable1Stats(unified_data$aml_risk)
    
    unified_data$FAB_category<-factor(unified_data$FAB_category,levels = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "MDS RAEB-t", "MDS RAEB", "无法分类" ),
                                      labels = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "MDS RAEB-t", "MDS RAEB", "unclassify" ))
    label(unified_data$FAB_category) <- "FAB Classification"
    t1[["FAB Classification"]] <- 
      getTable1Stats(unified_data$FAB_category)
    
    
    label(unified_data$CKIT) <- "CKIT"
    t1[["Mutations ckit"]] <- 
      getTable1Stats(unified_data$CKIT)
    
    label(unified_data$NRAS) <- "NRAS"
    t1[["Mutations nras"]] <- 
      getTable1Stats(unified_data$NRAS)
    
    label(unified_data$CEBPA) <- "CEBPA"
    t1[["Mutations cepba"]] <- 
      getTable1Stats(unified_data$CEBPA)
    
    label(unified_data$NPM1) <- "NPM1"
    t1[["Mutations npm1"]] <- 
      getTable1Stats(unified_data$NPM1)
    
    label(unified_data$ASXL1) <- "ASXL1"
    t1[["Mutations asxl1"]] <- 
      getTable1Stats(unified_data$ASXL1)
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    basic_line<-mergeDesc(t1) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Basic descriptive statistics for the whole cohort", tfoot = "&dagger; The unit is in 10<sup>9</sup> /L")
    print(basic_line)
    
  })
  
  output$short_outcome<-renderUI({
    
    if(input$clincical_outocme_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$clincical_outocme_hospital)
    }
    
    
    getTable1Stats <- function(x, digits = 0, ...){
      getDescriptionStatsBy(x = x,
                            by = unified_data$Treatment,
                            digits = digits,
                            continuous_fn = describeMedian,
                            header_count = TRUE,
                            statistics = TRUE,
                            show_all_values = TRUE,
                            ...)}
    
    t2 <- list()
    #unified_data$sex_category<-factor(unified_data$sex_category,levels = c("o","1"),labels = c("男","女"))
    label(unified_data$induction1_outcome) <- "Induction1"
    t2[["Induction1"]] <-
      getTable1Stats(unified_data$induction1_outcome)
    
    label(unified_data$induction2_outcome) <- "Induction2"
    t2[["Induction2"]] <- 
      getTable1Stats(unified_data$induction2_outcome)
    
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    short_outcome<-mergeDesc(t2) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Clinical outcome of induction1/2",
                tfoot = " ")
    print(short_outcome)
    
    
  })
  
  output$Survival<-renderPlot({
    
    
    if(input$clincical_outocme_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$clincical_outocme_hospital)
    }
    
    
    
    fitOSSH <-survfit(Surv(OS,Status_OS)~Treatment,data=unified_data)
    
    ggOSSH <- ggsurvplot(fitOSSH,data = unified_data,
                         pval = TRUE, conf.int = TRUE,
                         risk.table = TRUE, # Add risk table
                         risk.table.col = "strata", # Change risk table color by groups
                         linetype = "strata", # Change line type by groups
                         surv.median.line = "hv", # Specify median survival
                         ggtheme = theme_bw(), # Change ggplot2 theme
                         palette = input$clincical_outcome_palette,legend.title="Overall Survival",legend.labs=c("LDC","SDC")
    )
    
    
    
    
    fitEFSSH <-survfit(Surv(EFS,Status_EFS)~Treatment,data=unified_data)
    ggEFSSH <- ggsurvplot(fitOSSH,data=unified_data,
                          pval = TRUE, conf.int = TRUE,
                          risk.table = TRUE, # Add risk table
                          risk.table.col = "strata", # Change risk table color by groups
                          linetype = "strata", # Change line type by groups
                          surv.median.line = "hv", # Specify median survival
                          ggtheme = theme_bw(), # Change ggplot2 theme
                          palette = input$clincical_outcome_palette,legend.title="Event Free Survival",legend.labs=c("LDC","SDC")
    )
    
    
    
    fitRFSSH <-survfit(Surv(RFS,Status_RFS)~Treatment,data=unified_data)
    ggRFSSH <- ggsurvplot(fitRFSSH,data=unified_data,
                          pval = TRUE, conf.int = TRUE,
                          risk.table = TRUE, # Add risk table
                          risk.table.col = "strata", # Change risk table color by groups
                          linetype = "strata", # Change line type by groups
                          surv.median.line = "hv", # Specify median survival
                          ggtheme = theme_bw(), # Change ggplot2 theme
                          palette = input$clincical_outcome_palette,fun="event",legend.title="Cumulative Incidence of Relapse",legend.labs=c("LDC","SDC")
    )
    
    
    
    #ggOSSH$table
    cowplot::plot_grid(
      cowplot::plot_grid(ggOSSH$plot,ggOSSH$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
      cowplot::plot_grid(ggEFSSH$plot,ggEFSSH$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
      cowplot::plot_grid(ggRFSSH$plot,ggRFSSH$table+guides(color=F),ncol=1,rel_heights = c(2,1))
    )
    
    #cowplot::plot_grid(ggOSSH$table,ggEFSSH$plot,ggRFSSH$plot,ncol=2,labels = c("A","B","C"))
    
    
    
    
  })
  
  output$deathrelapsetransplant<-renderUI({
    
    
    if(input$death_relap_transplant_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$death_relap_transplant_hospital)
    }
    
    
    
    getTable1Stats <- function(x, digits = 0, ...){
      getDescriptionStatsBy(x = x,
                            by = unified_data$Treatment,
                            digits = digits,
                            continuous_fn = describeMedian,
                            header_count = TRUE,
                            statistics = TRUE,
                            show_all_values = TRUE,
                            ...)}
    
    t4 <- list()
    # there are no death information yet, so we genrate simulation
    unified_data$is_death <-sample(c("Yes","No"),size = nrow(unified_data),replace = T)
    unified_data$is_death <- factor(unified_data$is_death,levels = c("Yes","No"),labels = c("Yes","No"))
    label(unified_data$is_death) <- "death" 
    t4[["Death"]] <-
      getTable1Stats(unified_data$is_death)
    
    
    unified_data$is_relapse <-sample(c("Yes","No"),size = nrow(unified_data),replace = T)
    unified_data$is_relapse <- factor(unified_data$is_relapse,levels = c("Yes","No"),labels = c("Yes","No"))
    label(unified_data$induction2_outcome) <- "relapse"
    t4[["Relapse"]] <- 
      getTable1Stats(unified_data$is_relapse)
    
    unified_data$is_transplant <-sample(c("Yes","No"),size = nrow(unified_data),replace = T)
    unified_data$is_transplant <- factor(unified_data$is_transplant,levels = c("Yes","No"),labels = c("Yes","No"))
    label(unified_data$is_transplant) <- "relapse"
    t4[["Transplant"]] <- 
      getTable1Stats(unified_data$is_transplant)
    
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    drt<-mergeDesc(t4) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Death/Relapse/Transplant",
                tfoot = " ")
    print(drt)
    
    
  })
  
  output$sideeffect<-renderUI({
    
    if(input$sideeffect_costs_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$sideeffect_costs_hospital)
    }
    
    
    getTable1Stats <- function(x, digits = 0, ...){
      getDescriptionStatsBy(x = x,
                            by = unified_data$Treatment,
                            digits = digits,
                            continuous_fn = describeMedian,
                            header_count = TRUE,
                            statistics = TRUE,
                            show_all_values = TRUE,
                            ...)}
    
    t3 <- list()
    #unified_data$sex_category<-factor(unified_data$sex_category,levels = c("o","1"),labels = c("男","女"))
    
    label(unified_data$induction1_anc_recover_time) <- "Induction1 anc reovery"
    t3[["Induction1: recovery Time ANC "]] <-
      getTable1Stats(unified_data$induction1_anc_recover_time)
    
    
    label(unified_data$induction2_anc_recover_time) <- "Induction2 anc reovery"
    t3[["Induction2: recovery Time ANC"]] <- 
      getTable1Stats(unified_data$induction2_anc_recover_time)
    
    label(unified_data$induction1_plt_recover_time) <- "Induction1 platelet reovery"
    t3[["Induction1: recovery Time Platelet"]] <-
      getTable1Stats(unified_data$induction1_plt_recover_time)
    
    label(unified_data$induction2_plt_recover_time) <- "Induction2 platelet reovery"
    t3[["Induction2: recovery Time Platelet"]] <- 
      getTable1Stats(unified_data$induction2_plt_recover_time)
    
    
    
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    side<-mergeDesc(t3) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Side effects",
                tfoot = " ")
    print(side)
    
  })
  
  output$costs<-renderUI({
    
    if(input$sideeffect_costs_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$sideeffect_costs_hospital)
    }
    
    
    
    
    t3_cost <- list()
    label(unified_data$induction1_period_cost) <- "Induction1 costs"
    t3_cost[["Induction1: Costs"]] <- 
      getTable1Stats(unified_data$induction1_period_cost)
    
    label(unified_data$induction2_period_cost) <- "Induction2 costs"
    t3_cost[["Induction2: Costs"]] <- 
      getTable1Stats(unified_data$induction2_period_cost)
    cost<-mergeDesc(t3_cost) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Costs",
                tfoot = " ")
    
    print(cost)
    
    
    
  })
  
  output$RoutineBlood<-renderPlot({
    
    if(input$blood_rotine_hospital != 0){
      series_data_long<- series_data_long %>% dplyr::filter(hospital_id == input$blood_rotine_hospital)
    }
    series_data_long %>%
      ggplot() + 
      geom_line(aes(x=Series_nth_day,y=value,color=factor(Treatment),group=factor(Series_patient_id))) + 
      facet_grid(Params~Treatment,scales = "free") + theme_bw()  +labs(color="Treatment") + xlab(label = "Days") +
      stat_smooth(aes(x=Series_nth_day,y=value))
  })    
  
  # short-term outcome versus induction ---------------------------------------------------------------
  
  
  output$sex_induction1_LDC <- renderUI({
    
    if(input$induction_sex_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_sex_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="LDC")
    gen_corr_table(dat,by="induction1_outcome","Sex","sex_category","LDC Induction1")
  })
  
  output$sex_induction2_LDC <- renderUI({
    
    if(input$induction_sex_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_sex_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="LDC")
    gen_corr_table(dat,by="induction2_outcome","Sex","sex_category","LDC Induction2")
  })
  
  output$sex_induction1_SDC <- renderUI({
    
    if(input$induction_sex_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_sex_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="SDC")
    gen_corr_table(dat,by="induction1_outcome","Sex","sex_category","SDC Induction1")
  })
  
  output$sex_induction2_SDC <- renderUI({
    
    if(input$induction_sex_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_sex_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="SDC")
    gen_corr_table(dat,by="induction2_outcome","Sex","sex_category","SDC Induction2")
  })
  
  
  
  
  
  # risk
  
  output$risk_induction1_LDC <- renderUI({
    
    if(input$induction_risk_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_risk_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="LDC")
    gen_corr_table(dat,by="induction1_outcome","AML risk","aml_risk_category","LDC Induction1")
  })
  
  output$risk_induction2_LDC <- renderUI({
    
    if(input$induction_risk_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_risk_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="LDC")
    gen_corr_table(dat,by="induction2_outcome","AML risk","aml_risk_category","LDC Induction2")
  })
  
  output$risk_induction1_SDC <- renderUI({
    
    if(input$induction_risk_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_risk_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="SDC")
    gen_corr_table(dat,by="induction1_outcome","AML risk","aml_risk_category","SDC Induction1")
  })
  
  output$risk_induction2_SDC <- renderUI({
    
    if(input$induction_risk_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_risk_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="SDC")
    gen_corr_table(dat,by="induction2_outcome","AML risk","aml_risk_category","SDC Induction2")
  })
  
  # fab
  
  output$fab_induction1_LDC <- renderUI({
    
    if(input$induction_fab_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_fab_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="LDC")
    gen_corr_table(dat,by="induction1_outcome","FAB Classification","FAB_category","LDC Induction1")
  })
  
  output$fab_induction2_LDC <- renderUI({
    
    if(input$induction_fab_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_fab_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="LDC")
    gen_corr_table(dat,by="induction2_outcome","FAB Classification","FAB_category","LDC Induction2")
  })
  
  output$fab_induction1_SDC <- renderUI({
    
    if(input$induction_fab_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_fab_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="SDC")
    gen_corr_table(dat,by="induction1_outcome","FAB Classification","FAB_category","SDC Induction1")
  })
  
  output$fab_induction2_SDC <- renderUI({
    
    if(input$induction_fab_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_fab_hospital)
    }
    
    dat<- unified_data %>% filter(Treatment=="SDC")
    gen_corr_table(dat,by="induction2_outcome","FAB Classification","FAB_category","SDC Induction2")
  })
  
  # mutations
  
  output$mutation_induction1_LDC <- renderUI({
    
    if(input$induction_mutation_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_mutation_hospital)
    }
    
    dat1<- gene_mutations %>% filter(Treatment=="LDC"  )
    gen_corr_table_mutation(dat1,by="induction1_outcome","Gene Mutaiton","Mutation","LDC Induction1")
  })
  
  output$mutation_induction2_LDC <- renderUI({
    
    if(input$induction_mutation_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_mutation_hospital)
    }
    
    dat2<- gene_mutations %>% filter(Treatment=="LDC")
    gen_corr_table_mutation(dat2,by="induction2_outcome","Gene Mutaiton","Mutation","LDC Induction2")
  })
  
  output$mutation_induction1_SDC <- renderUI({
    
    if(input$induction_mutation_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_mutation_hospital)
    }
    
    dat3<- gene_mutations %>% filter(Treatment=="SDC")
    gen_corr_table_mutation(dat3,by="induction1_outcome","Gene Mutaiton","Mutation","SDC Induction1")
  })
  
  output$mutation_induction2_SDC <- renderUI({
    
    if(input$induction_mutation_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_mutation_hospital)
    }
    
    dat4<- gene_mutations %>% filter(Treatment=="SDC")
    gen_corr_table_mutation(dat4,by="induction2_outcome","Gene Mutaiton","Mutation","SDC Induction2")
  })   
  
  
  output$induction_age<- renderPlot({
    
    if(input$induction_age_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_age_hospital)
    }
    
    mycomp <- list(c("CR","PR"),c("PR","NR"),c("CR","NR"))
    
    age_data_LDC<- unified_data %>% select(age,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="LDC")
    ind1ldc<-myggbox(age_data_LDC,x = "induction1_outcome",y = "age","Induction1","Age","Induction1 LDC",mycomp,input$age_ind_palette)
    ind2ldc<-myggbox(age_data_LDC,x = "induction2_outcome",y = "age","Induction2","Age","Induction2 LDC",mycomp,input$age_ind_palette)
    
    age_data_SDC<- unified_data %>% select(age,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="SDC")
    ind1sdc<-myggbox(age_data_SDC,x = "induction1_outcome",y = "age","Induction1","Age","Induction1 SDC",mycomp,input$age_ind_palette)
    ind2sdc<-myggbox(age_data_SDC,x = "induction2_outcome",y = "age","Induction2","Age","Induction2 SDC",mycomp,input$age_ind_palette)
    
    ggpubr::ggarrange(ind1ldc,ind1sdc,ind2ldc,ind2sdc ,ncol=2,nrow=2,labels = "AUTO")
    
  })
  
  output$induction_wbc <- renderPlot({
    
    if(input$induction_wbc_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_wbc_hospital)
    }
    
    
    mycomp <- list(c("CR","PR"),c("PR","NR"),c("CR","NR"))
    
    unified_data$wbc <- sample(1:100,size = nrow(unified_data),replace = T)
    
    age_data_LDC<- unified_data %>% select(wbc,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="LDC")
    ind1ldc<-myggbox(age_data_LDC,x = "induction1_outcome",y = "wbc","Induction1","White Blood Cell Count","Induction1 LDC",mycomp,input$wbc_ind_palette)
    ind2ldc<-myggbox(age_data_LDC,x = "induction2_outcome",y = "wbc","Induction2","White Blood Cell Count","Induction2 LDC",mycomp,input$wbc_ind_palette)
    
    age_data_SDC<- unified_data %>% select(wbc,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="SDC")
    ind1sdc<-myggbox(age_data_SDC,x = "induction1_outcome",y = "wbc","Induction1","White Blood Cell Count","Induction1 SDC",mycomp,input$wbc_ind_palette)
    ind2sdc<-myggbox(age_data_SDC,x = "induction2_outcome",y = "wbc","Induction2","White Blood Cell Count","Induction2 SDC",mycomp,input$wbc_ind_palette)
    
    ggpubr::ggarrange(ind1ldc,ind1sdc,ind2ldc,ind2sdc,ncol=2,nrow=2,labels = "AUTO")
    
    
  })
  
  output$induction_anc <- renderPlot({
    
    
    if(input$induction_anc_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_anc_hospital)
    }
    
    mycomp <- list(c("CR","PR"),c("PR","NR"),c("CR","NR"))
    
    unified_data$anc <- sample(1:100,size = nrow(unified_data),replace = T)
    
    age_data_LDC<- unified_data %>% select(anc,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="LDC")
    ind1ldc<-myggbox(age_data_LDC,x = "induction1_outcome",y = "anc","Induction1","Absolute Neutrophil Count","Induction1 LDC",mycomp,input$anc_ind_palette)
    ind2ldc<-myggbox(age_data_LDC,x = " ",y = "anc","Induction2","Absolute Neutrophil Count","Induction2 LDC",mycomp,input$anc_ind_palette)
    
    age_data_SDC<- unified_data %>% select(anc,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="SDC")
    ind1sdc<-myggbox(age_data_SDC,x = "induction1_outcome",y = "anc","Induction1","Absolute Neutrophil Count","Induction1 SDC",mycomp,input$anc_ind_palette)
    ind2sdc<-myggbox(age_data_SDC,x = "induction2_outcome",y = "anc","Induction2","Absolute Neutrophil Count","Induction2 SDC",mycomp,input$anc_ind_palette)
    
    ggpubr::ggarrange(ind1ldc,ind1sdc,ind2ldc,ind2sdc ,ncol=2,nrow=2,labels = "AUTO")
    
    
  })
  
  output$induction_plt <- renderPlot({
    
    if(input$induction_plt_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_plt_hospital)
    }
    
    mycomp <- list(c("CR","PR"),c("PR","NR"),c("CR","NR"))
    
    unified_data$plt <- sample(1:100,size = nrow(unified_data),replace = T)
    
    age_data_LDC<- unified_data %>% select(plt,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="LDC")
    ind1ldc<-myggbox(age_data_LDC,x = "induction1_outcome",y = "plt","Induction1","Platelet","Induction1 LDC",mycomp,input$plt_ind_palette)
    ind2ldc<-myggbox(age_data_LDC,x = "induction2_outcome",y = "plt","Induction2","Platelet","Induction2 LDC",mycomp,input$plt_ind_palette)
    
    age_data_SDC<- unified_data %>% select(plt,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="SDC")
    ind1sdc<-myggbox(age_data_SDC,x = "induction1_outcome",y = "plt","Induction1","Platelet","Induction1 SDC",mycomp,input$plt_ind_palette)
    ind2sdc<-myggbox(age_data_SDC,x = "induction2_outcome",y = "plt","Induction2","Platelet","Induction2 SDC",mycomp,input$plt_ind_palette)
    
    ggpubr::ggarrange(ind1ldc,ind1sdc,ind2ldc,ind2sdc ,ncol=2,nrow=2,labels = "AUTO")
    
  })
  
  output$induction_hb <- renderPlot({
    
    
    if(input$induction_hb_hospital != 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$induction_hb_hospital)
    }
    
    mycomp <- list(c("CR","PR"),c("PR","NR"),c("CR","NR"))
    
    unified_data$hb <- sample(1:100,size = nrow(unified_data),replace = T)
    
    age_data_LDC<- unified_data %>% select(hb,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="LDC")
    ind1ldc<-myggbox(age_data_LDC,x = "induction1_outcome",y = "hb","Induction1","Hemoglobin","Induction1 LDC",mycomp,input$hb_ind_palette)
    ind2ldc<-myggbox(age_data_LDC,x = "induction2_outcome",y = "hb","Induction2","Hemoglobin","Induction2 LDC",mycomp,input$hb_ind_palette)
    
    age_data_SDC<- unified_data %>% select(hb,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment=="SDC")
    ind1sdc<-myggbox(age_data_SDC,x = "induction1_outcome",y = "hb","Induction1","Hemoglobin","Induction1 SDC",mycomp,input$hb_ind_palette)
    ind2sdc<-myggbox(age_data_SDC,x = "induction2_outcome",y = "hb","Induction2","Hemoglobin","Induction2 SDC",mycomp,input$hb_ind_palette)
    
    ggpubr::ggarrange(ind1ldc,ind1sdc,ind2ldc,ind2sdc ,ncol=2,nrow=2,labels = "AUTO")
    
  })
  
  
  # long-term outcome versus induction  ----------------------------------------------------------------------------
  ## Survival ~ Sex
  output$Survival_sex <- renderPlot({
    
    
    if(input$survival_sex_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_sex_hospital)
    }
    
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")
    fitsldcos <-survfit(Surv(OS,Status_OS)~sex_category,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$sex_surv_palette,
                            legend.labs=c("男","女"))
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~sex_category,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$sex_surv_palette,
                             legend.labs=c("男","女"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~sex_category,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$sex_surv_palette,
                             legend.labs=c("男","女"),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")
    fitssdc <-survfit(Surv(OS,Status_OS)~sex_category,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$sex_surv_palette,
                            legend.labs=c("男","女"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~sex_category,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$sex_surv_palette,
                             legend.labs=c("男","女"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~sex_category,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$sex_surv_palette,fun="event",
                             legend.labs=c("男","女"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
  })
  
  ## Survival~ risk
  output$Survival_risk <- renderPlot({
    
    if(input$survival_risk_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_risk_hospital)
    }
    
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")
    fitsldcos <-survfit(Surv(OS,Status_OS)~aml_risk_category,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$risk_surv_palette,
                            legend.labs=c("低危险度","中危险度","高危险度","无法分类"))
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~aml_risk_category,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$risk_surv_palette,
                             legend.labs=c("低危险度","中危险度","高危险度","无法分类"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~aml_risk_category,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$risk_surv_palette,
                             legend.labs=c(c("低危险度","中危险度","高危险度","无法分类")),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")
    fitssdc <-survfit(Surv(OS,Status_OS)~aml_risk_category,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$risk_surv_palette,
                            legend.labs=c("低危险度","中危险度","高危险度","无法分类"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~aml_risk_category,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$risk_surv_palette,
                             legend.labs=c("低危险度","中危险度","高危险度","无法分类"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~aml_risk_category,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$risk_surv_palette,fun="event",
                             legend.labs=c("低危险度","中危险度","高危险度","无法分类"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
  })
  
  ## Survival~ fab classification
  output$Survival_fab <- renderPlot({
    
    
    if(input$survival_fab_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_fab_hospital)
    }
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")
    fitsldcos <-survfit(Surv(OS,Status_OS)~FAB_category,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$fab_surv_palette,
                            legend.labs=as.character(unique(dat_LDC$FAB_category)))
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~FAB_category,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$fab_surv_palette,
                             legend.labs=as.character(unique(dat_LDC$FAB_category)))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~FAB_category,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$fab_surv_palette,
                             legend.labs=as.character(unique(dat_LDC$FAB_category)),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")
    fitssdc <-survfit(Surv(OS,Status_OS)~FAB_category,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$fab_surv_palette,
                            legend.labs=as.character(unique(dat_SDC$FAB_category)))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~FAB_category,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$fab_surv_palette,
                             legend.labs=as.character(unique(dat_SDC$FAB_category)))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~FAB_category,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$fab_surv_palette,fun="event",
                             legend.labs=as.character(unique(dat_SDC$FAB_category)))
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
  })
  
  ## Survival~ gene mutation
  output$Survival_mutations <- renderPlot({
    
    
    
    if(input$gene_survival_hospital != 0){
      print("select running")
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$gene_survival_hospital)
    }
    
    
    print(input$gene_survival_hospital)
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC") %>% dplyr::select(OS,Status_OS,EFS,Status_EFS,RFS,Status_RFS,input$genes) %>% rename("Mutation"=input$genes)
    fitsldcos <-survfit(Surv(OS,Status_OS)~Mutation,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$mutation_surv_palette,
                            legend.labs=c("阳性","阴性"))
    

    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~Mutation,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$mutation_surv_palette,
                             legend.labs=c("阳性","阴性"))
    
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~Mutation,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$mutation_surv_palette,
                             legend.labs=c("阳性","阴性"),fun="event")
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC") %>% dplyr::select(OS,Status_OS,EFS,Status_EFS,RFS,Status_RFS,input$genes) %>% rename("Mutation"=input$genes)
    fitssdc <-survfit(Surv(OS,Status_OS)~Mutation,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$mutation_surv_palette,
                            legend.labs=c("阳性","阴性"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~Mutation,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$mutation_surv_palette,
                             legend.labs=c("阳性","阴性"))
    fitssdcrfs <- survfit(Surv(RFS,Status_RFS)~Mutation,data=dat_SDC)
    ggssdcrfs <- ggsurvplot(fitssdcrfs,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Cumulative Incidence of Relapse",
                            palette =input$mutation_surv_palette,fun = "event",
                            legend.labs=c("阳性","阴性"))
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
  })
  
  output$survival_age <- renderPlot({
    if(input$survival_age_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_age_hospital)
    }
    
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(age<=input$bins,"Low","High")))
    #dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(age<=9,"Low","High")))
    
    fitsldcos <-survfit(Surv(OS,Status_OS)~grps,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$age_surv_palette,
                            legend.labs=c("Low","High"))
    
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$age_surv_palette,
                             legend.labs=c("Low","High"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$age_surv_palette,
                             legend.labs=c("Low","High"),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")%>% mutate(grps = factor(ifelse(age<=input$bins,"Low","High")))
    fitssdc <-survfit(Surv(OS,Status_OS)~grps,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$age_surv_palette,
                            legend.labs=c("Low","High"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$age_surv_palette,
                             legend.labs=c("Low","High"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$age_surv_palette,fun = "event",
                             legend.labs=c("Low","High"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
  })
  
  output$survival_wbc <- renderPlot({
    if(input$survival_wbc_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_wbc_hospital)
    }
    
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(wbc<=input$bins,"Low","High")))
    #dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(wbc<=9,"Low","High")))
    
    fitsldcos <-survfit(Surv(OS,Status_OS)~grps,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$wbc_surv_palette,
                            legend.labs=c("Low","High"))
    
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$wbc_surv_palette,
                             legend.labs=c("Low","High"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$wbc_surv_palette,
                             legend.labs=c("Low","High"),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")%>% mutate(grps = factor(ifelse(wbc<=input$bins,"Low","High")))
    fitssdc <-survfit(Surv(OS,Status_OS)~grps,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$wbc_surv_palette,
                            legend.labs=c("Low","High"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$wbc_surv_palette,
                             legend.labs=c("Low","High"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$wbc_surv_palette,fun = "event",
                             legend.labs=c("Low","High"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
    
  })
  
  output$survival_anc<-renderPlot({
    if(input$survival_anc_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_anc_hospital)
    }
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(anc<=input$bins,"Low","High")))
    #dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(anc<=9,"Low","High")))
    
    fitsldcos <-survfit(Surv(OS,Status_OS)~grps,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$anc_surv_palette,
                            legend.labs=c("Low","High"))
    
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$anc_surv_palette,
                             legend.labs=c("Low","High"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$anc_surv_palette,
                             legend.labs=c("Low","High"),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")%>% mutate(grps = factor(ifelse(anc<=input$bins,"Low","High")))
    fitssdc <-survfit(Surv(OS,Status_OS)~grps,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$anc_surv_palette,
                            legend.labs=c("Low","High"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$anc_surv_palette,
                             legend.labs=c("Low","High"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$anc_surv_palette,fun = "event",
                             legend.labs=c("Low","High"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
    
    
  })
  
  output$survival_plt<-renderPlot({
    if(input$survival_plt_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_plt_hospital)
    }
    
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(plt<=input$bins,"Low","High")))
    #dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(wbc<=9,"Low","High")))
    
    fitsldcos <-survfit(Surv(OS,Status_OS)~grps,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$plt_surv_palette,
                            legend.labs=c("Low","High"))
    
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$plt_surv_palette,
                             legend.labs=c("Low","High"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$plt_surv_palette,
                             legend.labs=c("Low","High"),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")%>% mutate(grps = factor(ifelse(plt<=input$bins,"Low","High")))
    fitssdc <-survfit(Surv(OS,Status_OS)~grps,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$plt_surv_palette,
                            legend.labs=c("Low","High"))
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$plt_surv_palette,
                             legend.labs=c("Low","High"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$plt_surv_palette,fun = "event",
                             legend.labs=c("Low","High"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
    
  })
  
  output$survival_hb<-renderPlot({
    if(input$survival_hb_hospital!= 0){
      unified_data<- unified_data %>% dplyr::filter(hospital_id == input$survival_hb_hospital)
    }
    
    dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(hb<=input$bins,"Low","High")))
    #dat_LDC <- unified_data %>% filter(Treatment =="LDC")%>% mutate(grps = factor(ifelse(wbc<=9,"Low","High")))
    
    fitsldcos <-survfit(Surv(OS,Status_OS)~grps,data=dat_LDC)
    ggsldcos<-   ggsurvplot(fitsldcos,data = dat_LDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="LDC: Overall Survival",
                            palette =input$hb_surv_palette,
                            legend.labs=c("Low","High"))
    
    fitsldcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_LDC)
    ggsldcefs<-   ggsurvplot(fitsldcefs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Event Free Survival",
                             palette =input$hb_surv_palette,
                             legend.labs=c("Low","High"))
    fitsldcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_LDC)
    ggsldcrfs<-   ggsurvplot(fitsldcrfs,data = dat_LDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="LDC: Cumulative Incidence of Relapse",
                             palette =input$hb_surv_palette,
                             legend.labs=c("Low","High"),fun="event")
    
    
    
    
    dat_SDC <- unified_data %>% filter(Treatment =="SDC")%>% mutate(grps = factor(ifelse(hb<=input$bins,"Low","High")))
    fitssdc <-survfit(Surv(OS,Status_OS)~grps,data=dat_SDC)
    ggssdcos<-   ggsurvplot(fitssdc,data = dat_SDC,
                            pval = TRUE, conf.int = TRUE,
                            risk.table = TRUE, # Add risk table
                            risk.table.col = "strata", # Change risk table color by groups
                            linetype = "strata", # Change line type by groups
                            surv.median.line = "hv", # Specify median survival
                            ggtheme = theme_bw(), # Change ggplot2 theme
                            legend.title="SDC: Overall Survival",
                            palette =input$hb_surv_palette,
                            legend.labs=c("Low","High"))
    
    fitssdcefs <-survfit(Surv(EFS,Status_EFS)~grps,data=dat_SDC)
    ggssdcefs<-   ggsurvplot(fitssdcefs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Event Free Survival",
                             palette =input$hb_surv_palette,
                             legend.labs=c("Low","High"))
    fitssdcrfs <-survfit(Surv(RFS,Status_RFS)~grps,data=dat_SDC)
    ggssdcrfs<-   ggsurvplot(fitssdcrfs,data = dat_SDC,
                             pval = TRUE, conf.int = TRUE,
                             risk.table = TRUE, # Add risk table
                             risk.table.col = "strata", # Change risk table color by groups
                             linetype = "strata", # Change line type by groups
                             surv.median.line = "hv", # Specify median survival
                             ggtheme = theme_bw(), # Change ggplot2 theme
                             legend.title="SDC: Cumulative Incidence of Relapse",
                             palette =input$hb_surv_palette,fun = "event",
                             legend.labs=c("Low","High"))
    
    
    cowplot::plot_grid(
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggsldcos$plot,ggsldcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcefs$plot,ggsldcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggsldcrfs$plot,ggsldcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),
      
      cowplot::plot_grid(
        cowplot::plot_grid(ggssdcos$plot,ggssdcos$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcefs$plot,ggssdcefs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
        cowplot::plot_grid(ggssdcrfs$plot,ggssdcrfs$table+guides(color=F),ncol=1,rel_heights = c(2,1)),ncol=1
      ),ncol=2
    )
    
    
  })
  
  
}


