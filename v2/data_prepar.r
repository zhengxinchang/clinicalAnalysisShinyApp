
library(tidyverse)
library(magrittr)

#setwd("E:/projects/git_repo/aml_database_analysis_shinyapp/v2")

source("./data_validation.r",encoding = "utf-8")



data_prepare <- function(){
  
  PatientDF <- requestAndCleanDataframePatient()
  
  InitialDF <- requestAndCleanDataframeInitial()
  
  PERIOD <- getJsonPeroid()
  
  Induction1 <- requestAndCleanDataframeInduction1(PERIOD = PERIOD)
  
  Induction2 <- requestAndCleanDataframeInduction2(PERIOD = PERIOD)
  
  Survival <- requestAndCleanDataframeArchive()
  
  Series <- requestAndCleanDataframeSeriesData(PERIOD = PERIOD)
  
  # PREPROCESSING & INTEGRATE DATA ------------------------------------------
  
  ## select baseline data from initial dataset
  ## CKIT = mt_ckit
  ## NRAS = mt_nras
  ## CEBPA = mt_cepb
  ## ASXL1 = mt_asxl1
  ## NPM1 = mt_npm1
  ## reformat patientid anc,wbc,plt,hb to numeric 
  base_line_data <-InitialDF$data %>% 
    dplyr::select(patient_id,anc,wbc,plt,hb, mt_ckit,mt_nras,mt_cebp,mt_asxl1,mt_npm1) %>%
    rename("CKIT"="mt_ckit","NRAS"="mt_nras","CEBPA"="mt_cebp","ASXL1"="mt_asxl1","NPM1"="mt_npm1") %>%
    mutate_at(.vars = vars("patient_id",'anc',"wbc","plt",'hb'),.funs = as.double)
  
  
  ## left join baseline data to patient data
  ## left_join() is not safe when right table have multiple record hit with left table,  !!! just for demonstration !!!
  ## [unified_data] a datafrmae contaions all information including patients basic information, data from diagnosis, data during treatment, and so on.
  ## this step also convert teatment code to treatment name
  ## this step also rename FAB column
  unified_data <- PatientDF$data %>% 
    left_join(base_line_data,by = "patient_id")  %>%
    mutate_at(.vars = vars("sex"),.funs = as.character) %>% 
    rename("Treatment"="aml_group")  %>%
    rename("FAB"="aml_classify") 
  
  
  ## add hospital_name column to store hospital name
  unified_data$hospital_name<-recode_factor(as.factor(unified_data$hospital_id),
                                            `0`="所有医院", 
                                            `1`="苏州儿童医院", 
                                            `2`="安徽省立医院", 
                                            `3`="徐州市儿童医院", 
                                            `4`="山东大学齐鲁医院", 
                                            `5`="广西医科大学附属第一医院", 
                                            `6`="复旦大学附属儿科医院", 
                                            `7`="浙江大学附属儿童医院", 
                                            `8`="郑州大学第一附属医院", 
                                            `9`="中南大学湘雅医院", 
                                            `10`="广州市妇女儿童医疗中心", 
                                            `11`="安徽医科大学附属第二医院", 
                                            `12`="上海交通大学附属新华医院", 
                                            `13`="开封市儿童医院",.default=NA_character_)
  
  
  
  # recode FAB
  unified_data$FAB_category <- recode_factor(as.factor(unified_data$FAB),
                                             `0`="所有亚型",
                                             `1`="M0",
                                             `2`="M1",
                                             `3`="M2",
                                             `4`="M3",
                                             `5`="M4",
                                             `6`="M5",
                                             `7`="M6",
                                             `8`="M7",
                                             `9`="MDS RAEB-t",
                                             `10`="MDS RAEB",
                                             `11`="无法分类",
                                             .default=NA_character_)
  
  
  ## recode sex
  unified_data$sex_category<- recode_factor(as.factor(unified_data$sex),
                                            `0`="Male",
                                            `1`="Female",
                                            .default=NA_character_)
  
  ## recode aml risk group
  unified_data$aml_risk_category<- recode_factor(as.factor(unified_data$aml_risk),
                                                 `1`="Low_Risk",
                                                 `2`="Middle_Risk",
                                                 `3`="High_Risk",
                                                 `4`="Unclassified", # what does 4th grade mean?
                                                 .default=NA_character_)
  
  
  # LDC("LDC", 0),
  # SDC("SDC", 1),
  # NON_RANDOM_LDC("非随机LDC", 2),
  # NON_RANDOM_SDC("非随机SDC", 3),
  # NON_RANDOM_RR_A("RR-A", 4),
  # NON_RANDOM_RR_B("RR-B", 5),
  # NON_RANDOM_RR_C("RR-C", 6),
  # NON_RANDOM_RR_D("RR-D", 7),
  # NON_RANDOM_LDC_M7("LDC-M7", 8);
  unified_data$Treatment<- recode_factor(as.factor(unified_data$Treatment),
                                         `0`="LDC",
                                         `1`="SDC",
                                         `2`="NonRandom_LDC",
                                         `3`="NonRandom_SDC",
                                         `4`="RR_A", 
                                         `5`="RR_B",
                                         `6`="RR_C",
                                         `7`="RR_D",
                                         `8`="LDC_M7",
                                         .default=NA_character_)
  
  ## calculate patients' age
  unified_data$birthdate <- as.numeric(unified_data$birthday)
  unified_data$diagdate <- as.numeric(unified_data$diagnosis_date)
  for(i in seq(1,nrow(unified_data))){
    tryCatch({
      unified_data$age[i]<-as.numeric(
        round((unified_data$diagdate[i]-unified_data$birthdate[i])/365,2)
      )
    },error=function(e){
      return(NA)
    },warning = function(w){
      return(NA)
    })
  }
  
  
  ## Period data summary
  ## period indicates the xth treatment cycle.
  ## nthday indicates the day of specific treatment, -1 is the patient basic information
  ## subset induction1 data
  induction1<-Induction1$data
  
  ## subset induction2 data
  induction2<-Induction2$data
  
  ## add induction1 and induction2 to unified_data
  unified_data <-  unified_data %>% left_join(induction1,by=c("patient_id"="induction1_patient_id"))        
  unified_data <-  unified_data %>% left_join(induction2,by=c("patient_id"="induction2_patient_id"))        
  
  unified_data$induction1_outcome <-  recode(unified_data$induction1_d26_marrow_estimate,
                                             `1`="CR",
                                             `2`="CRi",
                                             `3`="PR",
                                             `4`="NR",
                                             .default=NA_character_,
                                             .missing=NA_character_)
  unified_data$induction2_outcome <-  recode(unified_data$induction2_d26_marrow_estimate,
                                             `1`="CR",
                                             `2`="CRi",
                                             `3`="PR",
                                             `4`="NR",
                                             .default=NA_character_,
                                             .missing=NA_character_)

  
  
  ## reformat all factor varialbes and logical variables
  ## todo , finally the short peroid outcome will be assign to CR PR NR CRi 
  
  unified_data$induction1_outcome <- factor(unified_data$induction1_outcome,levels = c("CR","PR","NR"))
  unified_data$induction2_outcome <- factor(unified_data$induction2_outcome,levels=c("CR","PR","NR"))
  
  
  ## replace with realdata update date : 2020年11月30日
  
  ## ===
  ## Surv() 函数，1代表事件发生0代表事件没发生
  ## join data with survival data
  unified_data  <-  unified_data %>% select( -is_relapse  )  %>% left_join(Survival,by=c("patient_id"="patient_id")) 
  
  # 将逻辑值转化为0、1状态，1代表事件发生，则代表死亡,0代表的是
  unified_data$OS<- as.numeric(unified_data$relapse_date) - as.numeric(unified_data$diagnosis_date)
  unified_data$Status_OS<- as.numeric(unified_data$is_dead)
  
  
  
  # EFS 这里先这么做，后期修改这个数据以匹配临床定义（EFS1,EFS2）
  unified_data$EFS<- sample(x = seq(1,365*5),replace = T,size = nrow(unified_data))
  unified_data$Status_EFS<- sample(x = seq(1,1,2),replace = T,size = nrow(unified_data))
  
  
  # 后期需要修改，目前还是使用简单的EFS曲线的倒过来（EFS以复发为EVENT)
  unified_data$RFS<- as.numeric(unified_data$relapse_date) - as.numeric(unified_data$diagnosis_date)
  unified_data$Status_RFS<- as.numeric(unified_data$is_relapse)
  
  
  ## reformating side effect and cost colums
  
  unified_data$induction1_anc_recover_time<-as.numeric(unified_data$induction1_anc_recover_time)
  unified_data$induction2_anc_recover_time<-as.numeric(unified_data$induction2_anc_recover_time)
  unified_data$induction1_plt_recover_time <- as.numeric(unified_data$induction1_plt_recover_time)
  unified_data$induction2_plt_recover_time <- as.numeric(unified_data$induction2_plt_recover_time)
  unified_data$induction1_period_cost<-as.numeric(unified_data$induction1_period_cost)
  unified_data$induction2_period_cost<-as.numeric(unified_data$induction2_period_cost)
  
  
  
  ## defeine a variable "series_data" to take time series data such as wbc plt in each day
  
  series_data <-  Series$data
  
  series_data$Series_period_code<-series_data$Series_period_code %>% recode(`1`="Induction1",
                                                                            `2`="Induction2",
                                                                            `3`="Consolidation1",
                                                                            `4`="Consolidation2",
                                                                            `5`="Consolidation3",
                                                                            .default=NA_character_,
                                                                            .missing=NA_character_) 
  series_data<-series_data %>% filter(!is.na(Series_period_code)) 
  
  series_data$Series_wbc <- as.numeric(series_data$Series_wbc)
  series_data$Series_anc <- as.numeric(series_data$Series_anc)
  series_data$Series_hb <- as.numeric(series_data$Series_hb)
  series_data$Series_plt  <- as.numeric(series_data$Series_plt)
  series_data$Series_nth_day<-as.numeric(series_data$Series_nth_day)
  
  series_data_long<-series_data %>% 
    select(Series_patient_id,Series_period_code,Series_nth_day,Series_wbc,Series_plt,Series_hb,Series_anc) %>%
    tidyr::gather(key="Params",value="value",-c(Series_nth_day,Series_period_code,Series_patient_id)) %>% na.omit()
  
  patient_treat<- unified_data %>% select(patient_id,Treatment,hospital_id)
  
  series_data_long <- series_data_long %>% left_join(patient_treat,by=c("Series_patient_id"="patient_id")) %>% na.omit()
  
  # genrate simulation data

  
  # simulation data over
  
  gene_mutations<- unified_data %>% select(CKIT,NRAS,NPM1,CEBPA,ASXL1,induction1_outcome,induction2_outcome,Treatment) %>%
    gather(key="Mutation",value = "status",-c(induction1_outcome,induction2_outcome,Treatment)) %>% na.omit()
  
  
  return(list(unified_data=unified_data,series_data_long=series_data_long,gene_mutations = gene_mutations))
  
}

