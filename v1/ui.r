# LOAD PACKAGES

suppressWarnings({
  if(!require(shinythemes)) install.packages("shinythemes")
  library(shiny)
  library(shiny.router)
  library(ggplot2)
  library(tidyverse)
  library(magrittr)
})


options(device = function(file, width = 7, height = 7, ...) {
  windows(width = width, height = height, ...)
})

setwd("E:/projects/git_repo/aml_database_analysis_shinyapp/v2")


# Preprocess unifieddata --------------------------------------------------

patient_data <- readxl::read_excel("../tables_from_mysql/patient.xlsx")
initial_data <- readxl::read_excel("../tables_from_mysql/initial.xlsx")
period_data <- readxl::read_excel("../tables_from_mysql/period.xlsx")



options(warn = -1)
# PREPROCESSING & INTEGRATE DATA ------------------------------------------

## select baseline data from initial dataset
## CKIT = mt_ckit
## NRAS = mt_nras
## CEBPA = mt_cepb
## ASXL1 = mt_asxl1
## NPM1 = mt_npm1
## reformat patientid anc,wbc,plt,hb to numeric 
base_line_data <-initial_data %>% 
  dplyr::select(patient_id,anc,wbc,plt,hb, mt_ckit,mt_nras,mt_cebp,mt_asxl1,mt_npm1) %>%
  rename("CKIT"="mt_ckit","NRAS"="mt_nras","CEBPA"="mt_cebp","ASXL1"="mt_asxl1","NPM1"="mt_npm1") %>%
  mutate_at(.vars = vars("patient_id",'anc',"wbc","plt",'hb'),.funs = as.double)


## left join baseline data to patient data
## left_join() is not safe when right table have multiple record hit with left table,  !!! just for demonstration !!!
## [unified_data] a datafrmae contaions all information including patients basic information, data from diagnosis, data during treatment, and so on.
## this step also convert teatment code to treatment name
## this step also rename FAB column
unified_data <- patient_data %>% 
  left_join(base_line_data,by = "patient_id")  %>%
  mutate_at(.vars = vars("sex"),.funs = as.character) %>% 
  rename("Treatment"="aml_group") %>% filter(Treatment =="1" | Treatment=="0") %>% 
  mutate_at(.vars = vars("Treatment"),.funs = function(x){ifelse(x=="1","SDC","LDC")}) %>%
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
                                          `0`="男",
                                          `1`="女",
                                          .default=NA_character_)

## recode aml risk group
unified_data$aml_risk_category<- recode_factor(as.factor(unified_data$aml_risk),
                                               `1`="低危",
                                               `2`="中危",
                                               `3`="高危",
                                               `4`="无法分类", # what does 4th grade mean?
                                               .default=NA_character_)

## calculate patients' age
unified_data$birthdate <- as.Date(unified_data$birthday)
unified_data$diagdate <- as.Date(unified_data$diagnosis_date)
unified_data$age<-as.numeric(
  round(difftime(unified_data$diagdate,unified_data$birthday)/365,2)
)


## Period data summary
## period indicates the xth treatment cycle.
## nthday indicates the day of specific treatment, -1 is the patient basic information
## subset induction1 data
induction1<-period_data %>% dplyr::select(
  period_code, 
  period_id,
  d26_leukemia_percent, 
  period_cost,
  patient_id,
  nth_day,
  d26_marrow_estimate,
  is_transplant,
  weight,
  high,
  wbc,  # white blood count in each day
  blood_platelet, # platelet count in each day
  plt_recover_time, 
  anc_recover_time)%>%
  filter(period_code =="1" & nth_day  ==-1) %>% 
  arrange(patient_id) %>% rename_all(.funs = function(x){str_c("induction1",x,sep="_")})
## subset induction2 data
induction2<-period_data %>% dplyr::select(
  period_code, 
  period_id,
  d26_leukemia_percent, 
  period_cost,
  patient_id,
  nth_day,
  d26_marrow_estimate,
  is_transplant,
  weight,
  high,
  wbc,  # white blood count in each day
  blood_platelet, # platelet count in each day
  plt_recover_time, 
  anc_recover_time)%>%
  filter(period_code =="2" & nth_day  ==-1) %>% 
  arrange(patient_id) %>% rename_all(.funs = function(x){str_c("induction2",x,sep="_")})

## add induction1 and induction2 to unified_data
unified_data %<>% left_join(induction1,by=c("patient_id"="induction1_patient_id"))        
unified_data %<>% left_join(induction2,by=c("patient_id"="induction2_patient_id"))        
unified_data %<>% mutate(induction1_outcome=ifelse(as.numeric(induction1_d26_marrow_estimate)<=5,"CR",ifelse(as.numeric(induction1_d26_marrow_estimate)<20,"PR","NR")))
unified_data %<>% mutate(induction2_outcome=ifelse(as.numeric(induction2_d26_marrow_estimate)<=5,"CR",ifelse(as.numeric(induction2_d26_marrow_estimate)<20,"PR","NR")))

## reformat all factor varialbes and logical variables
unified_data$induction1_outcome <- factor(unified_data$induction1_outcome,levels = c("CR","PR","NR"))
unified_data$induction2_outcome <- factor(unified_data$induction2_outcome,levels=c("CR","PR","NR"))

## considerting that we does not have real followup data, we generate the simulated data for os efs and rfs
unified_data$OS<- sample(x = seq(1,365*5),replace = T,size = nrow(unified_data))
unified_data$Status_OS<- sample(x = seq(1,1,2),replace = T,size = nrow(unified_data))
unified_data$EFS<- sample(x = seq(1,365*5),replace = T,size = nrow(unified_data))
unified_data$Status_EFS<- sample(x = seq(1,1,2),replace = T,size = nrow(unified_data))
unified_data$RFS<- sample(x = seq(1,365*5),replace = T,size = nrow(unified_data))
unified_data$Status_RFS<- sample(x = seq(1,1,2),replace = T,size = nrow(unified_data))


## reformating side effect and cost colums

unified_data$induction1_anc_recover_time<-as.numeric(unified_data$induction1_anc_recover_time)
unified_data$induction2_anc_recover_time<-as.numeric(unified_data$induction2_anc_recover_time)
unified_data$induction1_plt_recover_time <- as.numeric(unified_data$induction1_plt_recover_time)
unified_data$induction2_plt_recover_time <- as.numeric(unified_data$induction2_plt_recover_time)
unified_data$induction1_period_cost<-as.numeric(unified_data$induction1_period_cost)
unified_data$induction2_period_cost<-as.numeric(unified_data$induction2_period_cost)

# DEFINE COMMON VARS ------------------------------------------------------

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

palette_chn = c( "Nature Publishing Group (NPG)"="npg", 
                 "The Lancet"="lancet", 
                 "Journal Clinical of Oncology"="jco", 
                 "JAMA"="jama", 
                 "The New England Journal of Medicine"="nejm")


# UI ----------------------------------------------------------------------


shiny::navbarPage("临床数据实时分析系统(demo2)",theme=shinytheme("flatly"),footer=list(hr(),tag("h5","连续变量使用Wilcox rank sum test; 分类变量使用Fisher Exact test; 生存率估计使用 Kaplan-Meier方法; 不同组生存曲线比较使用Log-rank test。")),

                  tabPanel("多中心临床试验数据概览",

                           
                           
                           fluidPage(
                             verticalLayout(
                               tabsetPanel(
                                 tabPanel("患者基线数据概览",
                                          sidebarPanel(
                                            selectInput("base_line_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            width=12
                                          ),
                                          mainPanel(
                                            htmlOutput("Table1",inline = T),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("临床疗效",
                                          sidebarPanel(
                                            selectInput("clincical_outocme_hospital","医院：",choices = hospital_names_chn),
                                            selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                            width = 12
                                          ),
                                          mainPanel(
                                            tags$h3("诱导缓解疗程"),
                                            htmlOutput("short_outcome",inline = T),
                                            tags$h3("长期生存数据"),
                                            plotOutput("Survival",width = "100%",height = "1000px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("复发/死亡/移植",
                                          sidebarPanel(
                                            selectInput("death_relap_transplant_hospital","医院：",choices=hospital_names_chn),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("死亡、复发、骨髓移植"),
                                            htmlOutput("deathrelapsetransplant",inline = T),
                                            width=12
                                          )
                                 ),
                                 tabPanel("副作用与花费",
                                          sidebarPanel(
                                            selectInput("sideeffect_costs_hospital","医院：",choices=hospital_names_chn),
                                            width=12  
                                          ),
                                          mainPanel(
                                            tags$h3("副作用与花费"),
                                            htmlOutput("sideeffect",inline = T),
                                            htmlOutput("costs",inline = T),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("血常规时序数据展示",
                                          sidebarPanel(
                                            selectInput("blood_rotine_hospital","医院：",choices=hospital_names_chn),
                                            width=12  
                                          ),
                                          mainPanel(
                                            tags$h3("治疗过程中血常规数据时序展示"),
                                            plotOutput("RoutineBlood",height = "1000px"),
                                            width = 12
                                          )
                                 )
                               )
                             )
                           )
                  ),
                  tabPanel("临床因素与诱导缓解疗效",
                           fluidPage(
                             shiny::verticalLayout(
                               tabsetPanel(
                                 tabPanel("Sex",
                                          sidebarPanel(
                                            selectInput("induction_sex_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("性别与诱导缓解疗效的关系"),
                                            column(12,
                                                   column(6,uiOutput("sex_induction1_LDC",inline = T),uiOutput("sex_induction2_LDC",inline = T)),
                                                   column(6,uiOutput("sex_induction1_SDC",inline = T),uiOutput("sex_induction2_SDC",inline = T))
                                            ),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("Age",
                                          sidebarPanel(
                                            selectInput("induction_age_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                                        
                                            ),
                                            selectInput("age_ind_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("年龄与诱导缓解疗效的关系"),
                                            plotOutput("induction_age",width = "100%",height = "900px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("WBC",
                                          sidebarPanel(
                                            selectInput("induction_wbc_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("wbc_ind_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("White Blood Cell Count与诱导缓解疗效的关系"),
                                            plotOutput("induction_wbc",width = "100%",height = "900px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("ANC",
                                          sidebarPanel(
                                            selectInput("induction_anc_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("anc_ind_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("Absolute Neutrophil Count与诱导缓解疗效的关系"),
                                            plotOutput("induction_anc",width = "100%",height = "900px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("PLT",
                                          sidebarPanel(
                                            selectInput("induction_plt_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("plt_ind_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          
                                          mainPanel(
                                            tags$h3("Platelet与诱导缓解疗效的关系"),
                                            plotOutput("induction_plt",width = "100%",height = "900px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("HB",
                                          sidebarPanel(
                                            selectInput("induction_hb_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("hb_ind_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          
                                          mainPanel(
                                            tags$h3("Hemoglobin与诱导缓解疗效的关系"),
                                            plotOutput("induction_hb",width = "100%",height = "900px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("Risk Group",
                                          sidebarPanel(
                                            selectInput("induction_risk_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("AML 危险度与诱导缓解疗效的关系"),
                                            column(12,
                                                   column(6,uiOutput("risk_induction1_LDC",inline = T),uiOutput("risk_induction2_LDC",inline = T)),
                                                   column(6,uiOutput("risk_induction1_SDC",inline = T),uiOutput("risk_induction2_SDC",inline = T))
                                            ),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("FAB Classification",
                                          sidebarPanel(
                                            selectInput("induction_fab_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("FAB分型与诱导缓解疗效的关系"),
                                            column(12,
                                                   column(6,uiOutput("fab_induction1_LDC",inline = T),uiOutput("fab_induction2_LDC",inline = T)),
                                                   column(6,uiOutput("fab_induction1_SDC",inline = T),uiOutput("fab_induction2_SDC",inline = T))
                                            ),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("Gene Mutation",
                                          sidebarPanel(
                                            selectInput("induction_mutation_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("基因突变与诱导缓解疗效的关系"),
                                            column(12,
                                                   column(6,uiOutput("mutation_induction1_LDC",inline = T),uiOutput("mutation_induction2_LDC",inline = T)),
                                                   column(6,uiOutput("mutation_induction1_SDC",inline = T),uiOutput("mutation_induction2_SDC",inline = T))
                                            ), 
                                            width = 12
                                          )
                                 )
                                 
                               )
                             )
                           )
                           
                           
                  ),
                  #===================================
                  tabPanel("临床因素与长期生存疗效",
                           fluidPage(
                             shiny::verticalLayout(
                               shiny::tabsetPanel(
                                 tabPanel("Sex",
                                          sidebarPanel(
                                            selectInput("survival_sex_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("sex_surv_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          
                                          mainPanel(
                                            tags$h3("性别与长期生存的关系"),
                                            plotOutput("Survival_sex",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("Age",
                                          sidebarPanel(
                                            selectInput("survival_age_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("age_surv_palette","配色",choices = palette_chn),
                                            sliderInput("bins", label = "年龄分组", min =  min(na.omit(unified_data$age)), 
                                                        max = max(na.omit(unified_data$age)), value = median(na.omit(unified_data$age))),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("年龄与长期生存的关系"),
                                            
                                            plotOutput("survival_age",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("WBC",
                                          sidebarPanel(
                                            selectInput("survival_wbc_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("wbc_surv_palette","配色",choices = palette_chn),
                                            sliderInput("bins", label = "White Blood Cell Count分组", min =  min(na.omit(unified_data$wbc)), 
                                                        max = max(na.omit(unified_data$wbc)), value = median(na.omit(unified_data$wbc))),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("White Blood Cell Count与长期生存的关系"),
                                            
                                            plotOutput("survival_wbc",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("ANC",
                                          sidebarPanel(
                                            selectInput("survival_anc_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("anc_surv_palette","配色",choices = palette_chn),
                                            sliderInput("bins", label = "Absolute Neutrophil Count分组", min =  min(na.omit(unified_data$anc)),
                                                        max = max(na.omit(unified_data$anc)), value = median(na.omit(unified_data$anc))), 
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("Absolute Neutrophil Count与长期生存的关系"),

                                            plotOutput("survival_anc",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("PLT",
                                          sidebarPanel(
                                            selectInput("survival_plt_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("plt_surv_palette","配色",choices = palette_chn),
                                            sliderInput("bins", label = "Platete分组", min =  min(na.omit(unified_data$plt)),
                                                        max = max(na.omit(unified_data$plt)), value = median(na.omit(unified_data$plt))), 
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("Platelet与长期生存的关系"),

                                            plotOutput("survival_plt",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("HB",
                                          sidebarPanel(
                                            selectInput("survival_hb_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("hb_surv_palette","配色",choices = palette_chn),
                                            sliderInput("bins", label = "Hemoglobin分组", min =  min(na.omit(unified_data$hb)),
                                                        max = max(na.omit(unified_data$hb)), value = median(na.omit(unified_data$hb))), 
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("Hemoglobin与长期生存的关系"),

                                            plotOutput("survival_hb",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("Risk Group",
                                          sidebarPanel(
                                            selectInput("survival_risk_hospital", "医院：", 
                                                        choices=hospital_names_chn,
                                            ),
                                            selectInput("risk_surv_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("AML 危险度与长期生存的关系"),
                                            plotOutput("Survival_risk",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("FAB Classification",
                                          sidebarPanel(
                                            selectInput("survival_fab_hospital", "医院：", 
                                                        choices=hospital_names_chn
                                            ),
                                            
                                            selectInput("fab_surv_palette","配色",choices = palette_chn),
                                            width=12
                                          ),
                                          mainPanel(
                                            tags$h3("FAB分型与长期生存的关系"),
                                            plotOutput("Survival_fab",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 ),
                                 tabPanel("Gene Mutation",
                                          sidebarPanel(
                                            selectInput("gene_survival_hospital", "医院：", selected=0, #这里最好起不同的名字，防止混淆
                                                        choices=hospital_names_chn
                                            ),
                                            
                                            selectInput("genes", "基因突变：", 
                                                        choices=c("CKIT"="CKIT","NPM1"="NPM1","NRAS"="NRAS","ASXL1"="ASXL1","CEBPA"="CEBPA")
                                            ),
                                            selectInput("mutation_surv_palette","配色",choices = palette_chn),
                                            
                                            width=12
                                          ),
                                          
                                          mainPanel(
                                            tags$h3("基因突变与长期生存的关系"),
                                            plotOutput("Survival_mutations",width = "100%",height = "1500px"),
                                            width = 12
                                          )
                                 )
                               )
                             )
                             
                           )
                           
                           
                  )
                  
                  
)  
