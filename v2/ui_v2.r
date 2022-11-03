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

source("./data_prepar.r",encoding = "utf-8")

data_list <- data_prepare()
unified_data <- data_list$unified_data
series_data <- data_list$series_data


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
                           
                           fixedPage(
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
                           fixedPage(
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
                           fixedPage(
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
