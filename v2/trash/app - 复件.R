
# LOAD PACKAGES
if(!suppressWarnings({require(shiny)})){print("Load Package shiny  Failed")}
if(!suppressWarnings({require(shinythemes)})){print("Load Package shinytheme  Failed")}
if(!suppressWarnings({require(ggplot2)})){print("Load Package ggplot2  Failed")}
if(!suppressWarnings({require(tidyverse)})){print("Load Package tidyverse Failed")}
if(!suppressWarnings({require(magrittr)})){print("Load Package magrittr Failed")}
if(!suppressWarnings({require(ggpubr)})){print("Load Package ggpubr Failed")}
if(!suppressWarnings({require(survminer)})){print("Load Package survminer Failed")}
if(!suppressWarnings({require(cowplot)})){print("Load Package cowplot Failed")}
if(!suppressWarnings({require(Gmisc)})){print("Load Package Gmisc Failed")}

# #library(shiny.router)
# # library(ggplot2)
# # library(tidyverse)
# library(magrittr)
# #library(table1)
# library(ggpubr)
# library(survminer)
# library(cowplot)
# library(readxl)
# library(Hmisc)
# library(Gmisc)


# Style all the table output using htmlTable's theme handler
htmlTable::setHtmlTableTheme(css.rgroup = "")
#options(htmlTable.cat=T)

# supress all warnings information
options(warn = -1)

setwd("E:/projects/git_repo/aml_database_analysis_shinyapp/v2")


source("./data_prepar.r",encoding = "utf-8")
source("./helper_func.r",encoding = "utf-8")
source("./constant_vars.r",encoding = "utf-8")

data_list <- data_prepare()
unified_data <- data_list$unified_data

# treat_group <- unified_data %>% dplyr::select(Treatment) %>% 
#   group_by(Treatment) %>% summarise(Num=n()) %>% column_to_rownames(var = "Treatment") %>%t()


#unified_data$Treatment
# SERVER -----------------------------------
server <- function(input, output) {
  
  
  
}


# UI -----------------------------------
ui <- shiny::navbarPage("临床数据实时分析系统(demo2)",theme=shinytheme("flatly"),fluid=FALSE,
                        ## 数据概览 ---------------
                        
                        
                        tabPanel("患者数据概览",
                                 
                                 fixedPage(
                                   verticalLayout(
                                     ### 数据概览的总体选择窗口 ----
                                     wellPanel(style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
                                               wellPanel(
                                                 
                                                 
                                                 checkboxGroupInput("treatGroup1", label = h5(strong("选择治疗组")),
                                                                    choices = (unified_data$Treatment %>% unique() %>% as.character() %>% as.list()),
                                                                    selected = 1,inline=T,width = "100%"),
                                                 tags$style(type="text/css", HTML("#treatGroup1> .checkbox-inline{width:30px}")),
                                                 selectInput("clincical_palette1","配色",choices = palette_chn)
                                                 
                                               ),
                                               actionButton(inputId = "treatGroup1_analysis",label = "Analyze all with default settings",width = "100%")
                                               
                                     ),
                                     
                                     # 下方选项卡 ----
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               actionButton(inputId = "treatGroup1_downloads",label = "Download",width = "15%"),
                                               tags$style(type="text/css", HTML("#treatGroup1_downloads {float: right;}")),
                                               
                                               
                                               
                                               tabsetPanel(
                                                 tabPanel("患者基线数据概览",
                                                          
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          
                                                          width="100%",
                                                          mainPanel(
                                                            htmlOutput("Table1",inline = T),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("临床疗效",
                                                          
                                                          mainPanel(
                                                            tags$h3("诱导缓解疗程"),
                                                            htmlOutput("short_outcome",inline = T),
                                                            tags$h3("长期生存数据"),
                                                            plotOutput("Survival",width = "100%",height = "1000px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("复发/死亡/移植",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          mainPanel(
                                                            tags$h3("死亡、复发、骨髓移植"),
                                                            htmlOutput("deathrelapsetransplant",inline = T),
                                                            width=12
                                                          )
                                                 ),
                                                 tabPanel("副作用与花费",
                                                          
                                                          mainPanel(
                                                            tags$h3("副作用与花费"),
                                                            htmlOutput("sideeffect",inline = T),
                                                            htmlOutput("costs",inline = T),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("血常规时序数据展示",
                                                          
                                                          mainPanel(
                                                            tags$h3("治疗过程中血常规数据时序展示"),
                                                            plotOutput("RoutineBlood",height = "1000px"),
                                                            width = 12
                                                          )
                                                 )
                                               )
                                               
                                     )
                                     
                                     
                                     
                                   )
                                 )
                        ),
                        ## 诱导缓解疗效 --------------
                        tabPanel("临床因素与诱导缓解疗效",
                                 fixedPage(
                                   shiny::verticalLayout(
                                     
                                     wellPanel(style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
                                               wellPanel(
                                                 
                                                 
                                                 checkboxGroupInput("treatGroup1", label = h5(strong("选择治疗组")),
                                                                    choices = (unified_data$Treatment %>% unique() %>% as.character() %>% as.list()),
                                                                    selected = 1,inline=T,width = "100%"),
                                                 tags$style(type="text/css", HTML("#treatGroup1> .checkbox-inline{width:30px}")),
                                                 selectInput("clincical_outcome_palette","配色",choices = palette_chn)
                                                 
                                               ),
                                               actionButton(inputId = "treatGroup1_analysis",label = "Analyze all with default settings",width = "100%")
                                               
                                     ),
                                     
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               actionButton(inputId = "treatGroup1_downloads",label = "Download",width = "15%"),
                                               tags$style(type="text/css", HTML("#treatGroup1_downloads {float: right;}")),
                                               
                                               tabsetPanel(
                                                 tabPanel("Sex",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
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
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          mainPanel(
                                                            tags$h3("年龄与诱导缓解疗效的关系"),
                                                            plotOutput("induction_age",width = "100%",height = "900px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("WBC",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          mainPanel(
                                                            tags$h3("White Blood Cell Count与诱导缓解疗效的关系"),
                                                            plotOutput("induction_wbc",width = "100%",height = "900px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("ANC",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          mainPanel(
                                                            tags$h3("Absolute Neutrophil Count与诱导缓解疗效的关系"),
                                                            plotOutput("induction_anc",width = "100%",height = "900px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("PLT",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          
                                                          mainPanel(
                                                            tags$h3("Platelet与诱导缓解疗效的关系"),
                                                            plotOutput("induction_plt",width = "100%",height = "900px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("HB",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          
                                                          mainPanel(
                                                            tags$h3("Hemoglobin与诱导缓解疗效的关系"),
                                                            plotOutput("induction_hb",width = "100%",height = "900px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Risk Group",
                                                          
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
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
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
                                 )
                        ),
                        ## 长期生存------------------------------
                        tabPanel("临床因素与长期生存疗效",
                                 fixedPage(
                                   shiny::verticalLayout(
                                     
                                     wellPanel(style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
                                               wellPanel(
                                                 
                                                 checkboxGroupInput("treatGroup1", label = h5(strong("选择治疗组")),
                                                                    choices = (unified_data$Treatment %>% unique() %>% as.character() %>% as.list()),
                                                                    selected = 1,inline=T,width = "100%"),
                                                 tags$style(type="text/css", HTML("#treatGroup1> .checkbox-inline{width:30px}")),
                                                 selectInput("clincical_outcome_palette","配色",choices = palette_chn)
                                                 
                                               ),
                                               actionButton(inputId = "treatGroup1_analysis",label = "Analyze all with default settings",width = "100%")
                                               
                                     ),
                                     
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               actionButton(inputId = "treatGroup1_downloads",label = "Download",width = "15%"),
                                               tags$style(type="text/css", HTML("#treatGroup1_downloads {float: right;}")),
                                               
                                               shiny::tabsetPanel(
                                                 tabPanel("Sex",
                                                          # wellPanel(
                                                          #   # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          
                                                          mainPanel(
                                                            tags$h3("性别与长期生存的关系"),
                                                            plotOutput("Survival_sex",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Age",
                                                          
                                                          wellPanel(
                                                            # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                            
                                                            sliderInput("bins", label = "年龄分组", min =  min(na.omit(unified_data$age)), 
                                                                        max = max(na.omit(unified_data$age)), value = median(na.omit(unified_data$age))),
                                                            actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          ),
                                                          mainPanel(
                                                            tags$h3("年龄与长期生存的关系"),
                                                            
                                                            plotOutput("survival_age",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("WBC",
                                                          
                                                          
                                                          wellPanel(
                                                            # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                            sliderInput("bins", label = "White Blood Cell Count分组", min =  min(na.omit(unified_data$wbc)), 
                                                                        max = max(na.omit(unified_data$wbc)), value = median(na.omit(unified_data$wbc))),
                                                            actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          
                                                          mainPanel(
                                                            tags$h3("White Blood Cell Count与长期生存的关系"),
                                                            
                                                            plotOutput("survival_wbc",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("ANC",
                                                          wellPanel(
                                                            sliderInput("bins", label = "Absolute Neutrophil Count分组", min =  min(na.omit(unified_data$anc)), 
                                                                        max = max(na.omit(unified_data$anc)), value = median(na.omit(unified_data$anc))),
                                                            actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          ),
                                                          mainPanel(
                                                            tags$h3("Absolute Neutrophil Count与长期生存的关系"),
                                                            
                                                            plotOutput("survival_anc",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("PLT",
                                                          
                                                          wellPanel(
                                                            # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                            sliderInput("bins", label = "Platete分组", min =  min(na.omit(unified_data$plt)),
                                                                        max = max(na.omit(unified_data$plt)), value = median(na.omit(unified_data$plt))), 
                                                            actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          mainPanel(
                                                            tags$h3("Platelet与长期生存的关系"),
                                                            
                                                            plotOutput("survival_plt",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("HB",
                                                          
                                                          wellPanel(
                                                            # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                            sliderInput("bins", label = "Hemoglobin分组", min =  min(na.omit(unified_data$hb)),
                                                                        max = max(na.omit(unified_data$hb)), value = median(na.omit(unified_data$hb))), 
                                                            actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          mainPanel(
                                                            tags$h3("Hemoglobin与长期生存的关系"),
                                                            
                                                            plotOutput("survival_hb",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Risk Group",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          mainPanel(
                                                            tags$h3("AML 危险度与长期生存的关系"),
                                                            plotOutput("Survival_risk",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("FAB Classification",
                                                          # wellPanel(
                                                          #   selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                          #   actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                          #   
                                                          # ),
                                                          mainPanel(
                                                            tags$h3("FAB分型与长期生存的关系"),
                                                            plotOutput("Survival_fab",width = "100%",height = "1500px"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Gene Mutation",
                                                          
                                                          wellPanel(
                                                            # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                            selectInput("genes", "基因突变：", 
                                                                        choices=c("CKIT"="CKIT","NPM1"="NPM1","NRAS"="NRAS","ASXL1"="ASXL1","CEBPA"="CEBPA")
                                                            ),
                                                            actionButton(inputId = "treatGroup1_analysis",label = "Re-Analyze",width = "100%")
                                                            
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
                        ),
                        ## 说明文档 -----------------------
                        tabPanel("说明文档",
                                 wellPanel(style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
                                           includeHTML("./document.html")
                                           
                                 )
                        )
                        
)  

shinyApp(ui = ui, server = server)


