# LOAD PACKAGES----
if (!suppressWarnings({
  require(shiny)
})) {
  print("Load Package shiny  Failed")
}
if (!suppressWarnings({
  require(shinythemes)
})) {
  print("Load Package shinytheme  Failed")
}
if (!suppressWarnings({
  require(ggplot2)
})) {
  print("Load Package ggplot2  Failed")
}
if (!suppressWarnings({
  require(tidyverse)
})) {
  print("Load Package tidyverse Failed")
}
if (!suppressWarnings({
  require(magrittr)
})) {
  print("Load Package magrittr Failed")
}
if (!suppressWarnings({
  require(ggpubr)
})) {
  print("Load Package ggpubr Failed")
}
if (!suppressWarnings({
  require(survminer)
})) {
  print("Load Package survminer Failed")
}
if (!suppressWarnings({
  require(cowplot)
})) {
  print("Load Package cowplot Failed")
}
if (!suppressWarnings({
  require(Gmisc)
})) {
  print("Load Package Gmisc Failed")
}
if (!suppressWarnings({
  require(Hmisc)
})) {
  print("Load Package Hmisc Failed")
}
if (!suppressWarnings({
  require(cmprsk)
})) {
  print("Load Package cmprsk Failed")
}
if (!suppressWarnings({
  require(shinyWidgets)
})) {
  print("Load Package shinyWidgets Failed")
}
if (!suppressWarnings({
  require(waiter)
})) {
  print("Load Package shinyWidgets Failed")
}

htmlTable::setHtmlTableTheme(css.rgroup = "")
# supress all warnings information
options(warn = -1)
#setwd("E:/projects/git_repo/aml/src/main/webapp/shinyApp")
setwd("/srv/shiny-server/chemo2")
source("./data_prepar.r", encoding = "utf-8")
source("./helper_func.r", encoding = "utf-8")
source("./constant_vars.r", encoding = "utf-8")


# SERVER -----------------------------------

server <- function(input, output, session) {
  print(paste("Shiny App Start ... time: ", date(), sep = " "))
  
  # Server 端逻辑
  # 首先根据URL中传递的参数对患者进行筛选 ServerFuncSelectPatients.r
  # 之后生成reative事件 ServerFuncReactive.r
  # 最后进行渲染 ServerFuncRender.r

  # get data 放在这里，会让shiny页面立刻弹出，配合Waiter 包，实现先弹出，后等待
  data_list <- data_prepare()
  unified_data <- data_list$unified_data
  series_data_long <- data_list$series_data_long
  gene_mutation <- data_list$gene_mutations
  # 读取所有的患者，在Shiny中进行筛选
  source("./ServerFuncSelectPatients.r",local = T,encoding = "utf-8")
  
  
  # 隐藏选项卡
  hideTab(inputId = "nav", target = "结果")
  
  # 渲染所有的结果，在一个大的ObserveEvent(input$buttonClick) 中进行
  source("./ServerFuncRender.r",local = T,encoding = "utf-8")
  
  # 分别生成reactive模式
  source("./ServerFuncReactive.r",local = T,encoding = "utf-8")
  
  
  #下载功能
  source("./ServerFuncDownload.r",local = T,encoding = "utf-8")
  
  waiter_hide()
}

# UI -----------------------------------

ui <-
  shiny::navbarPage(
    title = "临床数据实时分析系统",
    theme = shinytheme("flatly"),
    fluid = FALSE,
    id = "nav",
    ## 选择方案疗法和配色 ----

    tabPanel(
      "选择治疗方案和配色",
      fixedPage(
        use_waiter(),
        waiter_show_on_load(html = spin_fading_circles()),
        tags$head(tags$script(src = "message-handler.js")),
        tags$style(type = "text/css", HTML(".download_css {float: right;}")),
        verticalLayout(
          wellPanel(
            style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
            wellPanel(
              uiOutput("dynamic_groups_table"),
              uiOutput("dynamic_groups_choice_group"),
              tags$style(type = "text/css", HTML(
                "#treatGroup1> .checkbox-inline{width:30px}"
              )),
              selectInput("clincical_outcome_palette", "配色", choices = palette_chn)
            ),
            actionButton(
              inputId = "treatGroup_analysis",
              label = "Analyze all with default settings",
              width = "100%"
            )
          )
        )
      )
    ),

    ## 结果 ----
    tabPanel("结果",
      id = "results",
      fixedPage(
        shiny::verticalLayout(
          verticalTabsetPanel(
            contentWidth = 11,
            verticalTabPanel(
              "概览",
              # tags$style(type = "text/css", HTML("#treatGroup1_downloads {float: right;}")),
              # actionButton(
              #   inputId = "treatGroup1_downloads",
              #   label = "Download",
              #   width = "10%"
              # ),
              tabsetPanel(
                tabPanel("患者基线数据概览",
                  width = "100%",
                  mainPanel(
                    downloadLink(
                      outputId   = "baseline_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("患者基线特征"),
                    
                    htmlOutput("Table1", inline = T),
                    width = 12
                  )
                ),
                tabPanel(
                  "临床疗效",
                  mainPanel(
                    downloadLink(
                      outputId   = "outcome_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("诱导缓解疗程"),
                    htmlOutput("short_outcome", inline = T),
                    tags$h3("长期生存数据"),
                    plotOutput("Survival", width = "100%", height = "1000px"),
                    width = 12
                  )
                ),
                tabPanel(
                  "复发/死亡/移植",
                  mainPanel(
                    downloadLink(
                      outputId   = "adverse_event_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("死亡、复发、骨髓移植"),
                    htmlOutput("deathrelapsetransplant", inline = T),
                    width = 12
                  )
                ),
                tabPanel(
                  "副作用与花费",
                  mainPanel(
                    downloadLink(
                      outputId   = "sideeffects_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("副作用与花费"),
                    htmlOutput("sideeffect", inline = T),
                    htmlOutput("costs", inline = T),
                    width = 12
                  )
                )
                # ,
                # tabPanel(
                #   "血常规时序数据展示",
                # 
                #   mainPanel(
                #     tags$h3("治疗过程中血常规数据时序展示"),
                #     plotOutput("RoutineBlood", height = "1000px"),
                #     width = 12
                #   )
                #   
                #   
                # )
              )
            ),
            verticalTabPanel(
              "临床因素~诱导缓解疗效",

              tabsetPanel(
                tabPanel(
                  "Sex",
                  mainPanel(
                    downloadLink(
                      outputId   = "sex_indction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("性别与诱导缓解疗效的关系"),
                    fixedRow(tags$div(
                      id =
                        "ind1_sex_div"
                    )),
                    fixedRow(tags$div(
                      id =
                        "ind2_sex_div"
                    )),
                    width = 12
                  )
                ),
                tabPanel(
                  "Age",
                  mainPanel(
                    downloadLink(
                      outputId   = "age_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("年龄与诱导缓解疗效的关系"),
                    tags$div(
                      id =
                        "induction_Age_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "WBC",
                  mainPanel(
                    downloadLink(
                      outputId   = "wbc_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("White Blood Cell Count与诱导缓解疗效的关系"),
                    tags$div(
                      id =
                        "induction_Wbc_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "ANC",
                  mainPanel(
                    downloadLink(
                      outputId   = "anc_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("Absolute Neutrophil Count与诱导缓解疗效的关系"),
                    tags$div(
                      id =
                        "induction_Anc_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "PLT",
                  mainPanel(
                    downloadLink(
                      outputId   = "plt_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("Platelet与诱导缓解疗效的关系"),
                    tags$div(
                      id =
                        "induction_Plt_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "HB",
                  mainPanel(
                    downloadLink(
                      outputId   = "hb_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("Hemoglobin与诱导缓解疗效的关系"),
                    tags$div(
                      id =
                        "induction_Hb_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "Risk Group",
                  mainPanel(
                    downloadLink(
                      outputId   = "risk_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("AML 危险度与诱导缓解疗效的关系"),
                    fixedRow(tags$div(
                      id =
                        "ind1_risk_div"
                    )),
                    fixedRow(tags$div(
                      id =
                        "ind2_risk_div"
                    )),
                    width = 12
                  )
                ),
                tabPanel(
                  "FAB Classification",
                  mainPanel(
                    downloadLink(
                      outputId   = "fab_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("FAB分型与诱导缓解疗效的关系"),
                    fixedRow(tags$div(
                      id =
                        "ind1_FAB_div"
                    )),
                    fixedRow(tags$div(
                      id =
                        "ind2_FAB_div"
                    )),
                    width = 12
                  )
                ),
                tabPanel(
                  "Gene Mutation",
                  mainPanel(
                    downloadLink(
                      outputId   = "mut_induction_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    fixedRow(tags$div(id = "ind1_GeneMut_div")),
                    fixedRow(tags$div(
                      id =
                        "ind2_GeneMut_div"
                    )),
                    width = 12
                  )
                )
              )
            ),
            verticalTabPanel(
              "临床因素~长期生存",
              # tags$style(type = "text/css", HTML("#treatGroup1_downloads {float: right;}")),
              # actionButton(
              #   inputId = "treatGroup1_downloads",
              #   label = "Download",
              #   width = "10%"
              # ),
              tabsetPanel(
                tabPanel(
                  "Sex",
                  mainPanel(
                    downloadLink(
                      outputId   = "sex_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("性别与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_sex_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "Age",
                  wellPanel(
                    # sliderInput(
                    #   "Surv_Age_bins",
                    #   label = "年龄分组",
                    #   min =  min(na.omit(subdat$age)),
                    #   max = max(na.omit(subdat$age)),
                    #   value = median(na.omit(subdat$age))
                    # ),
                    tags$div(
                      id ="SliderInputAge"
                      ),
                    # uiOutput("SliderInputAge"),
                    actionButton(
                      inputId = "button_Surv_Age",
                      label = "Re-Analyze",
                      width = "100%"
                    )
                  ),
                  mainPanel(
                    downloadLink(
                      outputId   = "age_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("年龄与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_age_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "WBC",
                  wellPanel(
                    # sliderInput(
                    #   "Surv_Wbc_bins",
                    #   label = "White Blood Cell Count分组",
                    #   min =  min(na.omit(unified_data$wbc)),
                    #   max = max(na.omit(unified_data$wbc)),
                    #   value = median(na.omit(unified_data$wbc))
                    # ),
                    tags$div(
                      id =
                        "SliderInputWbc"
                    ),
                    actionButton(
                      inputId = "button_Surv_Wbc",
                      label = "Re-Analyze",
                      width = "100%"
                    )
                  ),
                  mainPanel(
                    downloadLink(
                      outputId   = "wbc_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("White Blood Cell Count与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_wbc_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "ANC",
                  wellPanel(
                    # sliderInput(
                    #   "Surv_Anc_bins",
                    #   label = "Absolute Neutrophil Count分组",
                    #   min =  min(na.omit(unified_data$anc)),
                    #   max = max(na.omit(unified_data$anc)),
                    #   value = median(na.omit(unified_data$anc))
                    # ),
                    tags$div(
                      id =
                        "SliderInputAnc"
                    ),
                    actionButton(
                      inputId = "button_Surv_Anc",
                      label = "Re-Analyze",
                      width = "100%"
                    )
                  ),
                  mainPanel(
                    downloadLink(
                      outputId   = "anc_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("Absolute Neutrophil Count与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_anc_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "PLT",
                  wellPanel(
                    # sliderInput(
                    #   "Surv_Plt_bins",
                    #   label = "Platete分组",
                    #   min =  min(na.omit(unified_data$plt)),
                    #   max = max(na.omit(unified_data$plt)),
                    #   value = median(na.omit(unified_data$plt))
                    # ),
                    tags$div(
                      id =
                        "SliderInputPlt"
                    ),
                    actionButton(
                      inputId = "button_Surv_Plt",
                      label = "Re-Analyze",
                      width = "100%"
                    )
                  ),
                  mainPanel(
                    downloadLink(
                      outputId   = "plt_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("Platelet与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_plt_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "HB",
                  wellPanel(
                    # sliderInput(
                    #   "Surv_Hb_bins",
                    #   label = "Hemoglobin分组",
                    #   min =  min(na.omit(unified_data$hb)),
                    #   max = max(na.omit(unified_data$hb)),
                    #   value = median(na.omit(unified_data$hb))
                    # ),
                    tags$div(
                      id =
                        "SliderInputHb"
                    ),
                    actionButton(
                      inputId = "button_Surv_Hb",
                      label = "Re-Analyze",
                      width = "100%"
                    )
                  ),
                  mainPanel(
                    downloadLink(
                      outputId   = "hb_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("Hemoglobin与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_hb_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "Risk Group",
                  mainPanel(
                    downloadLink(
                      outputId   = "risk_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("AML 危险度与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_risk_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "FAB Classification",

                  mainPanel(
                    downloadLink(
                      outputId   = "fab_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("FAB分型与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_fab_div"
                    ),
                    width = 12
                  )
                ),
                tabPanel(
                  "Gene Mutation",
                  wellPanel(
                    # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                    selectInput(
                      "genes",
                      "基因突变：",
                      choices =
                        c(
                          "CKIT" = "CKIT",
                          "NPM1" = "NPM1",
                          "NRAS" = "NRAS",
                          "ASXL1" = "ASXL1",
                          "CEBPA" = "CEBPA"
                        )
                    ),
                    actionButton(
                      inputId = "button_Surv_genemut",
                      label = "Re-Analyze",
                      width = "100%"
                    )
                  ),
                  mainPanel(
                    downloadLink(
                      outputId   = "mut_survival_downloads",
                      label = "Download",
                      class = "download_css"
                      # width = "10%"
                    ),
                    tags$h3("基因突变与长期生存的关系"),
                    tags$div(
                      id =
                        "surv_genemut_div"
                    ),
                    width = 12
                  )
                )
              )
            )
          )
        )
      )
    ),
    ## 说明文档 -----------------------
    tabPanel(
      "说明文档",
      fixedPage(shiny::verticalLayout(
        wellPanel(
          style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",

          shiny::tabsetPanel(
            tabPanel(
              "Statistical Methods",
              includeHTML("./document.html")
            ),
            tabPanel(
              "R Version & R Package",
              includeHTML("./sessionInfo.html")
            )
          )
        )
      ))
    )
  )

shinyApp(ui = ui, server = server)
