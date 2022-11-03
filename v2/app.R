
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
if(!suppressWarnings({require(Hmisc)})){print("Load Package Hmisc Failed")}
if(!suppressWarnings({require(cmprsk)})){print("Load Package cmprsk Failed")}



# Style all the table output using htmlTable's theme handler
htmlTable::setHtmlTableTheme(css.rgroup = "")
# options(htmlTable.cat=T)

# supress all warnings information
options(warn = -1)

setwd("E:/projects/git_repo/aml_database_analysis_shinyapp/v2")


source("./data_prepar.r",encoding = "utf-8")
source("./helper_func.r",encoding = "utf-8")
source("./constant_vars.r",encoding = "utf-8")

data_list <- data_prepare()
unified_data <- data_list$unified_data
series_data_long <- data_list$series_data_long
gene_mutation <- data_list$gene_mutations

# 确定分组的数量，后续提升，调整顺序，放到dataprepare.r 添加每组数量信息
treat_group <- unified_data %>% dplyr::select(Treatment) %>% unique()
treat_group_list <- treat_group$Treatment  %>% 
  factor(levels = c("LDC","SDC","NonRandom_LDC","NonRandom_SDC","LDC_M7","RR_A","RR_B","RR_C","RR_D")) %>% as.character()%>%
  as.list()


#unified_data$Treatment
# SERVER -----------------------------------
server <- function(input, output,session) {
  
  
  print(paste("Shiny App Start ... time: ",date(),sep=" "))
  
  # globalClick<<- FALSE 
  # reactive，就是对部分UI的input进行孤立，仅仅考虑这些input的变化而重新计算数据。不会受到所有控件的控制。
  
  ## 选择治疗组和配色 -------------------------------------------
  
  ## 确保至少两组进行后续分析
  observeEvent(input$treatGroup_analysis,{
    #print("button click...")
    #print(input$treatGroup)
    #print(length(input$treatGroup))
    
    if(length(input$treatGroup) <2){
      #print("length < 2")
      # globalClick<<-FALSE
      session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
    }else{
        updateTabsetPanel(session, "nav",
                          selected ="患者数据概览")
      # globalClick<<-TRUE
    }
    
    
  })
  
  # 生成unidata的筛选数据  
  reactive_subdata <-eventReactive(input$treatGroup_analysis,{
    print("reactive subdat is running")
    subdat <- unified_data[unified_data$Treatment %in% c(input$treatGroup),]
    # subdat <- unified_data[unified_data$Treatment %in% c("SDC","LDC"),]
    subdat$Treatment <- droplevels(subdat$Treatment)
    return(subdat)
  })
  
  
  # 生成series data long的筛选程序
  reactive_series_data_long<- reactive({
    print("reactive series data is running")
    series_data_long_sub <- series_data_long[series_data_long$Treatment %in% c(input$treatGroup),]
    series_data_long_sub$Treatment <- droplevels(series_data_long_sub$Treatment)
    return(series_data_long_sub)
  })
  
  
  # observeEvent({globalClick == TRUE},{
  #   print("GlobalClick == TRUE")
  #   
  # })
  
  ##  概览  -------------------------------------------
  
  #setProgress(message = "Main Progress...")
            
  ## 生成table1
  reactive_table1<- reactive({
   
   # https://cran.rstudio.com/web/packages/Gmisc/vignettes/Descriptives.html#the-basics-of-getdescriptionstatsby
   subdat <- reactive_subdata()
   t1 <- list()
   label(subdat$sex_category) <- "Sex"
   t1[["Sex"]] <-
     getTable1Stats(subdat$sex_category,subdat$Treatment)
   
   label(subdat$age) <- "Age"
   units(subdat$age) <- "Years"
   t1[["Age"]] <-
     getTable1Stats(subdat$age,subdat$Treatment)
   
   label(subdat$wbc) <- "White Blood Cell Counts"
   units(subdat$age) <- "10^9/L"
   t1[["White Blood Cell Counts&dagger;"]] <- 
     getTable1Stats(subdat$wbc,subdat$Treatment)
   
   label(subdat$plt) <- "Platelets"
   units(subdat$plt) <- "10^9/L"
   t1[["Platelets&dagger;"]] <- 
     getTable1Stats(subdat$plt,subdat$Treatment)
   
   label(subdat$anc) <- "Absolute Neutrophil Counts"
   units(subdat$anc) <- "10^9/L"
   t1[["Absolute Neutrophil Counts&dagger;"]] <- 
     getTable1Stats(subdat$anc,subdat$Treatment)
   
   label(subdat$hb) <- "Hemoglobin"
   units(subdat$hb) <- "10^9/L"
   t1[["Hemoglobin&dagger;"]] <- 
     getTable1Stats(subdat$hb,subdat$Treatment)
   
   subdat$aml_risk<-factor(subdat$aml_risk,levels = c(1,2,3,4),labels = c("Low","Mid","High","unclassify"))
   label(subdat$aml_risk) <- "AML Risk Group"
   t1[["AML Risk Group"]] <- 
     getTable1Stats(subdat$aml_risk,subdat$Treatment)
   
   subdat$FAB_category<-factor(subdat$FAB_category,levels = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "MDS RAEB-t", "MDS RAEB", "无法分类" ),
                               labels = c("M0", "M1", "M2", "M3", "M4", "M5", "M6", "M7", "MDS RAEB-t", "MDS RAEB", "unclassify" ))
   label(subdat$FAB_category) <- "FAB Classification"
   t1[["FAB Classification"]] <- 
     getTable1Stats(subdat$FAB_category,subdat$Treatment)
   
   #subdat$aml_risk<-factor(subdat$aml_risk,levels = c(1,2,3,4),labels = c("Low","Mid","High","unclassify"))
   label(subdat$CKIT) <- "CKIT"
   t1[["Mutations ckit"]] <- 
     getTable1Stats(subdat$CKIT,subdat$Treatment)
   
   label(subdat$NRAS) <- "NRAS"
   t1[["Mutations nras"]] <- 
     getTable1Stats(subdat$NRAS,subdat$Treatment)
   
   label(subdat$CEBPA) <- "CEBPA"
   t1[["Mutations cepba"]] <- 
     getTable1Stats(subdat$CEBPA,subdat$Treatment)
   
   label(subdat$NPM1) <- "NPM1"
   t1[["Mutations npm1"]] <- 
     getTable1Stats(subdat$NPM1,subdat$Treatment)
   
   label(subdat$ASXL1) <- "ASXL1"
   t1[["Mutations asxl1"]] <- 
     getTable1Stats(subdat$ASXL1,subdat$Treatment)
   #  we just set width of cells to 5000px, so that it could fit all screen with.
   # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
   basic_line<-mergeDesc(t1) %>% 
     addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
     htmlTable(caption  = "Basic descriptive statistics for the whole cohort", tfoot = "&dagger; The unit is in 10<sup>9</sup> /L")
   print(basic_line)

   
 })
  ## 绘制table1
  output$Table1<-renderUI({ withProgress(message = "Calculating Table1...",expr = {reactive_table1()})})
  
  ## 治疗反应 短期治疗反应
  reactive_short_outcome<- reactive({
    
    subdat<- reactive_subdata()
    t2 <- list()
    #unified_data$sex_category<-factor(unified_data$sex_category,levels = c("o","1"),labels = c("男","女"))
    label(subdat$induction1_outcome) <- "Induction1"
    t2[["Induction1"]] <-
      getTable1Stats(subdat$induction1_outcome,subdat$Treatment)
    
    label(subdat$induction2_outcome) <- "Induction2"
    t2[["Induction2"]] <- 
      getTable1Stats(subdat$induction2_outcome,subdat$Treatment)
    
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    short_outcome<-mergeDesc(t2) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Clinical outcome of induction1/2",
                tfoot = " ")
    print(short_outcome)
    
  })
  ## 绘制短期治疗反应
  output$short_outcome<-renderUI({withProgress(message = "Calculating Induction Response...",expr = {reactive_short_outcome()})})
  
  ## 治疗反应 长期治疗反应
  reactive_long_outcome<- reactive({
    
    subdat<-reactive_subdata()
    subdat_group <- subdat$Treatment %>% unique() %>%as.character()
    fitOSSH <-survfit(Surv(OS,Status_OS)~Treatment,data=subdat)
    
    ggOSSH <- ggsurvplot(fitOSSH,data = subdat,
                         pval = TRUE, conf.int = TRUE,
                         risk.table = TRUE, # Add risk table
                         risk.table.col = "strata", # Change risk table color by groups
                         linetype = "strata", # Change line type by groups
                         surv.median.line = "hv", # Specify median survival
                         ggtheme = theme_bw(), # Change ggplot2 theme
                         palette = input$clincical_outcome_palette,legend.title="Overall Survival",legend.labs=subdat_group
    )
    
    
    
    
    fitEFSSH <-survfit(Surv(EFS,Status_EFS)~Treatment,data=subdat)
    ggEFSSH <- ggsurvplot(fitEFSSH,data=subdat,
                          pval = TRUE, conf.int = TRUE,
                          risk.table = TRUE, # Add risk table
                          risk.table.col = "strata", # Change risk table color by groups
                          linetype = "strata", # Change line type by groups
                          surv.median.line = "hv", # Specify median survival
                          ggtheme = theme_bw(), # Change ggplot2 theme
                          palette = input$clincical_outcome_palette,legend.title="Event Free Survival",legend.labs=subdat_group
    )
    
    
    
    fitRFSSH <-survfit(Surv(RFS,Status_RFS)~Treatment,data=subdat)
    ggRFSSH <- ggsurvplot(fitRFSSH,data=subdat,
                          pval = TRUE, conf.int = TRUE,
                          risk.table = TRUE, # Add risk table
                          risk.table.col = "strata", # Change risk table color by groups
                          linetype = "strata", # Change line type by groups
                          surv.median.line = "hv", # Specify median survival
                          ggtheme = theme_bw(), # Change ggplot2 theme
                          palette = input$clincical_outcome_palette,fun="event",legend.title="Cumulative Incidence of Relapse",legend.labs=subdat_group
    )
    
    
    
    #ggOSSH$table
    cowplot::plot_grid(
      cowplot::plot_grid(ggOSSH$plot,ggOSSH$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
      cowplot::plot_grid(ggEFSSH$plot,ggEFSSH$table+guides(color=F),ncol=1,rel_heights = c(2,1)),
      cowplot::plot_grid(ggRFSSH$plot,ggRFSSH$table+guides(color=F),ncol=1,rel_heights = c(2,1))
    )
    
    #cowplot::plot_grid(ggOSSH$table,ggEFSSH$plot,ggRFSSH$plot,ncol=2,labels = c("A","B","C"))
    
    
  })
  ## 绘制长期治疗反应
  output$Survival <- renderPlot({withProgress(message = "Calculating Long-term Survival...",expr = {reactive_long_outcome()})})

  ## 生成不良事件
  reactive_adverse_event<- reactive({
    
    subdat<- reactive_subdata()

    
    t4 <- list()
  
    # there are no death information yet, so we genrate simulation
    # unified_data$is_death <-sample(c("Yes","No"),size = nrow(unified_data),replace = T)
    # unified_data$is_death <- factor(unified_data$is_death,levels = c("Yes","No"),labels = c("Yes","No"))
    label(subdat$is_dead) <- "death" 
    t4[["Death"]] <-
      getTable1Stats(subdat$is_dead,subdat$Treatment)
    
    
    # unified_data$is_relapse <-sample(c("Yes","No"),size = nrow(unified_data),replace = T)
    # unified_data$is_relapse <- factor(subdat$is_relapse,levels = c("Yes","No"),labels = c("Yes","No"))
    label(subdat$induction2_outcome) <- "relapse"
    t4[["Relapse"]] <- 
      getTable1Stats(subdat$is_relapse,subdat$Treatment)
    
    # unified_data$is_transplant <-sample(c("Yes","No"),size = nrow(unified_data),replace = T)
    # unified_data$is_transplant <- factor(unified_data$is_transplant,levels = c("Yes","No"),labels = c("Yes","No"))
    
    
    ## 后期要修复缺少 YesNO导致的计算出错
    # label(subdat$is_transplant) <- "transplant"
    # t4[["Transplant"]] <- 
    #   getTable1Stats(subdat$is_transplant,subdat$Treatment)
    # 
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    drt<-mergeDesc(t4) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Death/Relapse/Transplant",
                tfoot = " ")
    print(drt)
    
  })
  ## 绘制不良事件
  output$deathrelapsetransplant<- renderUI({withProgress(message = "Calculating Adverse Event...",expr = {reactive_adverse_event()})})
  
  ## 生成副作用
  reactive_sideeffect<- reactive({
    subdat <- reactive_subdata()
    t3 <- list()
    #unified_data$sex_category<-factor(unified_data$sex_category,levels = c("o","1"),labels = c("男","女"))
    
    label(subdat$induction1_anc_recover_time) <- "Induction1 anc reovery"
    t3[["Induction1: recovery Time ANC "]] <-
      getTable1Stats(subdat$induction1_anc_recover_time,subdat$Treatment)
    
    
    label(subdat$induction2_anc_recover_time) <- "Induction2 anc reovery"
    t3[["Induction2: recovery Time ANC"]] <- 
      getTable1Stats(subdat$induction2_anc_recover_time,subdat$Treatment)
    
    label(subdat$induction1_plt_recover_time) <- "Induction1 platelet reovery"
    t3[["Induction1: recovery Time Platelet"]] <-
      getTable1Stats(subdat$induction1_plt_recover_time,subdat$Treatment)
    
    label(subdat$induction2_plt_recover_time) <- "Induction2 platelet reovery"
    t3[["Induction2: recovery Time Platelet"]] <- 
      getTable1Stats(subdat$induction2_plt_recover_time,subdat$Treatment)
    
    #  we just set width of cells to 5000px, so that it could fit all screen with.
    # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
    side<-mergeDesc(t3) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Side effects",
                tfoot = " ")
    print(side)
  })
  ## 绘制副作用
  output$sideeffect<-renderUI({withProgress(message = "Calculating Side Effects...",expr = {reactive_sideeffect()})})  

  ## 生成治疗费用
  reactive_cost<- reactive({
    
    subdat<- reactive_subdata()
    
    t3_cost <- list()
    label(subdat$induction1_period_cost) <- "Induction1 costs"
    t3_cost[["Induction1: Costs"]] <- 
      getTable1Stats(subdat$induction1_period_cost,subdat$Treatment)
    
    label(subdat$induction2_period_cost) <- "Induction2 costs"
    t3_cost[["Induction2: Costs"]] <- 
      getTable1Stats(subdat$induction2_period_cost,subdat$Treatment)
    cost<-mergeDesc(t3_cost) %>% 
      addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
      htmlTable(caption  = "Costs",
                tfoot = " ")
    print(cost)
    
  })
  ## 绘制治疗费用
  output$costs <- renderUI({withProgress(expr = {reactive_cost()})})
  
  ## 生成血常规时序图
  reactive_routine_blood<- eventReactive(input$treatGroup_analysis,{
    series_data_long <- reactive_series_data_long()
    series_data_long %>%
      ggplot() + 
      geom_line(aes(x=Series_nth_day,y=value,color=factor(Treatment),group=factor(Series_patient_id))) + 
      facet_grid(Params~Treatment,scales = "free") + theme_bw()  +labs(color="Treatment") + xlab(label = "Days") +
      stat_smooth(aes(x=Series_nth_day,y=value))
  })
  ## 绘制血常规时序图
  output$RoutineBlood <- renderPlot({
    withProgress(message = "Calculating Blood Routine Examination...",expr = {
    reactive_routine_blood()
    })
    })
  
  ## 短期治疗反应 -------------------------
  
  ## 短期 ~性别 目前并未适配到不同分组，要适配。
  reactive_sex_induction <- reactive({
    subdat<- reactive_subdata()
    
    tmpListData1<-list()
    
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table(tmpSubDat,by="induction1_outcome","Sex","sex_category",paste(treatName,"Induction1",sep = " "))
      tmpListData1[[treatName]]<- tmpCorrTableInd1 
    }
    
    tmpListData2<-list()
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table(tmpSubDat,by="induction2_outcome","Sex","sex_category",paste(treatName,"Induction2",sep = " "))
      tmpListData2[[treatName]]<- tmpCorrTableInd1 
    }
    
    return(list("induction1"=tmpListData1,"induction2"=tmpListData2))
  })
  ## 渲染sex界面
  observeEvent(input$treatGroup_analysis,
               {
                  #incProgress(message = "Calculating Induction ~ Sex...")
                  
                 withProgress(message = "Calculating Induction ~ Sex...",{
                   if(input$treatGroup_analysis!=0){
                     #这里的code是为了对 短期反应~临床因素 和长期反应~临床因素的两个选项卡的页面的动态生成。
                     #主要是为了可以根据治疗反应动态的生成图表，参考以下代码编写
                     
                     #dynamic_list<- createDynamicTableRow(input$treatGroup,idPrefix = "shortSex")
                     
                     removeUI( selector = "#ind1_sex_div  > *",multiple = T)
                     removeUI( selector = "#ind2_sex_div  > *",multiple = T)
                     
                     
                     insertUI(
                       selector = "#ind1_sex_div",
                       where = "beforeEnd",
                       ui = tagList(
                         createDynamicTableRow(input$treatGroup,idPrefix = "shortSexInd1")
                       )
                     )
                     
                     insertUI(
                       selector = "#ind2_sex_div",
                       where = "beforeEnd",
                       ui = tagList(
                         createDynamicTableRow(input$treatGroup,idPrefix = "shortSexInd2")
                       )
                     )
                     
                   }  
                   sexList <-reactive_sex_induction() # list(induciton1=,induction2=)
                   # print(input$treatGroup)
                   sapply(input$treatGroup,FUN = function(treatName){
                     # print(paste0("treatName ",treatName))
                     output[[paste("shortSexInd1",treatName,sep = "_")]] <- renderUI({print(sexList$induction1[[treatName]])})
                     output[[paste("shortSexInd2",treatName,sep = "_")]] <- renderUI({print(sexList$induction2[[treatName]])}) 
                   })  
                 })
                 
                  
               })
  ## 短期 ~ risk
  reactive_risk_induction <- reactive({
    subdat<- reactive_subdata()
    
    Name="Risk"
    ColName="aml_risk_category"
    
    
    tmpListData1<-list()
    
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table(tmpSubDat,by="induction1_outcome",Name,ColName,paste(treatName,"Induction1",sep = " "))
      tmpListData1[[treatName]]<- tmpCorrTableInd1 
    }
    
    tmpListData2<-list()
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table(tmpSubDat,by="induction2_outcome",Name,ColName,paste(treatName,"Induction2",sep = " "))
      tmpListData2[[treatName]]<- tmpCorrTableInd1 
    }
    
    return(list("induction1"=tmpListData1,"induction2"=tmpListData2))
  })
  ## 渲染risk界面
  observeEvent(input$treatGroup_analysis,
               {
                 withProgress(message = "Calculating Induction ~ Risk...",{
                   
                 if(input$treatGroup_analysis!=0){
                   #这里的code是为了对 短期反应~临床因素 和长期反应~临床因素的两个选项卡的页面的动态生成。
                   #主要是为了可以根据治疗反应动态的生成图表，参考以下代码编写
                   
                   #dynamic_list<- createDynamicTableRow(input$treatGroup,idPrefix = "shortSex")
                   
                   
                   IDprefixInd1 = "shortRiskInd1"
                   IDprefixInd2 = "shortRiskInd2"
                   htmlIDHandelerInd1 = "#ind1_risk_div"
                   htmlIDHandelerInd2 = "#ind2_risk_div"
                   
                   
                   removeUI( selector = paste0(htmlIDHandelerInd1,"  > *",sep=" "),multiple = T)
                   removeUI( selector = paste0(htmlIDHandelerInd2,"  > *",sep=" "),multiple = T)
                   
                   
                   insertUI(
                     selector = htmlIDHandelerInd1,
                     where = "beforeEnd",
                     ui = tagList(
                       createDynamicTableRow(input$treatGroup,idPrefix = IDprefixInd1)
                     )
                   )
                   
                   insertUI(
                     selector = htmlIDHandelerInd2,
                     where = "beforeEnd",
                     ui = tagList(
                       createDynamicTableRow(input$treatGroup,idPrefix = IDprefixInd2)
                     )
                   )
                   
                 }  
                 RiskList <-reactive_risk_induction() # list(induciton1=,induction2=)
       
                 sapply(input$treatGroup,FUN = function(treatName){
                   # print(paste0("treatName ",treatName))
                   output[[paste(IDprefixInd1,treatName,sep = "_")]] <- renderUI({print(RiskList$induction1[[treatName]])})  
                   output[[paste(IDprefixInd2,treatName,sep = "_")]] <- renderUI({print(RiskList$induction2[[treatName]])})
                 })
                   
                 })
                 
               })

  ## 短期 ~ FAB
  reactive_FAB_induction <- reactive({
    subdat<- reactive_subdata()
    
    Name="FAB Classification"
    ColName="FAB_category"
    
    
    tmpListData1<-list()
    
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table(tmpSubDat,by="induction1_outcome",Name,ColName,paste(treatName,"Induction1",sep = " "))
      tmpListData1[[treatName]]<- tmpCorrTableInd1 
    }
    
    tmpListData2<-list()
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table(tmpSubDat,by="induction2_outcome",Name,ColName,paste(treatName,"Induction2",sep = " "))
      tmpListData2[[treatName]]<- tmpCorrTableInd1 
    }
    
    return(list("induction1"=tmpListData1,"induction2"=tmpListData2))
  })
  ## 渲染FAB界面
  observeEvent(input$treatGroup_analysis,
               {
                 withProgress(message = "Calculating Induction ~ FAB Classification...",{
                   
                 
                 if(input$treatGroup_analysis!=0){
                   #这里的code是为了对 短期反应~临床因素 和长期反应~临床因素的两个选项卡的页面的动态生成。
                   #主要是为了可以根据治疗反应动态的生成图表，参考以下代码编写
                   
                   #dynamic_list<- createDynamicTableRow(input$treatGroup,idPrefix = "shortSex")
                   
                   
                   IDprefixInd1 = "shortFABInd1"
                   IDprefixInd2 = "shortFABInd2"
                   htmlIDHandelerInd1 = "#ind1_FAB_div"
                   htmlIDHandelerInd2 = "#ind2_FAB_div"
                   
                   
                   removeUI( selector = paste0(htmlIDHandelerInd1,"  > *",sep=" "),multiple = T)
                   removeUI( selector = paste0(htmlIDHandelerInd2,"  > *",sep=" "),multiple = T)
                   
                   
                   insertUI(
                     selector = htmlIDHandelerInd1,
                     where = "beforeEnd",
                     ui = tagList(
                       createDynamicTableRow(input$treatGroup,idPrefix = IDprefixInd1)
                     )
                   )
                   
                   insertUI(
                     selector = htmlIDHandelerInd2,
                     where = "beforeEnd",
                     ui = tagList(
                       createDynamicTableRow(input$treatGroup,idPrefix = IDprefixInd2)
                     )
                   )
                   
                 }  
                 RiskList <-reactive_FAB_induction() # list(induciton1=,induction2=)
                 
                 sapply(input$treatGroup,FUN = function(treatName){
                   # print(paste0("treatName ",treatName))
                   output[[paste(IDprefixInd1,treatName,sep = "_")]] <- renderUI({print(RiskList$induction1[[treatName]])})  
                   output[[paste(IDprefixInd2,treatName,sep = "_")]] <- renderUI({print(RiskList$induction2[[treatName]])})  
                 })
                 
                   
                 })
                 
               })
  
  ## 短期 ~ Gene Mut
  reactive_GeneMut_induction <- reactive({
    subdat<- reactive_subdata()
    
    Name="Gene Mutation"
    ColName="FAB_category"
    
    
    gene_mutation
    
    tmpListData1<-list()
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table_mutation(dat = gene_mutation,by = "induction1_outcome",strataCol = "Mutation",rowvar = "status",paste(treatName,"Induction1",sep = " "))
      tmpListData1[[treatName]]<- tmpCorrTableInd1 
    }
    
    
    tmpListData2<-list()
    for ( treatName in input$treatGroup){
      tmpSubDat<- subdat %>% filter(Treatment==treatName)
      tmpCorrTableInd1<-gen_corr_table_mutation(dat = gene_mutation,by = "induction2_outcome",strataCol = "Mutation",rowvar = "status",paste(treatName,"Induction2",sep = " "))
      tmpListData2[[treatName]]<- tmpCorrTableInd1 
    }
    
    return(list("induction1"=tmpListData1,"induction2"=tmpListData2))
  })
  ## 渲染Gene Mut页面
  observeEvent(input$treatGroup_analysis,
               {
    
                 withProgress(message = "Calculating Induction ~ Gene Mutations...",{
                   
                              
                 if(input$treatGroup_analysis!=0){
                   #这里的code是为了对 短期反应~临床因素 和长期反应~临床因素的两个选项卡的页面的动态生成。
                   #主要是为了可以根据治疗反应动态的生成图表，参考以下代码编写
                   
                   #dynamic_list<- createDynamicTableRow(input$treatGroup,idPrefix = "shortSex")
                   
                   
                   IDprefixInd1 = "shorGeneMutInd1"
                   IDprefixInd2 = "shortGeneMutInd2"
                   htmlIDHandelerInd1 = "#ind1_GeneMut_div"
                   htmlIDHandelerInd2 = "#ind2_GeneMut_div"
                   
                   
                   removeUI( selector = paste0(htmlIDHandelerInd1,"  > *",sep=" "),multiple = T)
                   removeUI( selector = paste0(htmlIDHandelerInd2,"  > *",sep=" "),multiple = T)
                   
                   
                   insertUI(
                     selector = htmlIDHandelerInd1,
                     where = "beforeEnd",
                     ui = tagList(
                       createDynamicTableRow(input$treatGroup,idPrefix = IDprefixInd1)
                     )
                   )
                   
                   insertUI(
                     selector = htmlIDHandelerInd2,
                     where = "beforeEnd",
                     ui = tagList(
                       createDynamicTableRow(input$treatGroup,idPrefix = IDprefixInd2)
                     )
                   )
                   
                 }  
                 RiskList <-reactive_GeneMut_induction() # list(induciton1=,induction2=)
                 
                 sapply(input$treatGroup,FUN = function(treatName){
                   # print(paste0("treatName ",treatName))
                   output[[paste(IDprefixInd1,treatName,sep = "_")]] <- renderUI({print(RiskList$induction1[[treatName]])})  
                   output[[paste(IDprefixInd2,treatName,sep = "_")]] <- renderUI({print(RiskList$induction2[[treatName]])})  
                 })
                 
                   
                 })
               })

  ## 短期 ~ age
  reactive_Age_induction<- reactive({
    ##!!!###
    # 循环不同治疗方式，动态创建不同比较组，设置异常处理，封装为函数
    subdat<- reactive_subdata()
    VarName = "age"
    # change select age
    minidata <-  subdat %>% select(age,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment %in% input$treatGroup)    
    mycompInd1 <- genPermutaitonDouble((minidata$induction1_outcome%>% na.omit() %>% unique()))
    mycompInd2 <- genPermutaitonDouble((minidata$induction2_outcome%>% na.omit() %>% unique()))
    if(mycompInd1$flag){
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd1$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    if(mycompInd2$flag){
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction2_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd2$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette ,add = 'jitter',shape="induction2_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    ggpubr::ggarrange(ind1,ind2 ,ncol=1,labels = "AUTO")

  })
  ## 短期，渲染age  
  observeEvent(input$treatGroup_analysis,{
            
    
    withProgress(message = "Calculating Induction ~ Age...",{
      
    
    data_reactive <- reactive_Age_induction()
    
    htmlIDHandeler="#induction_Age_div"
    
    removeUI( selector = paste0(htmlIDHandeler,"  > *",sep=" "),multiple = T)
    
    insertUI(
      selector = htmlIDHandeler,
      where = "beforeEnd",
      ui = 
        plotOutput("induction_Age",width = "100%",height = paste((length(input$treatGroup)*500),"px",sep=""))
    )
    output$induction_Age <- shiny::renderPlot(data_reactive)
      
    })
  })
  
  
  

  ## 短期 ~ wbc
  reactive_Wbc_induction<- reactive({
    ##!!!###
    # 循环不同治疗方式，动态创建不同比较组，设置异常处理，封装为函数
    subdat<- reactive_subdata()
    #修改
    VarName = "wbc"
    # change select age
    #修改
    minidata <-  subdat %>% select(wbc,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment %in% input$treatGroup)    
    mycompInd1 <- genPermutaitonDouble((minidata$induction1_outcome%>% na.omit() %>% unique()))
    mycompInd2 <- genPermutaitonDouble((minidata$induction2_outcome%>% na.omit() %>% unique()))
    if(mycompInd1$flag){
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd1$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    if(mycompInd2$flag){
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction2_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd2$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette ,add = 'jitter',shape="induction2_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    ggpubr::ggarrange(ind1,ind2 ,ncol=1,labels = "AUTO")

  })
  ## 短期，渲染wbc 
  observeEvent(input$treatGroup_analysis,{
            
    
    withProgress(message = "Calculating Induction ~ Sex...",{
      
    data_reactive <- reactive_Wbc_induction()
    
    #修改
    htmlIDHandeler="#induction_Wbc_div"
    
    removeUI( selector = paste0(htmlIDHandeler,"  > *",sep=" "),multiple = T)
    
    insertUI(
      selector = htmlIDHandeler,
      where = "beforeEnd",
      # 修改
      ui =  plotOutput("induction_Wbc",width = "100%",height = paste((length(input$treatGroup)*500),"px",sep=""))
    )
    # 修改
    output$induction_Wbc <- shiny::renderPlot(data_reactive)
      
    })
    
    
  })
  

  ## 短期 ~ anc
  reactive_Anc_induction<- reactive({
    ##!!!###
    # 循环不同治疗方式，动态创建不同比较组，设置异常处理，封装为函数
    subdat<- reactive_subdata()
    #修改
    VarName = "anc"
    # change select age
    #修改
    minidata <-  subdat %>% select(anc,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment %in% input$treatGroup)    
    mycompInd1 <- genPermutaitonDouble((minidata$induction1_outcome%>% na.omit() %>% unique()))
    mycompInd2 <- genPermutaitonDouble((minidata$induction2_outcome%>% na.omit() %>% unique()))
    if(mycompInd1$flag){
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd1$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    if(mycompInd2$flag){
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction2_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd2$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette ,add = 'jitter',shape="induction2_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    ggpubr::ggarrange(ind1,ind2 ,ncol=1,labels = "AUTO")

  })
  ## 短期，渲染anc 
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Induction ~ Sex...",{
      
            
    Anc_reactive <- reactive_Anc_induction()
    
    #修改
    htmlIDHandeler="#induction_Anc_div"
    
    removeUI( selector = paste0(htmlIDHandeler,"  > *",sep=" "),multiple = T)
    
    insertUI(
      selector = htmlIDHandeler,
      where = "beforeEnd",
      # 修改
      ui =  plotOutput("induction_Anc",width = "100%",height = paste((length(input$treatGroup)*500),"px",sep=""))
    )
    # 修改
    output$induction_Anc <- shiny::renderPlot(Anc_reactive)
      
    })
    
  })
  

  ## 短期 ~ plt
  reactive_Plt_induction<- reactive({
    ##!!!###
    # 循环不同治疗方式，动态创建不同比较组，设置异常处理，封装为函数
    subdat<- reactive_subdata()
    #修改
    VarName = "plt"
    # change select age
    #修改
    minidata <-  subdat %>% select(plt,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment %in% input$treatGroup)    
    mycompInd1 <- genPermutaitonDouble((minidata$induction1_outcome%>% na.omit() %>% unique()))
    mycompInd2 <- genPermutaitonDouble((minidata$induction2_outcome%>% na.omit() %>% unique()))
    if(mycompInd1$flag){
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd1$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    if(mycompInd2$flag){
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction2_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd2$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette ,add = 'jitter',shape="induction2_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    ggpubr::ggarrange(ind1,ind2 ,ncol=1,labels = "AUTO")

  })
  ## 短期，渲染plt
  observeEvent(input$treatGroup_analysis,{
            
    withProgress(message = "Calculating Induction ~ platelet...",{
      
      
    
      
      data_reactive <- reactive_Plt_induction()
      
      #修改
      htmlIDHandeler="#induction_Plt_div"
      
      removeUI( selector = paste0(htmlIDHandeler,"  > *",sep=" "),multiple = T)
      
      insertUI(
        selector = htmlIDHandeler,
        where = "beforeEnd",
        # 修改
        ui =  plotOutput("induction_Plt",width = "100%",height = paste((length(input$treatGroup)*500),"px",sep=""))
      )
      # 修改
      output$induction_Plt <- shiny::renderPlot(data_reactive)  
    })
    
    
    
  })
  

  ## 短期 ~ hb
  reactive_Hb_induction<- reactive({
    ##!!!###
    # 循环不同治疗方式，动态创建不同比较组，设置异常处理，封装为函数
    subdat<- reactive_subdata()
    #修改
    VarName = "hb"
    # change select age
    #修改
    minidata <-  subdat %>% select(hb,induction1_outcome,induction2_outcome,Treatment) %>% filter(Treatment %in% input$treatGroup)    
    mycompInd1 <- genPermutaitonDouble((minidata$induction1_outcome%>% na.omit() %>% unique()))
    mycompInd2 <- genPermutaitonDouble((minidata$induction2_outcome%>% na.omit() %>% unique()))
    if(mycompInd1$flag){
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd1$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind1<- ggpubr::ggboxplot(minidata,x = "induction1_outcome",y = VarName,fill="induction1_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction1_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    if(mycompInd2$flag){
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette  ,add = 'jitter',shape="induction2_outcome") + 
        ggpubr::stat_compare_means(comparisons = mycompInd2$res,method = 'wilcox',paired = F) + 
        facet_wrap(.~Treatment,ncol =2) 
      
    }else{
      ind2<- ggpubr::ggboxplot(minidata,x = "induction2_outcome",y = VarName,fill="induction2_outcome", palette = input$clincical_outcome_palette ,add = 'jitter',shape="induction2_outcome") + 
        facet_wrap(.~Treatment,ncol =2)
    }
    ggpubr::ggarrange(ind1,ind2 ,ncol=1,labels = "AUTO")
    
  })
  ## 短期，渲染anc 
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Induction ~ Absolute Neutrophil Counts...",{
      
    data_reactive <- reactive_Hb_induction()
    
    #修改
    htmlIDHandeler="#induction_Hb_div"
    
    removeUI( selector = paste0(htmlIDHandeler,"  > *",sep=" "),multiple = T)
    
    insertUI(
      selector = htmlIDHandeler,
      where = "beforeEnd",
      # 修改
      ui =  plotOutput("induction_Hb",width = "100%",height = paste((length(input$treatGroup)*500),"px",sep=""))
    )
    # 修改
    output$induction_Hb <- shiny::renderPlot(data_reactive)
      
    })
    
  })
  
    
  
    
  ## 长期治疗反应 -----------------------

  ## survival!~ sex
  reactive_sex_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      mdata <- subdat %>% filter(Treatment ==treat)
      
      fitOS <-survfit(Surv(OS,Status_OS)~sex_category,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~sex_category,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~sex_category,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
        
  })
  
  observeEvent(input$treatGroup_analysis,{
    withProgress({
      selectorID= "#surv_sex_div"
      removeUI(selector = paste0(selectorID," >*"),multiple = T)
      
      insertUI(
        selector = selectorID,
        ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvSex"),
        where = "beforeEnd")
      
      
      PlotList <- reactive_sex_survival()
      
      sapply(input$treatGroup,FUN = function(treatName){
        output[[paste("SurvSex",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
      })
      
      
    },message = "Calculating Survival ~ Sex...")
    
  })
  

  ## survival!~ risk
  reactive_risk_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      mdata <- subdat %>% filter(Treatment ==treat)
      
      fitOS  <-survfit(Surv(OS,Status_OS)~aml_risk,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~aml_risk,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~aml_risk,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ Risk...",{
      
    selectorID= "#surv_risk_div"
    removeUI(selector = paste0(selectorID," >*"),multiple = T)
    
    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvRisk"),
      where = "beforeEnd")
    
    
    PlotList <- reactive_risk_survival()
    
    #print(PlotList)
    sapply(input$treatGroup,FUN = function(treatName){
      output[[paste("SurvRisk",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
    })
    
      
    })
    
  })
  
  
  ## survival!~ fab
  reactive_fab_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      mdata <- subdat %>% filter(Treatment ==treat)
      
      fitOS  <-survfit(Surv(OS,Status_OS)~FAB_category,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~FAB_category,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~FAB_category,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ FAB Classification...",{
      
    selectorID= "#surv_fab_div"
    removeUI(selector = paste0(selectorID," >*"),multiple = T)
    
    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvFAB"),
      where = "beforeEnd")
    
    
    PlotList <- reactive_fab_survival()
    
    #print(PlotList)
    sapply(input$treatGroup,FUN = function(treatName){
      output[[paste("SurvFAB",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
    })
    
      
    })
    
  })
  
  
  
  ## survival!~ GeneMutation
  reactive_genemut_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      mdata <- subdat %>% filter(Treatment ==treat) %>% rename("Mutation"=input$genes)
      
      fitOS  <-survfit(Surv(OS,Status_OS)~Mutation,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~Mutation,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~Mutation,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ Gene Mutations...",{
      
    selectorID= "#surv_genemut_div"
    removeUI(selector = paste0(selectorID," >*"),multiple = T)
    
    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvMut"),
      where = "beforeEnd")
    
    
    PlotList <- reactive_genemut_survival()
    
    #print(PlotList)
    sapply(input$treatGroup,FUN = function(treatName){
      output[[paste("SurvMut",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
    })
    
      
    })
    
  })
  
  observeEvent(input$button_Surv_genemut,{
    
    withProgress(message = "ReCalculating Induction ~ Gene Mutations...",{
      
    selectorID= "#surv_genemut_div"
    removeUI(selector = paste0(selectorID," >*"),multiple = T)
    
    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvMut"),
      where = "beforeEnd")
    
    
    PlotList <- reactive_genemut_survival()
    
    #print(PlotList)
    sapply(input$treatGroup,FUN = function(treatName){
      output[[paste("SurvMut",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
    })
    
      
    })
    
  })
  
  
  ## survival ~ Age 
  
  reactive_age_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      # 修改age
      GrpCol="age"
      
      mdata <- subdat %>% filter(Treatment ==treat) 
      
      mdata$Group<- cut(mdata[,GrpCol] ,breaks = c(-Inf,input$Surv_Age_bins,Inf),labels = c("Low","High"))
      

  
      fitOS  <-survfit(Surv(OS,Status_OS)~Group,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~Group,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~Group,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ Age",{
      
    subdat<- reactive_subdata()
    # 修改age
    GrpCol="age"
    
    numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Age_bins,Inf),labels = c("Low","High")) %>% 
      na.omit() %>% unique() %>% length()
    
    if(numOfGrps <2){
      
      session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
    }else{
      selectorID= "#surv_age_div"
      removeUI(selector = paste0(selectorID," >*"),multiple = T)
      
      insertUI(
        selector = selectorID,
        ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvAge"),
        where = "beforeEnd")
      PlotList <- reactive_age_survival()
      sapply(input$treatGroup,FUN = function(treatName){
        output[[paste("SurvAge",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
      })
    }
      
      
    })
    
      
        
  })
  
  observeEvent(input$button_Surv_Age,{
    withProgress(message = "ReCalculating Survival ~ Age",{
      subdat<- reactive_subdata()
      # 修改age
      GrpCol="age"
      
      numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Age_bins,Inf),labels = c("Low","High")) %>% 
        na.omit() %>% unique() %>% length()
      
      if(numOfGrps <2){
        
        session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
      }else{
        selectorID= "#surv_age_div"
        removeUI(selector = paste0(selectorID," >*"),multiple = T)
        
        insertUI(
          selector = selectorID,
          ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvAge"),
          where = "beforeEnd")
        PlotList <- reactive_age_survival()
        sapply(input$treatGroup,FUN = function(treatName){
          output[[paste("SurvAge",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  
        })
      }
      
    })

    
  })
  
  
  
  ## survival ~ Wbc
  
  reactive_wbc_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      # 修改
      GrpCol="wbc"
      
      mdata <- subdat %>% filter(Treatment ==treat) 
      
      #修改input$Surv_Age_bins
      mdata$Group<- cut(mdata[,GrpCol] ,breaks = c(-Inf,input$Surv_Wbc_bins,Inf),labels = c("Low","High"))
      
      fitOS  <-survfit(Surv(OS,Status_OS)~Group,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~Group,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~Group,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ White Blood Counts...",{
      
    
    subdat<- reactive_subdata()
    # 修改
    GrpCol="wbc"
    
    #修改 input$Surv_Age_bins
    numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Wbc_bins,Inf),labels = c("Low","High")) %>% 
      na.omit() %>% unique() %>% length()
    
    if(numOfGrps <2){
      
      session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
    }else{
      selectorID= "#surv_wbc_div" #修改
      removeUI(selector = paste0(selectorID," >*"),multiple = T)
      
      insertUI(
        selector = selectorID,
        ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvWbc"), #修改
        where = "beforeEnd")
      PlotList <- reactive_wbc_survival() #修改
      sapply(input$treatGroup,FUN = function(treatName){
        output[[paste("SurvWbc",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})   #修改
      })
    }
      
    })
    
    
    
  })
  
  observeEvent(input$button_Surv_Wbc,{
    
    withProgress(message = "ReCalculating Survival ~ White Blood Count...",{
      subdat<- reactive_subdata()
      # 修改age
      GrpCol="wbc"
      
      #修改
      numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Wbc_bins,Inf),labels = c("Low","High")) %>% 
        na.omit() %>% unique() %>% length()
      
      if(numOfGrps <2){
        
        session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
      }else{
        selectorID= "#surv_wbc_div" #修改
        removeUI(selector = paste0(selectorID," >*"),multiple = T)
        
        insertUI(
          selector = selectorID,
          ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvWbc"), #修改
          where = "beforeEnd")
        PlotList <- reactive_wbc_survival() #修改
        sapply(input$treatGroup,FUN = function(treatName){
          output[[paste("SurvWbc",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  #修改 
        })
      }
      
      
    })
    
  })
  
  

  ## survival ~ anc
  
  #修改
  reactive_anc_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      # 修改
      GrpCol="anc"
      
      mdata <- subdat %>% filter(Treatment ==treat) 
      
      #修改input$Surv_Age_bins
      mdata$Group<- cut(mdata[,GrpCol] ,breaks = c(-Inf,input$Surv_Anc_bins,Inf),labels = c("Low","High"))
      
      fitOS  <- tryCatch(expr = {survfit(Surv(OS,Status_OS)~Group,data=mdata)},warning=function(w){return(NULL)},error=function(e){return(NULL)})
      fitEFS <- tryCatch(expr = {survfit(Surv(EFS,Status_EFS)~Group,data=mdata)},warning=function(w){return(NULL)},error=function(e){return(NULL)})
      fitRFS<- tryCatch(expr = {survfit(Surv(RFS,Status_RFS)~Group,data=mdata)},warning=function(w){return(NULL)},error=function(e){return(NULL)})
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ Absolute Neutrophil Counts...",{
      
      
    subdat<- reactive_subdata()
    # 修改
    GrpCol="anc"
    
    #修改 input$Surv_Age_bins
    numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Anc_bins,Inf),labels = c("Low","High")) %>% 
      na.omit() %>% unique() %>% length()
    
    if(numOfGrps <2){
      
      session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
    }else{
      selectorID= "#surv_anc_div" #修改
      removeUI(selector = paste0(selectorID," >*"),multiple = T)
      
      insertUI(
        selector = selectorID,
        ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvAnc"), #修改
        where = "beforeEnd")
      PlotList <- reactive_anc_survival() #修改
      sapply(input$treatGroup,FUN = function(treatName){
        output[[paste("SurvAnc",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})   #修改
      })
    }
    
    })
    
    
  })
  
  observeEvent(input$button_Surv_Anc,{
    withProgress(message = "ReCalculating Survival ~ Absolute Neutrophil Counts...",{
      subdat<- reactive_subdata()
      # 修改age
      GrpCol="anc"
      
      #修改
      numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Anc_bins,Inf),labels = c("Low","High")) %>% 
        na.omit() %>% unique() %>% length()
      
      if(numOfGrps <2){
        
        session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
      }else{
        selectorID= "#surv_anc_div" #修改
        removeUI(selector = paste0(selectorID," >*"),multiple = T)
        
        insertUI(
          selector = selectorID,
          ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvAnc"), #修改
          where = "beforeEnd")
        PlotList <- reactive_anc_survival() #修改
        sapply(input$treatGroup,FUN = function(treatName){
          output[[paste("SurvAnc",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  #修改 
        })
      }
      
    })
    
    
  })
  
  
  ## survival ~ plt
  
  #修改
  reactive_plt_survival <- reactive({
    
    withProgress(message = "Calculating Induction ~ Absolute Neutrophil Counts...",{
      
      
    })
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      # 修改
      GrpCol="plt"
      
      mdata <- subdat %>% filter(Treatment ==treat) 
      
      #修改input$Surv_Age_bins
      mdata$Group<- cut(mdata[,GrpCol] ,breaks = c(-Inf,input$Surv_Plt_bins,Inf),labels = c("Low","High"))
      
      fitOS  <-survfit(Surv(OS,Status_OS)~Group,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~Group,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~Group,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ Platelets...",{
      
    subdat<- reactive_subdata()
    # 修改
    GrpCol="plt"
    #修改 input$Surv_Age_bins
    numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Plt_bins,Inf),labels = c("Low","High")) %>% 
      na.omit() %>% unique() %>% length()
    
    if(numOfGrps <2){
      
      session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
    }else{
      selectorID= "#surv_plt_div" #修改
      removeUI(selector = paste0(selectorID," >*"),multiple = T)
      insertUI(
        selector = selectorID,
        ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvPlt"), #修改
        where = "beforeEnd")
      PlotList <- reactive_plt_survival() #修改
      sapply(input$treatGroup,FUN = function(treatName){
        output[[paste("SurvPlt",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})   #修改
      })
    }
      
    })
    
    
  })
  
  observeEvent(input$button_Surv_Plt,{
    withProgress(message = "ReCalculating Survival ~ Paltelets",{
      subdat<- reactive_subdata()
      # 修改age
      GrpCol="plt"
      
      #修改
      numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Anc_bins,Inf),labels = c("Low","High")) %>% 
        na.omit() %>% unique() %>% length()
      
      if(numOfGrps <2){
        
        session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
      }else{
        selectorID= "#surv_plt_div" #修改
        removeUI(selector = paste0(selectorID," >*"),multiple = T)
        
        insertUI(
          selector = selectorID,
          ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvPlt"), #修改
          where = "beforeEnd")
        PlotList <- reactive_plt_survival() #修改
        sapply(input$treatGroup,FUN = function(treatName){
          output[[paste("SurvPlt",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  #修改 
        })
      }    
      
    })
  
    
  })
  
  
  ## survival ~ hb
  
  #修改
  reactive_hb_survival <- reactive({
    
    subdat<- reactive_subdata()
    plotlist <- list()
    # 循环遍历每种治疗方案：
    for (treat in input$treatGroup){
      
      # 修改
      GrpCol="hb"
      
      mdata <- subdat %>% filter(Treatment ==treat) 
      
      #修改input$Surv_Age_bins
      mdata$Group<- cut(mdata[,GrpCol] ,breaks = c(-Inf,input$Surv_Hb_bins,Inf),labels = c("Low","High"))
      
      fitOS  <-survfit(Surv(OS,Status_OS)~Group,data=mdata)
      fitEFS <-survfit(Surv(EFS,Status_EFS)~Group,data=mdata)
      fitRFS <-survfit(Surv(RFS,Status_RFS)~Group,data=mdata)
      
      listfit<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
      
      plot<- genSurvPlotEachTreatCatetory2(dat = mdata,listOfSurvfit = listfit,Pattle = input$clincical_outcome_palette)
      plotlist[[treat]]<- plot[[1]]
    }
    
    return(plotlist)
    
    
  })
  
  observeEvent(input$treatGroup_analysis,{
    
    withProgress(message = "Calculating Survival ~ Hemoglobin...",{
      
    subdat<- reactive_subdata()
    # 修改
    GrpCol="hb"
    
    #修改 input$Surv_Age_bins
    numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Hb_bins,Inf),labels = c("Low","High")) %>% 
      na.omit() %>% unique() %>% length()
    
    if(numOfGrps <2){
      
      session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
    }else{
      selectorID= "#surv_hb_div" #修改
      removeUI(selector = paste0(selectorID," >*"),multiple = T)
      
      insertUI(
        selector = selectorID,
        ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvHb"), #修改
        where = "beforeEnd")
      PlotList <- reactive_hb_survival() #修改
      sapply(input$treatGroup,FUN = function(treatName){
        output[[paste("SurvHb",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})   #修改
      })
    }
    
      
    })
    
    
  })
  #修改
  observeEvent(input$button_Surv_Hb,{
    withProgress(message = "ReCalculating Survival ~ Hemoglobin...",{
      subdat<- reactive_subdata()
      # 修改age
      GrpCol="hb"
      
      #修改
      numOfGrps<- cut(subdat[,GrpCol] ,breaks = c(-Inf,input$Surv_Hb_bins,Inf),labels = c("Low","High")) %>% 
        na.omit() %>% unique() %>% length()
      
      if(numOfGrps <2){
        
        session$sendCustomMessage(type = 'testmessage',message = '请至少选择2组进行后续分析')
      }else{
        selectorID= "#surv_hb_div" #修改
        removeUI(selector = paste0(selectorID," >*"),multiple = T)
        
        insertUI(
          selector = selectorID,
          ui = createDynamicPlotRow(input$treatGroup,idPrefix = "SurvHb"), #修改
          where = "beforeEnd")
        PlotList <- reactive_hb_survival() #修改
        sapply(input$treatGroup,FUN = function(treatName){
          output[[paste("SurvHb",treatName,sep = "_")]] <- renderPlot({PlotList[[treatName]]})  #修改 
        })
      }  
      
    })
    
    
  })
  
  
}


# UI -----------------------------------
ui <- shiny::navbarPage("临床数据实时分析系统(v2)",theme=shinytheme("flatly"),fluid=FALSE,id="nav",
                        ## 选择方案疗法和配色 ----
                        
                        tabPanel("选择治疗方案和配色",
                                 
                                 fixedPage(
                                   tags$head(tags$script(src = "message-handler.js")),
                                   
                                   verticalLayout(
                                     
                                     wellPanel(style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
                                               wellPanel(
                                                 
                                                 checkboxGroupInput("treatGroup", label = h5(strong("选择治疗组")),
                                                                    choices = treat_group_list,
                                                                    selected = 1,inline=T,width = "100%"),
                                                 tags$style(type="text/css", HTML("#treatGroup1> .checkbox-inline{width:30px}")),
                                                 selectInput("clincical_outcome_palette","配色",choices = palette_chn)
                                                 
                                               ),
                                               actionButton(inputId = "treatGroup_analysis",label = "Analyze all with default settings",width = "100%")
                                     )
                                     
                                   )
                                 )
                        ),
                        ## 数据概览 ---------------
# 
#                         conditionalPanel(
#                           condition = "input$treatGroup_analysis > 0",
#                           tabPanel("data")
#                         ),
                        
                        tabPanel("患者数据概览",
                                 
                                 fixedPage(
                                   verticalLayout(
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               actionButton(inputId = "treatGroup1_downloads",label = "Download",width = "15%"),
                                               tags$style(type="text/css", HTML("#treatGroup1_downloads {float: right;}")),
                                               
                                               tabsetPanel(
                                                 tabPanel("患者基线数据概览",

                                                          width="100%",
                                                          mainPanel(
                                                            tags$h3("患者基线特征"),
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
                                     
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               actionButton(inputId = "treatGroup1_downloads",label = "Download",width = "15%"),
                                               tags$style(type="text/css", HTML("#treatGroup1_downloads {float: right;}")),
                                               
                                               tabsetPanel(
                                                 tabPanel("Sex",

                                                          mainPanel(
                                                            tags$h3("性别与诱导缓解疗效的关系"),
                                                            
                                                            fixedRow(tags$div(id="ind1_sex_div")),
                                                            
                                                            fixedRow(tags$div(id="ind2_sex_div")),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Age",

                                                          mainPanel(
                                                            tags$h3("年龄与诱导缓解疗效的关系"),
                                                            tags$div(id="induction_Age_div"),
                                                            
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("WBC",

                                                          mainPanel(
                                                            tags$h3("White Blood Cell Count与诱导缓解疗效的关系"),
                                                            tags$div(id="induction_Wbc_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("ANC",

                                                          mainPanel(
                                                            tags$h3("Absolute Neutrophil Count与诱导缓解疗效的关系"),
                                                            tags$div(id="induction_Anc_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("PLT",

                                                          
                                                          mainPanel(
                                                            tags$h3("Platelet与诱导缓解疗效的关系"),
                                                            tags$div(id="induction_Plt_div"),
                                                            
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("HB",
                                                          
                                                          mainPanel(
                                                            tags$h3("Hemoglobin与诱导缓解疗效的关系"),
                                                            tags$div(id="induction_Hb_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Risk Group",
                                                          
                                                          mainPanel(
                                                            tags$h3("AML 危险度与诱导缓解疗效的关系"),
                                                            fixedRow(tags$div(id="ind1_risk_div")),
                                                            
                                                            fixedRow(tags$div(id="ind2_risk_div")),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("FAB Classification",
                                                          
                                                          mainPanel(
                                                            tags$h3("FAB分型与诱导缓解疗效的关系"),
                                                            fixedRow(tags$div(id="ind1_FAB_div")),
                                                            
                                                            fixedRow(tags$div(id="ind2_FAB_div"))
                                                            ,
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Gene Mutation",

                                                          mainPanel(
                                                            fixedRow(tags$div(id="ind1_GeneMut_div")),
                                                            
                                                            fixedRow(tags$div(id="ind2_GeneMut_div")),
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
                                     
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               actionButton(inputId = "treatGroup1_downloads",label = "Download",width = "15%"),
                                               tags$style(type="text/css", HTML("#treatGroup1_downloads {float: right;}")),
                                               
                                               shiny::tabsetPanel(
                                                 tabPanel("Sex",

                                                          mainPanel(
                                                            tags$h3("性别与长期生存的关系"),
                                                            tags$div(id="surv_sex_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Age",
                                                          
                                                          wellPanel(
                                                            
                                                            sliderInput("Surv_Age_bins", label = "年龄分组", min =  min(na.omit(unified_data$age)), 
                                                                        max = max(na.omit(unified_data$age)), value = median(na.omit(unified_data$age))),
                                                            actionButton(inputId = "button_Surv_Age",label = "Re-Analyze",width = "100%")
                                                          ),
                                                          mainPanel(
                                                            tags$h3("年龄与长期生存的关系"),
                                                            tags$div(id="surv_age_div"),
                                                            
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("WBC",
                                                          
                                                          
                                                          wellPanel(

                                                            sliderInput("Surv_Wbc_bins", label = "White Blood Cell Count分组", min =  min(na.omit(unified_data$wbc)), 
                                                                        max = max(na.omit(unified_data$wbc)), value = median(na.omit(unified_data$wbc))),
                                                            actionButton(inputId = "button_Surv_Wbc",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          
                                                          mainPanel(
                                                            tags$h3("White Blood Cell Count与长期生存的关系"),
                                                            tags$div(id="surv_wbc_div"),
                                                            
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("ANC",
                                                          wellPanel(
                                                            sliderInput("Surv_Anc_bins", label = "Absolute Neutrophil Count分组", min =  min(na.omit(unified_data$anc)), 
                                                                        max = max(na.omit(unified_data$anc)), value = median(na.omit(unified_data$anc))),
                                                            actionButton(inputId = "button_Surv_Anc",label = "Re-Analyze",width = "100%")
                                                          ),
                                                          mainPanel(
                                                            tags$h3("Absolute Neutrophil Count与长期生存的关系"),
                                                            tags$div(id="surv_anc_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("PLT",
                                                          
                                                          wellPanel(

                                                            sliderInput("Surv_Plt_bins", label = "Platete分组", min =  min(na.omit(unified_data$plt)),
                                                                        max = max(na.omit(unified_data$plt)), value = median(na.omit(unified_data$plt))), 
                                                            actionButton(inputId = "button_Surv_Plt",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          mainPanel(
                                                            tags$h3("Platelet与长期生存的关系"),
                                                            
                                                            tags$div(id="surv_plt_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("HB",
                                                          
                                                          wellPanel(

                                                            sliderInput("Surv_Hb_bins", label = "Hemoglobin分组", min =  min(na.omit(unified_data$hb)),
                                                                        max = max(na.omit(unified_data$hb)), value = median(na.omit(unified_data$hb))), 
                                                            actionButton(inputId = "button_Surv_Hb",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          mainPanel(
                                                            tags$h3("Hemoglobin与长期生存的关系"),
                                                            
                                                            tags$div(id="surv_hb_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Risk Group",

                                                          mainPanel(
                                                            tags$h3("AML 危险度与长期生存的关系"),
                                                            tags$div(id="surv_risk_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("FAB Classification",

                                                          mainPanel(
                                                            tags$h3("FAB分型与长期生存的关系"),
                                                            tags$div(id="surv_fab_div"),
                                                            width = 12
                                                          )
                                                 ),
                                                 tabPanel("Gene Mutation",
                                                          
                                                          wellPanel(
                                                            # selectInput("clincical_outcome_palette","配色",choices = palette_chn),
                                                            selectInput("genes", "基因突变：", 
                                                                        choices=c("CKIT"="CKIT","NPM1"="NPM1","NRAS"="NRAS","ASXL1"="ASXL1","CEBPA"="CEBPA")
                                                            ),
                                                            actionButton(inputId = "button_Surv_genemut",label = "Re-Analyze",width = "100%")
                                                            
                                                          ),
                                                          
                                                          mainPanel(
                                                            tags$h3("基因突变与长期生存的关系"),
                                                            tags$div(id="surv_genemut_div"),
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
                                 # wellPanel(style = "background-color: #FFFFFF;border-style:solid;border-width:2px;border-color:#F5F5F5;",
                                 #           includeHTML("./document.html")
                                 #           
                                 # ),
                                 
                                 fixedPage(
                                   shiny::verticalLayout(
                                     
                                     wellPanel(style = "background-color: #FFFFFF;height:2000px;border-style:solid;border-width:2px;border-color:#F5F5F5",
                                               
                                               shiny::tabsetPanel(
                                                 tabPanel("Statistical Methods",
                                                          includeHTML("./document.html")
                                                 ),
                                                 tabPanel("R Version & R Package",
                                                          includeHTML("./sessionInfo.html")
                                                          )
                                                 
                                               )
                                     )
                                   )
                                 )
                                 
                                 
                        )
                        
)  

shinyApp(ui = ui, server = server)


