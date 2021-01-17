
# reactive 生成分析数据 -------------

# reactive模式选择病人进行后续分析，这里筛选了Unifieddata
# 通过parseQueryString从URL接受GET请求
# return的是unifieddata
selectPtsData <- reactive({
  query <- parseQueryString(session$clientData$url_search)
  query_pts <- unlist(query)
  query_pts <- as.numeric(query_pts)
  if (length(query_pts) > 0) {
    print("Start Select Patients...")
    unified_data <-
      unified_data[unified_data$patient_id %in% query_pts, ]
  }
  return(unified_data)
})

# 承接selectPtsData 结果，进一步筛选治疗组，拆开两个函数是因为要动态展示选择的病人列表的治疗分组情况。
reactive_subdata <- eventReactive(input$treatGroup_analysis, {
  print("reactive subdat is running")
  udata <- selectPtsData()
  subdat <-
    udata[udata$Treatment %in% c(input$treatGroup), ]
  # subdat <- unified_data[unified_data$Treatment %in% c("SDC","LDC","RR_A"),]
  subdat$Treatment <- droplevels(subdat$Treatment)
  return(subdat)
})


# 筛选基因突变的病人列表和治疗组
reactive_subgeneMutationData <- reactive({
  print("reactive Gene Mutation data is running")
  # 筛选病人
  query <- parseQueryString(session$clientData$url_search)
  query_pts <- unlist(query)
  query_pts <- as.numeric(query_pts)
  if (length(query_pts) > 0) {
    print("Start Select Patients...")
    gene_mutation <-
      gene_mutation[gene_mutation$patient_id %in% query_pts, ]
  }

  # 筛选分组
  gene_mutation_sub <-
    gene_mutation[gene_mutation$Treatment %in% c(input$treatGroup), ]
  gene_mutation_sub$Treatment <-
    droplevels(gene_mutation_sub$Treatment)

  return(gene_mutation_sub)
})

# 筛选时序数据的病人列表和治疗组
reactive_series_data_long <- reactive({
  print("reactive series data is running")
  # 筛选病人
  query <- parseQueryString(session$clientData$url_search)
  query_pts <- unlist(query)
  query_pts <- as.numeric(query_pts)
  if (length(query_pts) > 0) {
    print("Start Select Patients...")
    series_data_long <-
      series_data_long[series_data_long$patient_id %in% query_pts, ]
  }

  # 筛选治疗组
  series_data_long_sub <-
    series_data_long[series_data_long$Treatment %in% c(input$treatGroup), ]
  series_data_long_sub$Treatment <-
    droplevels(series_data_long_sub$Treatment)
  return(series_data_long_sub)
})



## reactive 概览  -------------------------------------------

## 生成table1
reactive_table1 <- reactive({
  # https://cran.rstudio.com/web/packages/Gmisc/vignettes/Descriptives.html#the-basics-of-getdescriptionstatsby
  subdat <- reactive_subdata()
  t1 <- list()
  label(subdat$sex_category) <- "Sex"
  t1[["Sex"]] <-
    getTable1StatsSafe(subdat$sex_category, subdat$Treatment)

  label(subdat$age) <- "Age"
  units(subdat$age) <- "Years"
  t1[["Age"]] <-
    getTable1StatsSafe(subdat$age, subdat$Treatment)

  label(subdat$wbc) <- "White Blood Cell Counts"
  units(subdat$age) <- "10^9/L"
  t1[["White Blood Cell Counts&dagger;"]] <-
    getTable1StatsSafe(subdat$wbc, subdat$Treatment)

  label(subdat$plt) <- "Platelets"
  units(subdat$plt) <- "10^9/L"
  t1[["Platelets&dagger;"]] <-
    getTable1StatsSafe(subdat$plt, subdat$Treatment)

  label(subdat$anc) <- "Absolute Neutrophil Counts"
  units(subdat$anc) <- "10^9/L"
  t1[["Absolute Neutrophil Counts&dagger;"]] <-
    getTable1StatsSafe(subdat$anc, subdat$Treatment)

  label(subdat$hb) <- "Hemoglobin"
  units(subdat$hb) <- "10^9/L"
  t1[["Hemoglobin&dagger;"]] <-
    getTable1StatsSafe(subdat$hb, subdat$Treatment)

  label(subdat$aml_risk_category) <- "AML Risk Group"
  t1[["AML Risk Group"]] <-
    getTable1StatsSafe(subdat$aml_risk_category, subdat$Treatment)

  subdat$FAB_category <-
    factor(
      subdat$FAB_category,
      levels = c(
        "M0",
        "M1",
        "M2",
        "M3",
        "M4",
        "M5",
        "M6",
        "M7",
        "MDS RAEB-t",
        "MDS RAEB",
        "无法分类"
      ),
      labels = c(
        "M0",
        "M1",
        "M2",
        "M3",
        "M4",
        "M5",
        "M6",
        "M7",
        "MDS RAEB-t",
        "MDS RAEB",
        "unclassify"
      )
    )
  label(subdat$FAB_category) <- "FAB Classification"
  t1[["FAB Classification"]] <-
    getTable1StatsSafe(subdat$FAB_category, subdat$Treatment)

  # subdat$aml_risk<-factor(subdat$aml_risk,levels = c(1,2,3,4),labels = c("Low","Mid","High","unclassify"))
  label(subdat$CKIT) <- "CKIT"
  t1[["Mutations ckit"]] <-
    getTable1StatsSafe(subdat$CKIT, subdat$Treatment)

  label(subdat$NRAS) <- "NRAS"
  t1[["Mutations nras"]] <-
    getTable1StatsSafe(subdat$NRAS, subdat$Treatment)

  label(subdat$CEBPA) <- "CEBPA"
  t1[["Mutations cepba"]] <-
    getTable1StatsSafe(subdat$CEBPA, subdat$Treatment)

  label(subdat$NPM1) <- "NPM1"
  t1[["Mutations npm1"]] <-
    getTable1StatsSafe(subdat$NPM1, subdat$Treatment)

  label(subdat$ASXL1) <- "ASXL1"
  t1[["Mutations asxl1"]] <-
    getTable1StatsSafe(subdat$ASXL1, subdat$Treatment)
  #  we just set width of cells to 5000px, so that it could fit all screen with.
  # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
  basic_line <- mergeDesc(t1) %>%
    addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
    htmlTable(caption = "Basic descriptive statistics for the whole cohort", tfoot = "&dagger; The unit is in 10<sup>9</sup> /L")
  print(basic_line)
})



## 治疗反应 短期治疗反应
reactive_short_outcome <- reactive({
  subdat <- reactive_subdata()
  t2 <- list()
  # unified_data$sex_category<-factor(unified_data$sex_category,levels = c("o","1"),labels = c("男","女"))
  label(subdat$induction1_outcome) <- "Induction1"
  t2[["Induction1"]] <-
    getTable1StatsSafe(subdat$induction1_outcome, subdat$Treatment)

  label(subdat$induction2_outcome) <- "Induction2"
  t2[["Induction2"]] <-
    getTable1StatsSafe(subdat$induction2_outcome, subdat$Treatment)

  #  we just set width of cells to 5000px, so that it could fit all screen with.
  # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
  short_outcome <- mergeDesc(t2) %>%
    addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
    htmlTable(
      caption = "Clinical outcome of induction1/2",
      tfoot = " "
    )
  print(short_outcome)
})

## 治疗反应 长期治疗反应
reactive_long_outcome <- reactive({
  subdat <- reactive_subdata()
  subdat_group <- subdat$Treatment %>%
    unique() %>%
    as.character()
  fitOSSH <-
    survfit(Surv(OS, Status_OS) ~ Treatment, data = subdat)

  ggOSSH <- ggsurvplot(
    fitOSSH,
    data = subdat,
    pval = TRUE,
    conf.int = TRUE,
    risk.table = TRUE,
    # Add risk table
    risk.table.col = "strata",
    # Change risk table color by groups
    linetype = "strata",
    # Change line type by groups
    surv.median.line = "hv",
    # Specify median survival
    ggtheme = theme_bw(),
    # Change ggplot2 theme
    palette = input$clincical_outcome_palette,
    legend.title = "Overall Survival",
    legend.labs = subdat_group
  )

  fitEFSSH <-
    survfit(Surv(EFS, Status_EFS) ~ Treatment, data = subdat)
  ggEFSSH <- ggsurvplot(
    fitEFSSH,
    data = subdat,
    pval = TRUE,
    conf.int = TRUE,
    risk.table = TRUE,
    # Add risk table
    risk.table.col = "strata",
    # Change risk table color by groups
    linetype = "strata",
    # Change line type by groups
    surv.median.line = "hv",
    # Specify median survival
    ggtheme = theme_bw(),
    # Change ggplot2 theme
    palette = input$clincical_outcome_palette,
    legend.title = "Event Free Survival",
    legend.labs = subdat_group
  )

  fitEFSSH2 <-
    survfit(Surv(EFS2, Status_EFS2) ~ Treatment, data = subdat)
  ggEFSSH2 <- ggsurvplot(
    fitEFSSH2,
    data = subdat,
    pval = TRUE,
    conf.int = TRUE,
    risk.table = TRUE,
    # Add risk table
    risk.table.col = "strata",
    # Change risk table color by groups
    linetype = "strata",
    # Change line type by groups
    surv.median.line = "hv",
    # Specify median survival
    ggtheme = theme_bw(),
    # Change ggplot2 theme
    palette = input$clincical_outcome_palette,
    legend.title = "Event Free Survival 2",
    legend.labs = subdat_group
  )

  fitRFSSH <-
    cuminc(
      ftime = subdat$RFS,
      # failure time variable
      fstatus = subdat$Status_RFS,
      # variable with distinct codes for different causes of failure
      group = subdat$Treatment,
      # estimates will calculated within groups
      ## strata  = ,  # Tests will be stratified on this variable.
      rho = 0,
      # Power of the weight function used in the tests.
      cencode = 0,
      # value of fstatus variable which indicates the failure time is censored.
      ## subset = ,
      ## na.action = na.omit
    )

  ggRFSSH <- ggcompetingrisks(
    fitRFSSH,
    gnames = NULL,
    gsep = " ",
    multiple_panels = F,
    ggtheme = theme_bw(),
    pval = "xdf",
    conf.int = F,
    surv.median.line = "hv",
    # Specify median survival
    palette = input$clincical_outcome_palette,
    legend.title = list(color = "Event", linetype = "Group")
  )


  # ggOSSH$table
  cowplot::plot_grid(
    cowplot::plot_grid(
      ggOSSH$plot,
      ggOSSH$table + guides(color = F),
      ncol = 1,
      rel_heights = c(2, 1)
    ),
    cowplot::plot_grid(
      ggEFSSH$plot,
      ggEFSSH$table + guides(color = F),
      ncol = 1,
      rel_heights = c(2, 1)
    ),
    cowplot::plot_grid(
      ggEFSSH2$plot,
      ggEFSSH2$table + guides(color = F),
      ncol = 1,
      rel_heights = c(2, 1)
    ),
    cowplot::plot_grid(
      ggRFSSH,
      ggplot() +
        theme_void(),
      ncol = 1,
      rel_heights = c(2, 1)
    )
  )
})

## 生成不良事件
reactive_adverse_event <- reactive({
  subdat <- reactive_subdata()


  t4 <- list()

  # there are no death information yet, so we genrate simulation
  label(subdat$is_dead) <- "death"
  t4[["Death"]] <-
    getTable1StatsSafe(subdat$is_dead, subdat$Treatment)


  label(subdat$induction2_outcome) <- "relapse"
  t4[["Relapse"]] <-
    getTable1StatsSafe(subdat$is_relapse, subdat$Treatment)

  label(subdat$induction2_outcome) <- "transplant"
  t4[["Transplant"]] <-
    getTable1StatsSafe(subdat$is_transplant, subdat$Treatment)
  ## 后期要修复缺少 YesNO导致的计算出错
  # label(subdat$is_transplant) <- "transplant"
  # t4[["Transplant"]] <-
  #   getTable1StatsSafe(subdat$is_transplant,subdat$Treatment)
  #
  #  we just set width of cells to 5000px, so that it could fit all screen with.
  # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
  drt <- mergeDesc(t4) %>%
    addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
    htmlTable(
      caption = "Death/Relapse/Transplant",
      tfoot = " "
    )
  print(drt)
})

## 生成副作用
reactive_sideeffect <- reactive({
  subdat <- reactive_subdata()
  t3 <- list()
  # unified_data$sex_category<-factor(unified_data$sex_category,levels = c("o","1"),labels = c("男","女"))

  label(subdat$induction1_anc_recover_time) <-
    "Induction1 anc reovery"
  t3[["Induction1: recovery Time ANC "]] <-
    getTable1StatsSafe(subdat$induction1_anc_recover_time, subdat$Treatment)

  label(subdat$induction2_anc_recover_time) <-
    "Induction2 anc reovery"
  t3[["Induction2: recovery Time ANC"]] <-
    getTable1StatsSafe(subdat$induction2_anc_recover_time, subdat$Treatment)

  label(subdat$induction1_plt_recover_time) <-
    "Induction1 platelet reovery"
  t3[["Induction1: recovery Time Platelet"]] <-
    getTable1StatsSafe(subdat$induction1_plt_recover_time, subdat$Treatment)

  label(subdat$induction2_plt_recover_time) <-
    "Induction2 platelet reovery"
  t3[["Induction2: recovery Time Platelet"]] <-
    getTable1StatsSafe(subdat$induction2_plt_recover_time, subdat$Treatment)

  #  we just set width of cells to 5000px, so that it could fit all screen with.
  # https://stackoverflow.com/questions/25417633/can-i-control-word-wrap-or-column-width-in-htmltable
  side <- mergeDesc(t3) %>%
    addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
    htmlTable(
      caption = "Side effects",
      tfoot = " "
    )
  print(side)
})

## 生成治疗费用
reactive_cost <- reactive({
  subdat <- reactive_subdata()

  t3_cost <- list()
  label(subdat$induction1_period_cost) <- "Induction1 costs"
  t3_cost[["Induction1: Costs"]] <-
    getTable1StatsSafe(subdat$induction1_period_cost, subdat$Treatment)

  label(subdat$induction2_period_cost) <- "Induction2 costs"
  t3_cost[["Induction2: Costs"]] <-
    getTable1StatsSafe(subdat$induction2_period_cost, subdat$Treatment)
  cost <- mergeDesc(t3_cost) %>%
    addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
    htmlTable(
      caption = "Costs",
      tfoot = " "
    )
  print(cost)
})

## 生成血常规时序图
reactive_routine_blood <-
  reactive({
    series_data_long <- reactive_series_data_long()
    # series_data_long %>%
    #   ggplot() +
    #   geom_line(aes(
    #     x = Series_nth_day,
    #     y = value,
    #     color = factor(Treatment),
    #     group = factor(Series_patient_id)
    #   )) +
    #   facet_grid(Params ~ Treatment, scales = "free") +
    #   theme_bw() +
    #   labs(color = "Treatment") +
    #   xlab(label = "Days") +
    #   stat_smooth(aes(x = Series_nth_day, y = value))
    series_data_long <- series_data_long %>% head(200)
    plotlist<- list()
    
    uniq_treatment<- series_data_long$Treatment %>% unique() %>% na.omit()
    
    for (treat in uniq_treatment){
      
      sub_plot_plt<-tryCatch({
         series_data_long[which(series_data_long$Treatment == treat),] %>% 
          filter(Params == "Series_plt") %>%
          ggplot() +
          geom_line(aes(x = Series_nth_day,y = value,group=factor(Series_patient_id)),color="#FF8000") +
          theme_bw() +
          labs(color = "Treatment") +
          xlab(label = "Days") +
          ggtitle(label = "Palette(10^9/L)") +
          stat_smooth(aes(x = Series_nth_day, y = value))

      },
      error=function(e){
        ggplot()
      },
      warning=function(w){
        ggplot()
      })
      
      sub_plot_anc<-tryCatch({
        series_data_long[which(series_data_long$Treatment == treat),] %>%
          filter(Params == "Series_anc") %>%
          ggplot() +
          geom_line(aes(x = Series_nth_day,y = value,group=factor(Series_patient_id)),color="#228B22") +
          theme_bw() +
          labs(color = "Treatment") +
          xlab(label = "Days") +
          ggtitle(label = "Absolute Neutrophil Count(10^9/L)") +
          stat_smooth(aes(x = Series_nth_day, y = value))

      },
      error=function(e){
        ggplot()
      },
      warning=function(w){
        ggplot()
      })
    
      sub_plot_wbc<-tryCatch({
         series_data_long[which(series_data_long$Treatment == treat),] %>%
          filter(Params == "Series_wbc") %>%
          ggplot() +
          geom_line(aes(x = Series_nth_day,y = value,group=factor(Series_patient_id)),color="#DA70D6") +
          theme_bw() +
          labs(color = "Treatment") +
          xlab(label = "Days") +
          ggtitle(label = "White Blood Cell Count(10^9/L)") +
          stat_smooth(aes(x = Series_nth_day, y = value))

      },
      error=function(e){
        ggplot()
      },
      warning=function(w){
        ggplot()
      })
      sub_plot_hb<-tryCatch({
        series_data_long[which(series_data_long$Treatment == treat),] %>%
          filter(Params == "Series_hb") %>%
          ggplot() +
          geom_line(aes(x = Series_nth_day,y = value,group=factor(Series_patient_id)),color="#B03060") +
          theme_bw() +
          labs(color = "Treatment") +
          xlab(label = "Days") +
          ggtitle(label = "hemoglobin(g/L)") +
          stat_smooth(aes(x = Series_nth_day, y = value))

      },
      error=function(e){
        ggplot()
      },
      warning=function(w){
        ggplot()
      })
    
      plotlist[[treat]] <-cowplot::plot_grid(ggplot() + labs(title = treat ) + theme_void(),
        cowplot::plot_grid(sub_plot_wbc,sub_plot_anc,sub_plot_plt,sub_plot_hb,nrow=1),
        ncol=1,rel_heights = c(0.1,0.9))
      
    }
    
    plot_grid(plotlist = plotlist,ncol=1)
    
    
  })


## reactive 短期治疗反应 -------------------------

## 短期 ~性别 目前并未适配到不同分组，要适配。
reactive_sex_induction <- reactive({
  subdat <- reactive_subdata()

  tmpListData1 <- list()

  for (treatName in input$treatGroup) {
    tmpSubDat <- subdat %>% filter(Treatment == treatName)
    # tmpSubDat <- subdat %>% filter(Treatment == "SDC")


    tmpCorrTableInd1 <-
      gen_corr_table(tmpSubDat,
        by = "induction1_outcome",
        "Sex",
        "sex_category",
        paste(treatName, "Induction1", sep = " ")
      )
    tmpListData1[[treatName]] <- tmpCorrTableInd1
  }

  tmpListData2 <- list()
  for (treatName in input$treatGroup) {
    tmpSubDat <- subdat %>% filter(Treatment == treatName)
    tmpCorrTableInd1 <-
      gen_corr_table(tmpSubDat,
        by = "induction2_outcome",
        "Sex",
        "sex_category",
        paste(treatName, "Induction2", sep = " ")
      )
    tmpListData2[[treatName]] <- tmpCorrTableInd1
  }

  return(list("induction1" = tmpListData1, "induction2" = tmpListData2))
})

## 短期 ~ risk
reactive_risk_induction <- reactive({
  subdat <- reactive_subdata()

  Name <- "Risk"
  ColName <- "aml_risk_category"


  tmpListData1 <- list()

  for (treatName in input$treatGroup) {
    tmpSubDat <- subdat %>% filter(Treatment == treatName)
    tmpCorrTableInd1 <-
      gen_corr_table(tmpSubDat,
        by = "induction1_outcome",
        Name,
        ColName,
        paste(treatName, "Induction1", sep = " ")
      )
    tmpListData1[[treatName]] <- tmpCorrTableInd1
  }

  tmpListData2 <- list()
  for (treatName in input$treatGroup) {
    tmpSubDat <- subdat %>% filter(Treatment == treatName)
    tmpCorrTableInd1 <-
      gen_corr_table(tmpSubDat,
        by = "induction2_outcome",
        Name,
        ColName,
        paste(treatName, "Induction2", sep = " ")
      )
    tmpListData2[[treatName]] <- tmpCorrTableInd1
  }

  return(list("induction1" = tmpListData1, "induction2" = tmpListData2))
})

## 短期 ~ FAB
reactive_FAB_induction <- reactive({
  subdat <- reactive_subdata()

  Name <- "FAB Classification"
  ColName <- "FAB_category"


  tmpListData1 <- list()

  for (treatName in input$treatGroup) {
    tmpSubDat <- subdat %>% filter(Treatment == treatName)
    tmpCorrTableInd1 <-
      gen_corr_table(tmpSubDat,
        by = "induction1_outcome",
        Name,
        ColName,
        paste(treatName, "Induction1", sep = " ")
      )
    tmpListData1[[treatName]] <- tmpCorrTableInd1
  }

  tmpListData2 <- list()
  for (treatName in input$treatGroup) {
    tmpSubDat <- subdat %>% filter(Treatment == treatName)
    tmpCorrTableInd1 <-
      gen_corr_table(tmpSubDat,
        by = "induction2_outcome",
        Name,
        ColName,
        paste(treatName, "Induction2", sep = " ")
      )
    tmpListData2[[treatName]] <- tmpCorrTableInd1
  }

  return(list("induction1" = tmpListData1, "induction2" = tmpListData2))
})


## 短期 ~ Gene Mut
reactive_GeneMut_induction <- reactive({
  # subdat <- reactive_subdata()
  gene_mutation <- reactive_subgeneMutationData()
  Name <- "Gene Mutation"
  ColName <- "FAB_category"


  # gene_mutation

  tmpListData1 <- list()
  tmpListData2 <- list()
  for (treatName in input$treatGroup) {
    subgeneMut <- gene_mutation %>% filter(Treatment == treatName)

    tmpListData1[[treatName]] <- gen_corr_table_mutation(
      dat = subgeneMut,
      by = "induction1_outcome",
      strataCol = "Mutation",
      rowvar = "status",
      paste(treatName, "Induction1", sep = " ")
    )

    tmpListData2[[treatName]] <- gen_corr_table_mutation(
      dat = subgeneMut,
      by = "induction2_outcome",
      strataCol = "Mutation",
      rowvar = "status",
      paste(treatName, "Induction2", sep = " ")
    )
  }



  return(list("induction1" = tmpListData1, "induction2" = tmpListData2))
})


## 短期 ~ age
reactive_Age_induction <- reactive({
  genInductionPlot(subdat = reactive_subdata(), VarName = "age", treatGroup = input$treatGroup, mypalette = input$clincical_outcome_palette)
})


## 短期 ~ wbc
reactive_Wbc_induction <- reactive({
  genInductionPlot(subdat = reactive_subdata(), VarName = "wbc", treatGroup = input$treatGroup, mypalette = input$clincical_outcome_palette)
})


# ## 短期 ~ anc
reactive_Anc_induction <- reactive({
  genInductionPlot(subdat = reactive_subdata(), VarName = "anc", treatGroup = input$treatGroup, mypalette = input$clincical_outcome_palette)
})


## 短期 ~ plt
reactive_Plt_induction <- reactive({
  genInductionPlot(subdat = reactive_subdata(), VarName = "plt", treatGroup = input$treatGroup, mypalette = input$clincical_outcome_palette)
})


## 短期 ~ hb
reactive_Hb_induction <- reactive({
  genInductionPlot(subdat = reactive_subdata(), VarName = "hb", treatGroup = input$treatGroup, mypalette = input$clincical_outcome_palette)
})



## reactive 长期治疗反应 -----------------------


## survival!~ sex
reactive_sex_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  subdat$Group <- subdat$sex_category

  genSurvPlotIntegrate(subdat, input)
})

## survival!~ risk
reactive_risk_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  subdat$Group <- subdat$aml_risk_category

  genSurvPlotIntegrate(subdat, input)
})

## survival!~ fab
reactive_fab_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  subdat$Group <- subdat$FAB_category

  genSurvPlotIntegrate(subdat, input)
})

## survival!~ GeneMutation
reactive_genemut_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  # subdat$Group <- subdat$Mutation
  subdat$Group <- subdat[[input$genes]]

  genSurvPlotIntegrate(subdat, input)
})

observeEvent(input$button_Surv_genemut, {
  withProgress(message = "ReCalculating Survival ~ Gene Mutations...", {
    selectorID <- "#surv_genemut_div"
    removeUI(
      selector = paste0(selectorID, " >*"),
      multiple = T
    )

    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup, idPrefix = "SurvMut"),
      where = "beforeEnd"
    )

    SurvPlotList <- reactive_genemut_survival()
    
    sapply(
      input$treatGroup,
      FUN = function(treatName) {
        output[[paste("SurvMut", treatName, sep = "_")]] <-
          renderPlot({
            SurvPlotList[[treatName]]
          })
      }
    )
  })
})

## survival ~ Age

reactive_age_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  GrpCol <- "age"

  subdat$Group <- cut(
    subdat[, GrpCol],
    breaks = c(-Inf, input$Surv_Age_bins, Inf),
    labels = c("Low", "High")
  )

  genSurvPlotIntegrate(subdat, input)
})

observeEvent(input$button_Surv_Age, {
  withProgress(message = "ReCalculating Survival ~ Age", {
    subdat <- reactive_subdata()
    # 修改age
    GrpCol <- "age"

    numOfGrps <-
      cut(
        subdat[, GrpCol],
        breaks = c(-Inf, input$Surv_Age_bins, Inf),
        labels = c("Low", "High")
      ) %>%
      na.omit() %>%
      unique() %>%
      length()

    selectorID <- "#surv_age_div"
    removeUI(
      selector = paste0(selectorID, " >*"),
      multiple = T
    )

    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup, idPrefix = "SurvAge"),
      where = "beforeEnd"
    )
    PlotList <- reactive_age_survival()
    sapply(
      input$treatGroup,
      FUN = function(treatName) {
        output[[paste("SurvAge", treatName, sep = "_")]] <-
          renderPlot({
            PlotList[[treatName]]
          })
      }
    )
  })
})

## survival ~ Wbc

reactive_wbc_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  GrpCol <- "wbc"

  subdat$Group <- cut(
    subdat[, GrpCol],
    breaks = c(-Inf, input$Surv_Wbc_bins, Inf),
    labels = c("Low", "High")
  )

  genSurvPlotIntegrate(subdat, input)
})

observeEvent(input$button_Surv_Wbc, {
  withProgress(message = "ReCalculating Survival ~ White Blood Count...", {
    subdat <- reactive_subdata()
    # 修改age
    GrpCol <- "wbc"

    # 修改
    numOfGrps <-
      cut(
        subdat[, GrpCol],
        breaks = c(-Inf, input$Surv_Wbc_bins, Inf),
        labels = c("Low", "High")
      ) %>%
      na.omit() %>%
      unique() %>%
      length()

    selectorID <- "#surv_wbc_div" # 修改
    removeUI(
      selector = paste0(selectorID, " >*"),
      multiple = T
    )

    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup, idPrefix = "SurvWbc"),
      # 修改
      where = "beforeEnd"
    )
    PlotList <- reactive_wbc_survival() # 修改
    sapply(
      input$treatGroup,
      FUN = function(treatName) {
        output[[paste("SurvWbc", treatName, sep = "_")]] <-
          renderPlot({
            PlotList[[treatName]]
          }) # 修改
      }
    )
  })
})

## survival ~ anc

# 修改
reactive_anc_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  GrpCol <- "anc"

  subdat$Group <- cut(
    subdat[, GrpCol],
    breaks = c(-Inf, input$Surv_Anc_bins, Inf),
    labels = c("Low", "High")
  )

  genSurvPlotIntegrate(subdat, input)
})

observeEvent(input$button_Surv_Anc, {
  withProgress(message = "ReCalculating Survival ~ Absolute Neutrophil Counts...", {
    subdat <- reactive_subdata()
    # 修改age
    GrpCol <- "anc"

    # 修改
    numOfGrps <-
      cut(
        subdat[, GrpCol],
        breaks = c(-Inf, input$Surv_Anc_bins, Inf),
        labels = c("Low", "High")
      ) %>%
      na.omit() %>%
      unique() %>%
      length()
    selectorID <- "#surv_anc_div" # 修改
    removeUI(
      selector = paste0(selectorID, " >*"),
      multiple = T
    )

    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup, idPrefix = "SurvAnc"),
      # 修改
      where = "beforeEnd"
    )
    PlotList <- reactive_anc_survival() # 修改
    sapply(
      input$treatGroup,
      FUN = function(treatName) {
        output[[paste("SurvAnc", treatName, sep = "_")]] <-
          renderPlot({
            PlotList[[treatName]]
          }) # 修改
      }
    )
  })
})

## survival ~ plt

# 修改
reactive_plt_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  GrpCol <- "plt"

  subdat$Group <- cut(
    subdat[, GrpCol],
    breaks = c(-Inf, input$Surv_Plt_bins, Inf),
    labels = c("Low", "High")
  )

  genSurvPlotIntegrate(subdat, input)
})

observeEvent(input$button_Surv_Plt, {
  withProgress(message = "ReCalculating Survival ~ Paltelets", {
    subdat <- reactive_subdata()
    # 修改age
    GrpCol <- "plt"

    # 修改
    numOfGrps <-
      cut(
        subdat[, GrpCol],
        breaks = c(-Inf, input$Surv_Anc_bins, Inf),
        labels = c("Low", "High")
      ) %>%
      na.omit() %>%
      unique() %>%
      length()

    selectorID <- "#surv_plt_div" # 修改
    removeUI(
      selector = paste0(selectorID, " >*"),
      multiple = T
    )

    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup, idPrefix = "SurvPlt"),
      # 修改
      where = "beforeEnd"
    )
    PlotList <- reactive_plt_survival() # 修改
    sapply(
      input$treatGroup,
      FUN = function(treatName) {
        output[[paste("SurvPlt", treatName, sep = "_")]] <-
          renderPlot({
            PlotList[[treatName]]
          }) # 修改
      }
    )
  })
})

## survival ~ hb

# 修改
reactive_hb_survival <- reactive({
  subdat <- reactive_subdata()
  plotlist <- list()

  GrpCol <- "hb"

  subdat$Group <- cut(
    subdat[, GrpCol],
    breaks = c(-Inf, input$Surv_Hb_bins, Inf),
    labels = c("Low", "High")
  )

  genSurvPlotIntegrate(subdat, input)
})

# 修改
observeEvent(input$button_Surv_Hb, {
  withProgress(message = "ReCalculating Survival ~ Hemoglobin...", {
    subdat <- reactive_subdata()
    # 修改age
    GrpCol <- "hb"
    selectorID <- "#surv_hb_div" # 修改
    removeUI(
      selector = paste0(selectorID, " >*"),
      multiple = T
    )
    insertUI(
      selector = selectorID,
      ui = createDynamicPlotRow(input$treatGroup, idPrefix = "SurvHb"),
      # 修改
      where = "beforeEnd"
    )
    PlotList <- reactive_hb_survival() # 修改
    sapply(
      input$treatGroup,
      FUN = function(treatName) {
        output[[paste("SurvHb", treatName, sep = "_")]] <-
          renderPlot({
            PlotList[[treatName]]
          }) # 修改
      }
    )
  })
})
