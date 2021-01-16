
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


# 动态生成报表，有病人每组的人数
# 动态生成按钮
observe({
  selecteddata <- selectPtsData()
  summaryTable <- selecteddata %>%
    dplyr::select(Treatment) %>%
    group_by(Treatment) %>%
    summarise(count = n()) %>%
    arrange(Treatment) %>%
    column_to_rownames("Treatment") %>%
    t() %>%
    as.data.frame()

  treat_group_list <-
    intersect(
      c(
        "LDC",
        "SDC",
        "NonRandom_LDC",
        "NonRandom_SDC",
        "LDC_M7",
        "RR_A",
        "RR_B",
        "RR_C",
        "RR_D"
      ),
      (selecteddata[, "Treatment"] %>% unique() %>% as.character())
    )

  output$dynamic_groups_table <- renderUI({
    tableOutput("patient_Statistic")
  })

  output$dynamic_groups_choice_group <- renderUI({
    checkboxGroupButtons(
      "treatGroup",
      label = h5(strong("选择治疗组")),
      choices = treat_group_list,
      selected = 1,
      justified = F,
      checkIcon = list(yes = icon("ok",
        lib = "glyphicon"
      )),
    )
  })

  output$patient_Statistic <-
    renderTable(
      summaryTable,
      striped = T,
      hover = T,
      bordered = T,
      align = "c",
      width = "100%"
    )
})
