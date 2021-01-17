
## 渲染 -------------------
observeEvent(input$treatGroup_analysis, {
  if (length(input$treatGroup) < 2) {
    print(paste0(
      "select:",
      length(input$treatGroup),
      "group:",
      input$treatGroup
    ))
    session$sendCustomMessage(type = "testmessage", message = "请至少选择2组进行后续分析")
  } else {
    withProgress(
      message = "Calculating...",
      min = 0,
      max = 15,
      expr = {

        # 为后续的动态生成 Survival ~ 连续变量的 的SliderInput准备数据        
        subdat<- reactive_subdata()

        # 渲染概览模块
        {
          ## 绘制table1
          output$Table1 <-
            renderUI({
              reactive_table1()
            })
          ## 绘制短期治疗反应
          output$short_outcome <-
            renderUI({
              reactive_short_outcome()
            })
          ## 绘制长期治疗反应
          output$Survival <-
            renderPlot({
              reactive_long_outcome()
            })
          ## 绘制不良事件
          output$deathrelapsetransplant <-
            renderUI({
              reactive_adverse_event()
            })
          ## 绘制副作用
          output$sideeffect <-
            renderUI({
              reactive_sideeffect()
            })

          ## 绘制治疗费用
          output$costs <- renderUI({
            reactive_cost()
          })
          ## 绘制血常规时序图
          output$RoutineBlood <- renderPlot({
            reactive_routine_blood()
          })
        }
        incProgress(5)
        # 渲染短期治疗反应
        {
          shortSexList <-
            reactive_sex_induction()
          Render_induction(c("ind1_sex_div", "ind2_sex_div"), input$treatGroup, shortSexList, output)

          ## shrot risk
          shortRiskList <-
            reactive_risk_induction()
          Render_induction(c("ind1_risk_div", "ind2_risk_div"), input$treatGroup, shortRiskList, output)

          ## short fab

          shortFABList <-
            reactive_FAB_induction() # list(induciton1=,induction2=)

          Render_induction(c("ind1_FAB_div", "ind2_FAB_div"), input$treatGroup, shortFABList, output)

          ## short mutation
          shortMutList <-
            reactive_GeneMut_induction() # list(induciton1=,induction2=)
          Render_induction(c("ind1_GeneMut_div", "ind2_GeneMut_div"), input$treatGroup, shortMutList, output)

          ## short age
          Render_induction_plot("induction_Age_div", reactive_Age_induction(), input, output)

          ## short wbc
          Render_induction_plot("induction_Wbc_div", reactive_Wbc_induction(), input, output)

          ## short anc
          Render_induction_plot("induction_Anc_div", reactive_Anc_induction(), input, output)

          ## shrot plt
          Render_induction_plot("induction_Plt_div", reactive_Plt_induction(), input, output)

          ## short hb
          Render_induction_plot("induction_Hb_div", reactive_Hb_induction(), input, output)
        }
        incProgress(5)
        # 渲染长期生存
        {
          ## survival sex
          Render_Surv_plot("surv_sex_div", input$treatGroup, reactive_sex_survival(), output)

          ## survival risk
          Render_Surv_plot("surv_risk_div", input$treatGroup, reactive_risk_survival(), output)

          ## survival fab
          Render_Surv_plot("surv_fab_div", input$treatGroup, reactive_fab_survival(), output)

          ## survival mutation
          Render_Surv_plot("surv_genemut_div", input$treatGroup, reactive_genemut_survival(), output)

          
          # age
          genSurvInputSlider("SliderInputAge","Surv_Age_bins","年龄分组",subdat$age)
          ## survival age
          Render_Surv_plot("surv_age_div", input$treatGroup, reactive_age_survival(), output)

          
          # wbc
          genSurvInputSlider("SliderInputWbc","Surv_Wbc_bins","White Blood Cell Count分组",subdat$wbc)
          ## survival wbc
          Render_Surv_plot("surv_wbc_div", input$treatGroup, reactive_wbc_survival(), output)

          
          # anc
          genSurvInputSlider("SliderInputAnc","Surv_Anc_bins","Absolute Neutrophil Count分组",subdat$anc)
          ## survival anc
          Render_Surv_plot("surv_anc_div", input$treatGroup, reactive_anc_survival(), output)

          
          #plt
          genSurvInputSlider("SliderInputPlt","Surv_Plt_bins","Platete分组",subdat$plt)
          ## survival plt
          Render_Surv_plot("surv_plt_div", input$treatGroup, reactive_plt_survival(), output)
          
          
          #hb
          genSurvInputSlider("SliderInputHb","Surv_Hb_bins","Hemoglobin分组",subdat$hb)
          ## survival hb
          Render_Surv_plot("surv_hb_div", input$treatGroup, reactive_hb_survival(), output)
        }
        incProgress(5)

        # 展示结果界面
        {
          showTab(
            inputId = "nav",
            target = "结果",
            select = T
          )
        }
      }
    )
  }
})
