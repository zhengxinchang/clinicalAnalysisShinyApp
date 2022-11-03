
observe({
  
  
  
  subdat<- selectPtsData()
  
  output$SliderInputAge<- renderUI({
    sliderInput(
      "Surv_Age_bins",
      label = "年龄分组",
      min =  min(na.omit(subdat$age)),
      max = max(na.omit(subdat$age)),
      value = median(na.omit(subdat$age))
    )
  })
  
  output$SliderInputWbc<- renderUI({
    sliderInput(
      "Surv_Wbc_bins",
      label = "White Blood Cell Count分组",
      min =  min(na.omit(subdat$wbc)),
      max = max(na.omit(subdat$wbc)),
      value = median(na.omit(subdat$wbc))
    )
  })
  
  
  
  output$SliderInputAnc<- renderUI({
    sliderInput(
      "Surv_Anc_bins",
      label = "Absolute Neutrophil Count分组",
      min =  min(na.omit(subdat$anc)),
      max = max(na.omit(subdat$anc)),
      value = median(na.omit(subdat$anc))
    )
  })
  
  
  output$SliderInputPlt<- renderUI({
    sliderInput(
      "Surv_Plt_bins",
      label = "Palette分组",
      min =  min(na.omit(subdat$plt)),
      max = max(na.omit(subdat$plt)),
      value = median(na.omit(subdat$plt))
    )
  })
  
  
  output$SliderInputHb<- renderUI({
    sliderInput(
      "Surv_Hb_bins",
      label = "Hemoglobin分组",
      min =  min(na.omit(subdat$hb)),
      max = max(na.omit(subdat$hb)),
      value = median(na.omit(subdat$hb))
    )
  })
  
})
