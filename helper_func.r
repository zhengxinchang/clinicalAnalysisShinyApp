

gen_corr_table_mutation <-
  function(dat, by, strataCol, rowvar, header = "") {
    # by 分组变量，一般是治疗情况
    # name 表格左上角的名称
    # rowvar 一般是需要分组统计的，是name下的水平
    uniqName <-
      dat[, strataCol] %>%
      na.omit() %>%
      unique() %>%
      as.character()
    a1 <- list()
    for (uqName in uniqName) {
      tmpDat <- dat[which(dat[[strataCol]] == uqName), ]
      label(tmpDat[[rowvar]]) <- uqName
      NofLevel <-
        tmpDat[[rowvar]] %>%
        na.omit() %>%
        unique() %>%
        length()
      if (NofLevel >= 2) {
        a1[[uqName]] <-
          getTable1Stats(tmpDat[[rowvar]], strata = tmpDat[[by]])
      }
      else {
        tmpres <-
          getTable1StatsNoPval(tmpDat[[rowvar]], strata = tmpDat[[by]])
        tmpPval <- matrix(rep("", nrow(tmpres)), ncol = 1)
        tmpres2 <- cbind(tmpres, tmpPval)
        a1[[uqName]] <- tmpres2
      }
    }
    tryCatch(
      {
        t <-
          mergeDesc(a1) %>%
          addHtmlTableStyle(css.table = "width:90%") %>%
          htmlTable(caption = header, tfoot = " ")
        print(t)
      },
      error = function(e) {

      }
    )
  }


gen_corr_table <- function(dat, by, name, rowvar, header) {
  tryCatch(
    expr = {
      a1 <- list()
      label(dat[[rowvar]]) <- name
      a1[[name]] <-
        getTable1StatsSafe(dat[[rowvar]], strata = dat[[by]])

      t <- mergeDesc(a1) %>%
        addHtmlTableStyle(css.table = c("width: 100%")) %>%
        htmlTable(caption = header, tfoot = " ")
      return(t)
    },
    error = function(e) {

    }
  )
}


getTable1StatsNoPval <- function(x, strata, digits = 0, ...) {
  getDescriptionStatsBy(
    x = x,
    by = strata,
    digits = digits,
    continuous_fn = describeMedian,
    header_count = TRUE,
    statistics = FALSE,
    show_all_values = TRUE,
    ...
  )
}


getTable1Stats <- function(x, strata, digits = 0, ...) {
  getDescriptionStatsBy(
    x = x,
    by = strata,
    digits = digits,
    continuous_fn = describeMedian,
    header_count = TRUE,
    statistics = TRUE,
    show_all_values = TRUE,
    # useNA = TRUE,
    ...
  )
}

# 安全模式的Tableone表格，
getTable1StatsSafe <- function(x, strata, digits = 0, ...) {
  res <- tryCatch(
    expr = {
      getDescriptionStatsBy(
        x = x,
        by = strata,
        digits = digits,
        continuous_fn = describeMedian,
        header_count = TRUE,
        statistics = TRUE,
        show_all_values = TRUE,
        # useNA = TRUE,
        ...
      )
    },
    error = function(e) {
      tmp <- getDescriptionStatsBy(
        x = x,
        by = strata,
        digits = digits,
        continuous_fn = describeMedian,
        header_count = TRUE,
        statistics = FALSE,
        show_all_values = TRUE,
        # useNA = TRUE,
        ...
      )
      r <- nrow(tmp)
      pcol <- c("NA", rep("", r - 1))
      tmp2 <- cbind(tmp, pcol)
      colnames(tmp2)[length(colnames(tmp2))] <- "P-value"
      return(tmp2)
    }
  )
  return(res)
}

myggbox <- function(dats,
                    x,
                    y,
                    xlab,
                    ylab,
                    title,
                    comp,
                    my_palette) {
  p <-
    ggpubr::ggboxplot(
      dats,
      x = x,
      y = y,
      fill = x,
      palette = my_palette,
      add = "jitter",
      shape = x,
      ylab = ylab,
      xlab = xlab,
      title = title
    ) +
    ggpubr::stat_compare_means(
      comparisons = comp,
      method = "wilcox",
      paired = F
    )
  return(p)
}

myggboxNoCmp <- function(dats,
                         x,
                         y,
                         xlab,
                         ylab,
                         title,
                         comp,
                         my_palette) {
  p <-
    ggpubr::ggboxplot(
      dats,
      x = x,
      y = y,
      fill = x,
      palette = my_palette,
      add = "jitter",
      shape = x,
      ylab = ylab,
      xlab = xlab,
      title = title
    )
  return(p)
}

# create dynamic ui
createDynamicTableRow <-
  function(treatGroup, idPrefix, inline = T) {
    numofCols <- length(treatGroup)

    dynamic_list <- list()

    for (i in seq(1, numofCols)) {
      id <- paste(idPrefix, treatGroup[i], sep = "_")
      id_tag <- paste(id, "Tag", sep = "_")
      id_footerLine <- paste(id, "line", sep = "_")
      dynamic_list[[id_tag]] <- tags$h3(treatGroup[i])
      dynamic_list[[id]] <-
        uiOutput(
          outputId = id,
          inline = inline,
          class = "subRowTable"
        )
      dynamic_list[[id_footerLine]] <- hr()
      
    }
    return(dynamic_list)
  }

createDynamicPlotRow <- function(treatGroup, idPrefix) {
  numofCols <- length(treatGroup)

  dynamic_list <- list()

  for (i in seq(1, numofCols)) {
    id <- paste(idPrefix, treatGroup[i], sep = "_")
    id_tag <- paste(id, "Tag", sep = "_")
    dynamic_list[[id_tag]] <- tags$h3(treatGroup[i])
    dynamic_list[[id]] <-
      plotOutput(outputId = id, height = "1000px")
  }
  return(dynamic_list)
}

#' 生成short induction 的比较
#'
#' @param b clinical outcome column 类型是factor所以变成了
#'
#' @return
#' @export
#'
#' @examples
genPermutaitonDouble <- function(b) {
  a <- na.omit(b)
  lenA <- length(a)
  if (anyNA(b)) {
    lenA <- lenA + 1
  }

  res <- list()
  count <- 0
  tryCatch(
    {
      for (i in seq(1, lenA - 1, 1)) {
        for (j in seq(i + 1, lenA, 1)) {
          tmp <- c(i, j)
          count <- count + 1
          # print(tmp)
          res[[count]] <- tmp
        }
      }
      return(list(flag = TRUE, res = res))
    },
    error = function(e) {
      return(list(flag = FALSE, res = NULL))
    }
  )
}


# 专门生成Surfit对象的函数 新版本  
# 修复：
# 1. CIR的risk table
# 2. CIR的Group 名称
# 3. CIR的EFS1、EFS2
#' [DEPRECATED]
#'
#' @param dat 
#' @param listOfSurvfit 
#' @param Pattle 
#'
#' @return
#' @export
#'
#' @examples
genSurvPlotEachTreatCatetory2 <-
  function(dat, listOfSurvfit, Pattle) {
    NAME <- names(listOfSurvfit)
    res <- list()
    for (t in NAME) {
      if ("RFS" == t) {
        tmp <- tryCatch(
          expr = {
            ggcompetingrisks(
              listOfSurvfit[[t]],
              # gnames = NULL,
              # gsep = " ",
              multiple_panels = F,
              ggtheme = theme_bw(),
              pval = "xdf",
              conf.int = F,
              surv.median.line = "hv",
              # Specify median survival
              palette = Pattle,
              legend.title = list(color = "Event", linetype = "Group")
            )
          },
          error = function(e) {
            print(paste0("CIR Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot() +
              theme_void()
            return(tmp)
          }
        )
      } else if ("EFS" == t) {
        tmp <- tryCatch(
          expr = {
            ggsurvplot(
              listOfSurvfit[[t]],
              data = dat,
              pval = TRUE,
              conf.int = TRUE,
              risk.table = T,
              # Add risk table
              risk.table.col = "strata",
              # Change risk table color by groups
              linetype = "strata",
              # Change line type by groups
              surv.median.line = "hv",
              # Specify median survival
              ggtheme = theme_bw(),
              # Change ggplot2 theme
              legend.title = "Event Free Survival 1",
              palette = Pattle
            )
          },
          error = function(e) {
            print(paste0("EFS Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot()
            return(tmp)
          }
        )
      } else if ("EFS2" == t) {
        tmp <- tryCatch(
          expr = {
            ggsurvplot(
              listOfSurvfit[[t]],
              data = dat,
              pval = TRUE,
              conf.int = TRUE,
              risk.table = T,
              # Add risk table
              risk.table.col = "strata",
              # Change risk table color by groups
              linetype = "strata",
              # Change line type by groups
              surv.median.line = "hv",
              # Specify median survival
              ggtheme = theme_bw(),
              # Change ggplot2 theme
              legend.title = "Event Free Survival 2",
              palette = Pattle
            )
          },
          error = function(e) {
            print(paste0("EFS Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot()
            return(tmp)
          }
        )
      } else if ("OS" == t) {
        tmp <- tryCatch(
          expr = {
            ggsurvplot(
              listOfSurvfit[[t]],
              data = dat,
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
              legend.title = "Overall Survival",
              palette = Pattle
            )
          },
          error = function(e) {
            print(paste0("OS Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot()
            return(tmp)
          }
        )
      }


      if ("RFS" == t) {
        tmpCowplot <-
          cowplot::plot_grid(tmp,
            ggplot() +
              theme_void(),
            ncol = 1,
            rel_heights = c(2, 1)
          )
        res[[t]] <- tmpCowplot
      } else {
        tmpCowplot <-
          cowplot::plot_grid(
            tmp$plot,
            tmp$table + guides(color = F),
            ncol = 1,
            rel_heights = c(2, 1)
          )
        res[[t]] <- tmpCowplot
      }
    }
    a <-
      cowplot::plot_grid(res[["OS"]], res[["EFS"]], res[["EFS2"]], res[["RFS"]], ncol = 2, labels = "AUTO")
    return(list(a, res))
  }

#'[DEPRECATED] Backup for Survival plot generation [DEPRECATED]
#'
#' @param dat
#' @param listOfSurvfit
#' @param Pattle
#'
#' @return
#' @export
#'
#' @examples
genSurvPlotEachTreatCatetory3 <-
  function(dat, listOfSurvfit, Pattle) {
    NAME <- names(listOfSurvfit)
    res <- list()
    for (t in NAME) {
      # strataCategory <- names(listOfSurvfit[[t]]$strata) %>% str_remove("^.*=")
      if ("RFS" == t) {
        # try catch 代码不需要指定warning的处理方式，直接处理error即可

        tmp <- tryCatch(
          expr = {
            ggcompetingrisks(
              listOfSurvfit[[t]],
              # gnames = NULL,
              # gsep = " ",
              multiple_panels = F,
              ggtheme = theme_bw(),
              pval = "xdf",
              conf.int = F,
              surv.median.line = "hv",
              # Specify median survival
              palette = Pattle,
              legend.title = list(color = "Event", linetype = "Group")
            )
          },
          error = function(e) {
            print(paste0("CIR Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot()
            return(tmp)
          }
        )
      } else if ("EFS" == t) {
        tmp <- tryCatch(
          expr = {
            ggsurvplot(
              listOfSurvfit[[t]],
              data = dat,
              pval = TRUE,
              conf.int = TRUE,
              risk.table = T,
              # Add risk table
              risk.table.col = "strata",
              # Change risk table color by groups
              linetype = "strata",
              # Change line type by groups
              surv.median.line = "hv",
              # Specify median survival
              ggtheme = theme_bw(),
              # Change ggplot2 theme
              legend.title = "Event Free Survival",
              palette = Pattle
            )
          },
          error = function(e) {
            print(paste0("EFS Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot()
            return(tmp)
          }
        )
      } else if ("OS" == t) {
        tmp <- tryCatch(
          expr = {
            ggsurvplot(
              listOfSurvfit[[t]],
              data = dat,
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
              legend.title = "Overall Survival",
              palette = Pattle
            )
          },
          error = function(e) {
            print(paste0("OS Survial Plot with errors:", e))
            tmp <- list()
            tmp$plot <- ggplot()
            tmp$table <- ggplot()
            return(tmp)
          }
        )
      }

      if ("RFS" == t) {
        tmpCowplot <-
          cowplot::plot_grid(tmp,
            ggplot(),
            ncol = 1,
            rel_heights = c(2, 1)
          )
        res[[t]] <- tmpCowplot
      } else {
        tmpCowplot <-
          cowplot::plot_grid(
            tmp$plot,
            tmp$table + guides(color = F),
            ncol = 1,
            rel_heights = c(2, 1)
          )
        res[[t]] <- tmpCowplot
      }
    }
    a <-
      cowplot::plot_grid(res[["OS"]], res[["EFS"]], res[["RFS"]], ncol = 2, labels = "AUTO")
    return(list(a, res))
  }





#' Title
#'
#' @param subdat subdata form upper level namespace
#' @param input  input obj from shiny
#'
#' @return list of plot 
#' @export
#'
#' @examples
genSurvPlotIntegrate<-function(subdat,input){
  
  plotlist <- list()
  
  # 循环遍历每种治疗方案：
  for (treat in input$treatGroup) {
    mdata <- subdat %>% filter(Treatment == treat)
  
    fitOS <-
      tryCatch(
        expr = {
          survfit(Surv(OS, Status_OS) ~ Group,
                  data = mdata,
                  na.action = na.omit
          )
        },
        error = function(e) {
          print(paste0("error:", e))
          return(NULL)
        }
      )
    
    fitEFS <-
      tryCatch(
        expr = {
          survfit(Surv(EFS, Status_EFS) ~ Group,
                  data = mdata,
                  na.action = na.omit
          )
        },
        error = function(e) {
          print(paste0("error:", e))
          return(NULL)
        }
      )
    
    fitEFS2 <-
      tryCatch(
        expr = {
          survfit(Surv(EFS2, Status_EFS2) ~ Group,
                  data = mdata,
                  na.action = na.omit
          )
        },
        error = function(e) {
          print(paste0("error:", e))
          return(NULL)
        }
      )
    
    fitRFS <-
      tryCatch(
        expr = {
          # survfit(Surv(RFS, Status_RFS) ~ Group, data = mdata
          # print(mdata)
          # Rdata <- mdata %>% dplyr::select(RFS,Status_RFS,Group) %>%na.omit()
          cuminc(
            ftime = mdata$RFS,
            # failure time variable
            fstatus = mdata$Status_RFS,
            # variable with distinct codes for different causes of failure
            group = mdata$Group,
            # estimates will calculated within groups
            ## strata  = ,  # Tests will be stratified on this variable.
            rho = 0,
            # Power of the weight function used in the tests.
            cencode = 0,
            # value of fstatus variable which indicates the failure time is censored.
            ## subset = ,
            na.action = na.omit
          )
        },
        error = function(e) {
          print(paste0("survival sex error:", e))
          return(NULL)
        }
      )
    
    listfit <- list(
      OS = fitOS,
      EFS = fitEFS,
      EFS2 = fitEFS2,
      RFS = fitRFS
    )
    
    plot <-
      genSurvPlotEachTreatCatetory2(
        dat = mdata,
        listOfSurvfit = listfit,
        Pattle = input$clincical_outcome_palette
      )
    plotlist[[treat]] <- plot[[1]]
  }
  
  return(plotlist)
  
  
}


# 对Render操作中的removeUI和InsertUI进行封装的函数，减少操作，并且自动渲染
#
#' auto remove and create UI (induction1 & induction2)
#'
#' @param divID html ID for manipulation
#' @param treatGroup treatgoup
#' @param data reactive data
#' @param output shiny output object
#'
#' @return NULL
#' @export FALSE
#'
#' @examples
Render_induction <- function(divID, treatGroup, data, output) {
  htmlIDHandelerInd1 <- paste0("#", divID[1])
  htmlIDHandelerInd2 <- paste0("#", divID[2])

  IDprefixInd1 <- paste0(htmlIDHandelerInd1, "_sub")
  IDprefixInd2 <- paste0(htmlIDHandelerInd2, "_sub")

  removeUI(
    selector = paste0(htmlIDHandelerInd1, "  > *", sep = " "),
    multiple = T
  )
  removeUI(
    selector = paste0(htmlIDHandelerInd2, "  > *", sep = " "),
    multiple = T
  )
  insertUI(
    selector = htmlIDHandelerInd1,
    where = "beforeEnd",
    ui = tagList(createDynamicTableRow(treatGroup, idPrefix = IDprefixInd1))
  )
  insertUI(
    selector = htmlIDHandelerInd2,
    where = "beforeEnd",
    ui = tagList(createDynamicTableRow(treatGroup, idPrefix = IDprefixInd2))
  )

  sapply(
    treatGroup,
    FUN = function(treatName) {
      output[[paste(IDprefixInd1, treatName, sep = "_")]] <-
        renderUI({
          print(data$induction1[[treatName]])
        })
      output[[paste(IDprefixInd2, treatName, sep = "_")]] <-
        renderUI({
          print(data$induction2[[treatName]])
        })
    }
  )
}




#' Render Plot in Induction section
#'
#' @param divID UI中的ID
#' @param data  reactive data
#' @param output shiny output object
#' @param input shiny input object
#'
#' @return
#' @export
#'
#' @examples
Render_induction_plot <- function(divID, data, input, output) {
  htmlIDHandeler <- paste0("#", divID)
  subdivID <- paste0(htmlIDHandeler, "_sub")
  removeUI(
    selector = paste0(htmlIDHandeler, "  > *", sep = " "),
    multiple = T
  )
  insertUI(
    selector = htmlIDHandeler,
    where = "beforeEnd",
    ui =
      plotOutput(
        subdivID,
        width = "100%",
        height = paste((length(input$treatGroup) * 500), "px", sep = "")
      )
  )
  output[[subdivID]] <-
    shiny::renderPlot(data)
}



#' Render Survival plot
#'
#' @param divID UI中的ID
#' @param treatGroup 选择的group
#' @param data reactive()
#' @param output  shiny object
#'
#' @return
#' @export
#'
#' @examples
Render_Surv_plot <- function(divID, treatGroup, data, output) {
  selectorID <- paste0("#", divID)
  subID <- paste0(selectorID, "_sub")
  removeUI(
    selector = paste0(selectorID, " >*"),
    multiple = T
  )
  insertUI(
    selector = selectorID,
    ui = createDynamicPlotRow(treatGroup, idPrefix = subID),
    where = "beforeEnd"
  )

  sapply(
    treatGroup,
    FUN = function(treatName) {
      output[[paste(subID, treatName, sep = "_")]] <-
        renderPlot({
          data[[treatName]]
        })
    }
  )
}




genInductionPlot <- function(subdat, VarName, treatGroup, mypalette) {

  # 循环处理每个治疗组
  # 1. 生成两个list，分别是induction1 和induction2
  #
  # 2. 循环遍历每个治疗组
  #
  # 3. 提取数据，并且提取所有的治疗结局事件，重新计算组合
  #
  # 4. 进行作图，并且传入3 的组合，进行统计检验 注意添加治疗组信息
  #
  # 5. 将plot对象加入列表中
  #
  # 6. 结束循环，并且按照治疗组拼接，并且返回

  # print("test1")
  # subdat[, c("induction1_outcome", "induction2_outcome", "Treatment")]
  #
  # print("test finished")
  minidata <- subdat[, c(VarName, "induction1_outcome", "induction2_outcome", "Treatment")]
  print(paste0("all patients: ",nrow(minidata)))
  
  minidata <- minidata[!is.na(minidata[[VarName]]),]
  
  print(paste0("patients not na: ",nrow(minidata)))
  # print(paste0("minidata head :", head(minidata)))
  list_ind1 <- list()
  list_ind2 <- list()

  for (treatName in treatGroup) {
    minidata_treat <- minidata[which(minidata$Treatment == treatName), ]
    
    minidata_treat$induction1_outcome <- droplevels(minidata_treat$induction1_outcome)
    minidata_treat$induction2_outcome <- droplevels(minidata_treat$induction2_outcome)
    
    # print("test finished")
    mycompInd1 <-
      genPermutaitonDouble((minidata_treat$induction1_outcome %>% unique()))
    mycompInd2 <-
      genPermutaitonDouble((minidata_treat$induction2_outcome %>% unique()))

    

    # browser()
        
    if (mycompInd1$flag) {
      ind1 <-
        ggpubr::ggboxplot(
          minidata_treat,
          title = treatName,
          x = "induction1_outcome",
          y = VarName,
          fill = "induction1_outcome",
          palette = mypalette,
          add = "jitter",
          shape = "induction1_outcome",
        ) +
        ggpubr::stat_compare_means(
          comparisons = mycompInd1$res,
          method = "wilcox",
          paired = F
        )
    } else {
      ind1 <-
        ggpubr::ggboxplot(
          minidata_treat,
          title = treatName,
          x = "induction1_outcome",
          y = VarName,
          fill = "induction1_outcome",
          palette = mypalette,
          add = "jitter",
          shape = "induction1_outcome"
        )
    }


    if (mycompInd2$flag) {
      ind2 <-
        ggpubr::ggboxplot(
          minidata_treat,
          title = treatName,
          x = "induction2_outcome",
          y = VarName,
          fill = "induction2_outcome",
          palette = mypalette,
          add = "jitter",
          shape = "induction2_outcome"
        ) +
        ggpubr::stat_compare_means(
          comparisons = mycompInd2$res,
          method = "wilcox",
          paired = F
        )
    } else {
      ind2 <-
        ggpubr::ggboxplot(
          minidata_treat,
          title = treatName,
          x = "induction2_outcome",
          y = VarName,
          fill = "induction2_outcome",
          palette = mypalette,
          add = "jitter",
          shape = "induction2_outcome"
        )
    }

    list_ind1[[treatName]] <- ind1 + theme(plot.title = element_text(hjust = 1))
    list_ind2[[treatName]] <- ind2 + theme(plot.title = element_text(hjust = 1))
  }


  cowplot::plot_grid(

    cowplot::plot_grid(
      plotlist = list_ind1, ncol = 2
    ),
    cowplot::plot_grid(
      plotlist = list_ind2, ncol = 2
    ),
    ncol = 1, labels = c("Induction 1", "Induction 2"), vjust = 1.0,label_size = 20
  )

}



#' Title
#'
#' @param ID 
#' @param SliderID 
#' @param label 
#' @param datamin 
#' @param datamax 
#' @param datamidvalue 
#'
#' @return
#' @export
#'
#' @examples
genSurvInputSlider<-function(ID,SliderID,label,data){
  sharp_ID = paste0("#",ID)
  removeUI(
    selector = paste0( sharp_ID, " >*"),
    multiple = T
  )
  insertUI(
    selector = sharp_ID,
    where = "afterEnd",
    ui = sliderInput(
      SliderID,
      label = label,
      min =  min(na.omit(data)),
      max = max(na.omit(data)),
      value = median(na.omit(data))
    )
  )
  
}
