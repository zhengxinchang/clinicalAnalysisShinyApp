
gen_corr_table_mutation<-function(dat,by,strataCol,rowvar,header=""){
  
  # by 分组变量，一般是治疗情况
  # name 表格左上角的名称
  # rowvar 一般是需要分组统计的，是name下的水平
  # 
  
  #dat[which(dat[[rowvar]] == "NA"),]<-NA
  
  uniqName <- dat[,strataCol] %>% na.omit() %>%  unique() %>% as.character()
  a1 <- list()
  for (uqName in uniqName){
    #print(uqName)
    tmpDat<- dat[which(dat[[strataCol]] == uqName),] 
    #print(tmpDat)
    
    label(tmpDat[[rowvar]]) <- uqName
    
    NofLevel <- tmpDat[[rowvar]] %>%na.omit() %>%unique() %>%length()
    #print(NofLevel)
    if(NofLevel>=2){
      
      a1[[uqName]] <-getTable1Stats(tmpDat[[rowvar]],strata = tmpDat[[by]])
      #print(a1[[uqName]])
    }
    else{
      tmpres<-getTable1StatsNoPval(tmpDat[[rowvar]],strata = tmpDat[[by]])
      tmpPval <- matrix(rep("",nrow(tmpres)),ncol=1)
      tmpres2<-cbind(tmpres,tmpPval)
      a1[[uqName]] <- tmpres2
      #print(a1[[uqName]])
    }
    }
  tryCatch({
    t<-mergeDesc(a1)  %>% addHtmlTableStyle(css.table = "width:90%" ) %>%
      htmlTable(caption  = header, tfoot = " ")
    print(t)    
  },error=function(e){
    
  })
}

gen_corr_table<-function(dat,by,name,rowvar,header){
    tryCatch(expr = {
      a1 <- list()
      label(dat[[rowvar]]) <- name
      a1[[name]] <-
        getTable1Stats(dat[[rowvar]],strata=dat[[by]])
      
      t<-mergeDesc(a1) %>%
        addHtmlTableStyle(css.table = c("width: 47%;float:left;margin-right:2%")) %>%
        htmlTable(caption  = header, tfoot = " ")
      return(t)    
    },error=function(e){
      
    })
  
  }

getTable1StatsNoPval <- function(x, strata,digits = 0, ...){
  getDescriptionStatsBy(x = x,
                        by = strata,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        statistics = FALSE,
                        show_all_values = TRUE,
                        ...)
}


getTable1Stats <- function(x, strata,digits = 0, ...){
  getDescriptionStatsBy(x = x,
                        by = strata,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        statistics = TRUE,
                        show_all_values = TRUE,
                        ...)
}

myggbox<-function(dats,x,y,xlab,ylab,title,comp,my_palette){
  
  p<-ggpubr::ggboxplot(dats,x=x,y=y,fill=x, palette = my_palette ,add = 'jitter',
                       shape=x,ylab =ylab,
                       xlab = xlab,title=title) +
    ggpubr::stat_compare_means(comparisons = comp,method = 'wilcox',paired = F)
  return(p)
}

myggboxNoCmp<-function(dats,x,y,xlab,ylab,title,comp,my_palette){
  
  p<-ggpubr::ggboxplot(dats,x=x,y=y,fill=x, palette = my_palette ,add = 'jitter',
                       shape=x,ylab =ylab,
                       xlab = xlab,title=title)
  return(p)
}

# create dynamic ui 

createDynamicTableRow<-function(treatGroup,idPrefix,inline=T){
  
  numofCols = length(treatGroup)
  
  dynamic_list = list()

  for(i in seq(1,numofCols)){
    id <- paste(idPrefix,treatGroup[i],sep="_")

    dynamic_list[[id]] <- uiOutput(outputId = id,inline = inline ,class="subRowTable")
  } 
  return(dynamic_list)
}

createDynamicPlotRow<-function(treatGroup,idPrefix){
  
  numofCols = length(treatGroup)
  
  dynamic_list = list()
  
  for(i in seq(1,numofCols)){
    id <- paste(idPrefix,treatGroup[i],sep="_")
    
    dynamic_list[[id]] <- plotOutput(outputId = id,height = "1000px")
  } 
  return(dynamic_list)
}

# 两两组合
# 输入，一个向量
# 输出，一个列表，包含多个两两组合的向量
genPermutaitonDouble<- function(b){
  a <- na.omit(b)
  lenA <- length(a)
  res <-list()
  count = 0
  tryCatch({
    for(i in seq(1,lenA-1,1)){
      for(j in seq(i+1,lenA,1)){
        tmp<-c(a[i],a[j])
        count = count +1
        #print(tmp)
        res[[count]] <-tmp
      }
    }
    return(list(flag=TRUE,res=res))  
  },error=function(e){
    return(list(flag=FALSE,res=NULL))
      })
}


# 专门生成原版的Survplot的函数 ----
# 在后期需要修改 适合EFS1、EFS2
# 输入：一个列表，名称是不同类型生存曲线名称，值是Survfit对象，
# 输入：调色板，接收input$...palette参数
# 输入：dataframe，用于构建列表的数据框
# 
# 
# 
genSurvPlotEachTreatCatetory2<-function(dat,listOfSurvfit,Pattle){
  
  NAME <- names(listOfSurvfit)
  
  res<- list()
  
  
  for (t in NAME){
    
    strataCategory <- names(listOfSurvfit[[t]]$strata) %>% str_remove("^.*=")
    if("RFS"==t){
      
      # tmp<-ggsurvplot(listOfSurvfit[[t]],data = dat,
      #            pval = TRUE, conf.int = TRUE,
      #            risk.table = TRUE, # Add risk table
      #            risk.table.col = "strata", # Change risk table color by groups
      #            linetype = "strata", # Change line type by groups
      #            surv.median.line = "hv", # Specify median survival
      #            ggtheme = theme_bw(), # Change ggplot2 theme
      #            legend.title="Cumulative Incidence of Relapse",
      #            palette =Pattle,fun="event")
      tmp<-tryCatch(expr = {
        
        ggsurvplot(listOfSurvfit[[t]],data = dat,
                        pval = TRUE, conf.int = TRUE,
                        risk.table = TRUE, # Add risk table
                        risk.table.col = "strata", # Change risk table color by groups
                        linetype = "strata", # Change line type by groups
                        surv.median.line = "hv", # Specify median survival
                        ggtheme = theme_bw(), # Change ggplot2 theme
                        legend.title="Cumulative Incidence of Relapse",
                        palette =Pattle,fun="event")
      },
      # warning=function(w){
      #   tmp = list()
      #   tmp$plot<-ggplot()
      #   tmp$table <- ggplot()
      #   return(tmp)
      # },
      error=function(e){
        tmp = list()
        tmp$plot<-ggplot()
        tmp$table <- ggplot()
        return(tmp)
        
      })
      
    }else if("EFS"==t){
      tmp<-tryCatch(expr={
      ggsurvplot(listOfSurvfit[[t]],data = dat,
                 pval = TRUE, conf.int = TRUE,
                 risk.table = T, # Add risk table
                 risk.table.col = "strata", # Change risk table color by groups
                 linetype = "strata", # Change line type by groups
                 surv.median.line = "hv", # Specify median survival
                 ggtheme = theme_bw(), # Change ggplot2 theme
                 legend.title="Event Free Survival",
                 palette =Pattle)
      },
      # warning=function(w){
      #   tmp = list()
      #   tmp$plot<-ggplot()
      #   tmp$table <- ggplot()
      #   return(tmp)
      # },
      error=function(e){
        tmp = list()
        tmp$plot<-ggplot()
        tmp$table <- ggplot()
        return(tmp)
        
      })
    }else if("OS"==t){
      
      tmp<-tryCatch(expr={
      ggsurvplot(listOfSurvfit[[t]],data = dat,
                      pval = TRUE, conf.int = TRUE,
                      risk.table = TRUE, # Add risk table
                      risk.table.col = "strata", # Change risk table color by groups
                      linetype = "strata", # Change line type by groups
                      surv.median.line = "hv", # Specify median survival
                      ggtheme = theme_bw(), # Change ggplot2 theme
                      legend.title="Overall Survival",
                      palette =Pattle)
    },
    # warning=function(w){
    #   tmp = list()
    #   tmp$plot<-ggplot()
    #   tmp$table <- ggplot()
    #   print("Plot Subunit Warning..")
    #   return(tmp)
    #   
    # },
    error=function(e){
      tmp = list()
      tmp$plot<-ggplot()
      tmp$table <- ggplot()
      return(tmp)
      
    })
      
    }
    tmpCowplot<- cowplot::plot_grid(tmp$plot,tmp$table+guides(color=F),ncol=1,rel_heights = c(2,1))
    res[[t]] <- tmpCowplot
    
  }
  
  #print(res)
  a<-cowplot::plot_grid(res[["OS"]],res[["EFS"]],res[["RFS"]],ncol = 2,labels = "AUTO")
  return(list(a,res))
}


## test code for genSurvPlotEachTreatCatetory2 
## 
## 
# fitOS <-survfit(Surv(OS,Status_OS)~sex_category,data=mdata)
# fitEFS <-survfit(Surv(EFS,Status_EFS)~sex_category,data=mdata)
# fitRFS <-survfit(Surv(RFS,Status_RFS)~sex_category,data=mdata)
# 
# DAT<- list(OS=fitOS,EFS=fitEFS,RFS=fitRFS)
# 
# genSurvPlotEachTreatCatetory2(mdata,listOfSurvfit = DAT,Pattle = "jco")
