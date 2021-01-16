
if ( ! suppressWarnings (require(dplyr))){print("Can not load labrary dplyr...")}
if ( ! suppressWarnings (require(jsonlite))){print("Can not load labrary jsontile...")}
if ( ! suppressWarnings (require(stringr))){print("Can not load labrary stringr...")}

# 设置通用的校验table的函数，包括校验数据框的列数，每列名称以及数据类型
# 每个table都有函数对其校验，此函数中硬编码此table的信息，进行校验
# 每个table有调用方法类似的函数获取JSON，转化为数据框，挑选数据并且清洗数据。并调用 1） 2）中的函数进行数据校验

# 辅助函数，用于拼接消息
concatMessage<-function(M,S){
  res <- paste(M,S,sep = "\n")
  return(res)
}

# 数据类型校验失败，则返回False,如果成功则返回True
validateTypeColumn<-function(df,numcol,colclasses){
  
  # parameters type validataion
  # if (! is.)
  
 
  # validate the type of df ,number of column and type of each column
  valid <- TRUE
  Message <- ""
  
  if ( ! is.data.frame(df)){valid <- FALSE;Message<- concatMessage(Message,"input is not a data frame...")}
  
  if ( numcol !=  ncol(df)){valid <- FALSE;Message<- concatMessage(Message,"column number error...")}
  
  for ( k in unique(colclasses)){
    if (!(k %in% c("numeric","character","factor","logical","integer","double"))){
      Valid <- FALSE
      Message<- concatMessage(Message,"ColClasses type is not support...")
      break
      }
  }
  
  #应该构建一个DF的列名称对应类型的向量
  
  for (n in names(colclasses)){
    if(class(df[[n]]) != colclasses[[n]]){
      valid <-FALSE 
      Message <- concatMessage(Message,paste0("columns name or type error... name is :",n))
    }
  }
  return(list(Valid=valid,Message=Message))
}

# 对patientdata的校验 调用了validateTypeColumn
validateDataFramePatient<- function(patientdf){
  ## 这里构建对Patient DF的校验，主要是列名和对应的列的类型。输入一个df，其他参数硬编码进这个函数中
  
  ## 合法的列数
  dfncol <- 10
    
  ## 合法的列名称以及对应的列的数据类型
  dfColClass <- c("birthday"="double",
                  "aml_classify"="integer",
                  "patient_id"="integer",
                  "sex"="integer",
                  "diagnosis_date"="character",
                  "is_relapse"="logical",
                  "aml_risk"="integer",
                  "is_transplant"="logical",
                  "hospital_id"="integer",
                  "aml_group"="integer"
  )
    
  valid_res <- validateTypeColumn(patientdf,dfncol,dfColClass)
    
  return(valid_res)  
}

# 主接口，获得patient接口 -------------------
requestAndCleanDataframePatient<- function(){
  patientdf <- jsonlite::fromJSON("http://bigd.big.ac.cn/amltest/dataAnalysis/patientData")
  
  for(j in seq(1,nrow(patientdf))) {patientdf$birthday[j] <- convertOneCellCharacter2Date(patientdf$birthday[j])}
  for(j in seq(1,nrow(patientdf))) {patientdf$diagnosis_date[j] <- convertOneCellCharacter2Date(patientdf$diagnosis_date[j])}
  
  valid_res<- validateDataFramePatient(patientdf)
  return(list(valid=valid_res$Valid,valid_message=valid_res$Message,data=patientdf))
}

# 辅助函数，用于校验列的合法性，initaldata中使用
convertPosNegFlagOneCell<-function(s){
  
  flag <- "UNDIFINE"
  
  if(is.na(s)){
    flag <- NA
    return(flag)
  }
  
  
  if (stringr::str_detect(string = s,pattern = "-")){
    flag<- "negative"
  }else if(stringr::str_detect(string = s,pattern = "阴")){
    flag<- "negative"
    
  }else if(stringr::str_detect(string = s,pattern = "阴性")){
    flag <- "negative"
  }else if(stringr::str_detect(string = s,pattern = "^$")){
    flag <- NA
  }else if(stringr::str_detect(string = s,pattern = "^ +$")){
    flag <- NA
  }else if(stringr::str_detect(string = str_to_upper(s),pattern ="^ *NA *$")){
    flag <- NA
  }else if(stringr::str_detect(string = str_to_upper(s),pattern ="^ *NONE *$")){
    flag <- NA
  }else if(stringr::str_detect(string = str_to_upper(s),pattern ="^ *NULL *$")){
    flag <- NA
  }
  else {
    flag <- "positive"
  }
  
  return(flag)
  
  # 返回四种状态，positive negative NA 以及 UNDIFINE ，需要判定是否是UNDIFINE，这种异常状态
}

# 辅助函数，用于校验列的合法性，initaldata中使用
convertCharacter2Number<-function(s){
  
  # 逻辑：
  # 去除空格，
  # 检测*10^9/L字样，如果有去除
  # 检测g/L字样，如果有去除
  # 检测%字样，如果有直接返回 numeric -1 （错误代码）
  # 检测 - 字样如果有直接返回NA （非字符）
  # 检测 "" " " 直接转化为 NA（非字符）
  
  num<- -1
  
  if(is.na(s)){return(NA)}
  
  s2<- str_trim(s,side = "both")
  s2<- stringr::str_to_upper(s2)
  
  if(stringr::str_detect(s2,pattern = "%")){
    num<- NA
    return(num)
  }else if (stringr::str_detect(s2,pattern = "^-+$")){
    num<- NA
    return(num)
  }else if(stringr::str_detect(s2,pattern = "^ \\+$")){
    num<- NA
    return(num)
  }else if(stringr::str_detect(s2,pattern = "^$")){
    num<- NA
    return(num)
  }
  
  
  if(stringr::str_detect(s2,pattern = "[\\*Xx]10\\^9/L")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=[\\*Xx]10\\^9/L)") # 正向零宽断言，前向。
  }else if (stringr::str_detect(s2,pattern = "G/L")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=G/L)")
  }
  #print("aa")
  tryCatch(expr = {
    num <- as.numeric(s2)
  },
  warning=function(w){
    #print("warning")
    num <- -1
  },
  error=function(e){
    #print("error")
    num <- -1
  })
  return(num)
  
}

# 对initialdata的校验 调用了validateTypeColumn
validateDataFrameInitials<- function(initialdf){
  ## 这里构建对Patient DF的校验，主要是列名和对应的列的类型。输入一个df，其他参数硬编码进这个函数中
  
  ## 合法的列数
  dfncol <- 23
  
  ## 合法的列名称以及对应的列的数据类型
  dfColClass <- c(
      "mt_runx1"= "character",
      "pcr_mll_af10"= "character",
      "mt_ckit"= "character",
      "mt_nras"= "character",
      "mt_cebp"= "character",
      "pcr_mll_af9"= "character",
      "fish_bcr_abl1"= "character",
      "fish_mll"= "character",
      "mt_asxl1"= "character",
      "fish_cbf_myh11"= "character",
     "pcr_mll_af6"= "character",
     "fish_aml_eto"= "character",
     "pcr_mll_af4"= "character",
     "pcr_mll_af1"= "character",
     "patient_id"= "integer",
     "anc"= "numeric",
     "pcr_aml_eto"= "character",
     "wbc"= "numeric",
     "plt"= "numeric",
     "mt_npm1"= "character",
     "hb"= "numeric",
     "mt_flt31td"= "character",
     "pcr_cbf_myh11"= "character"
  )
  
  valid_res <- validateTypeColumn(initialdf,dfncol,dfColClass)
  return(valid_res)  
}

# 主接口，获得initial接口 -------------------
requestAndCleanDataframeInitial<- function(){
  
  initialdf <- jsonlite::fromJSON("http://bigd.big.ac.cn/amltest/dataAnalysis/initialData")
  
  tryCatch({
    # select data
    tmp<- initialdf %>% dplyr::select(patient_id,
                                      anc,
                                      wbc,
                                      plt,
                                      hb,
                                      mt_runx1,
                                      mt_ckit,
                                      mt_nras,
                                      mt_cebp,
                                      mt_asxl1,
                                      mt_npm1,
                                      mt_flt31td,
                                      pcr_mll_af10,
                                      pcr_mll_af9,
                                      pcr_mll_af6,
                                      pcr_mll_af4,
                                      pcr_mll_af1,
                                      pcr_aml_eto,
                                      pcr_cbf_myh11,
                                      fish_bcr_abl1,
                                      fish_mll,
                                      fish_cbf_myh11,
                                      fish_aml_eto)                                       
    
    # clean data
    # 分为突变数据，pcr数据，fish数据，这三类数据的矫正通过限定空值和阴性值，进而区分阳性值。
    # 设定 有字符 "阴“ “阴性“ ”-“的为阴性，”“，NA，以及空为未填，其他为阳性。
    for (i in c("mt_runx1",
                "mt_ckit",
                "mt_nras",
                "mt_cebp",
                "mt_asxl1",
                "mt_npm1",
                "mt_flt31td",
                "pcr_mll_af10",
                "pcr_mll_af9",
                "pcr_mll_af6",
                "pcr_mll_af4",
                "pcr_mll_af1",
                "pcr_aml_eto",
                "pcr_cbf_myh11",
                "fish_bcr_abl1",
                "fish_mll",
                "fish_cbf_myh11",
                "fish_aml_eto"
                )){
      
      for (j in seq(1,nrow(tmp))){
        #print(j)
        tmp[[i]][j] <- convertPosNegFlagOneCell(tmp[[i]][j])
      
      }
    }
    
    
    #对于WBC，hb，anc,plt数据，本应该是numeric类型，但是数据库中设计表所用类型为varchar，因此这里需要转换,这个问题大部分可通过正则匹配的方式修复，但是
    #一些数据点的数据项目填错，需要医院方校验，例如将ANC（绝对中性粒细胞计数）错填成中性粒细胞比例。
  
    for(j in c(
      "anc",
      "wbc",
      "plt",
      "hb"
    )){
      
      #rint(j)
      for (k in seq(1,nrow(tmp))){
        #print(k)
        tmp[[j]][k] <- convertCharacter2Number(tmp[[j]][k])
      }
      
      tmp[[j]]<-as.numeric(tmp[[j]])
    }
    
  })

  
  # validate data
  valid_res<- validateDataFrameInitials(initialdf = tmp )
  
  return(list(valid=valid_res$Valid,valid_message=valid_res$Message,data=tmp))
  
}

# 获得period表格后可以抽提出以下几个信息
# 1. induction1/2的治疗状态，血常规恢复时间等信息
# 2. 血常规时序数


getJsonPeroid<-function(){
  # http://localhost:9196/amltest/dataAnalysis/archiveData
  #Peroiddf <- jsonlite::fromJSON("http://localhost:9196/amltest/dataAnalysis/periodData")
  Peroiddf <- jsonlite::fromJSON("http://bigd.big.ac.cn/amltest/dataAnalysis/periodData")
  return(Peroiddf)
}

# 获得Period到全局命令空间中
PERIOD<- getJsonPeroid()


# 辅助函数 转换induction疗程的数值类型

convertCharacter2NumberOneCell_Induction<- function(s){
  # 逻辑：
  # 去除空格，
  # 检测*10^9/L字样，如果有去除
  # 检测g/L字样，如果有去除
  # 检测%字样，如果有直接返回 numeric -1 （错误代码）
  # 检测 - 字样如果有直接返回NA （非字符）
  # 检测 "" " " 直接转化为 NA（非字符）
  
  num<- -1
  
  if(is.na(s)){return(NA)}
  
  if(is.numeric(s) | is.integer(s)){return(s)}
  
  s2<- str_trim(s,side = "both")
  s2<- stringr::str_to_upper(s2)
  
  if (stringr::str_detect(s2,pattern = "^-+$")){
    num<- NA
    return(num)
  }else if(stringr::str_detect(s2,pattern = "^ \\+$")){
    num<- NA
    return(num)
  }else if(stringr::str_detect(s2,pattern = "^$")){
    num<- NA
    return(num)
  }
  
  
  if(stringr::str_detect(s2,pattern = "[\\*Xx]10\\^9/L")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=[\\*Xx]10\\^9/L)") # 正向零宽断言，前向。
  }else if (stringr::str_detect(s2,pattern = "G/L")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=G/L)")
  }else if (stringr::str_detect(s2,pattern = "KG")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=KG)")
  }else if (stringr::str_detect(s2,pattern = "M")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=M)")
  }else if (stringr::str_detect(s2,pattern = "%")){
    s2 <- stringr::str_extract(s2,pattern = "(\\d+\\.*?\\d+)(?=%)")
  }
  
  #print("aa")
  tryCatch(expr = {
    num <- as.numeric(s2)
  },
  warning=function(w){
    #print("warning")
    num <- -1
  },
  error=function(e){
    #print("error")
    num <- -1
  })
  return(num)
  
}

# 辅助函数校验indecton1 数据类型

validateDataFrameInduction1<- function(){
  
  
}


# 主接口，获得induction1接口 -------------------
requestAndCleanDataframeInduction1 <- function(PERIOD){
  
  Induction1df <- PERIOD %>% dplyr::select(
    period_code, 
    period_id,
    d26_leukemia_percent, 
    period_cost,
    patient_id,
    nth_day,
    d26_marrow_estimate,
    weight,
    high,
    wbc,  # white blood count in each day
    blood_platelet, # platelet count in each day
    plt_recover_time, 
    anc_recover_time )%>% 
    filter(period_code =="1" & nth_day  ==-1) %>% 
    arrange(patient_id)
  
  
  for(i in c(
    "period_code", 
    "period_id",
    "d26_leukemia_percent", 
    "period_cost",
    "patient_id",
    "nth_day",
    "d26_marrow_estimate",
    "weight",
    "high",
    "wbc",  # white blood count in each day
    "blood_platelet", # platelet count in each day
    "plt_recover_time", 
    "anc_recover_time"
  )){
    
    for ( j in seq(1,nrow(Induction1df))){
      Induction1df[[i]][j] <- convertCharacter2NumberOneCell_Induction(Induction1df[[i]][j])
      
    }
  }
  
  Induction1dfgoodName<- Induction1df %>% rename_all(.funs = function(x){str_c("induction1",str_trim(x,side = "both"),sep="_")})
  
  
  return(list(data=Induction1dfgoodName))
}


# 主接口，获得induction2接口 -------------------
requestAndCleanDataframeInduction2 <- function(PERIOD){
  
  Induction2df <- PERIOD %>% dplyr::select(
    period_code, 
    period_id,
    d26_leukemia_percent, 
    period_cost,
    patient_id,
    nth_day,
    d26_marrow_estimate,
    weight,
    high,
    wbc,  # white blood count in each day
    blood_platelet, # platelet count in each day
    plt_recover_time, 
    anc_recover_time )%>% 
    filter(period_code =="2" & nth_day  ==-1) %>% 
    arrange(patient_id)
  
  
  for(i in c(
    "period_code", 
    "period_id",
    "d26_leukemia_percent", 
    "period_cost",
    "patient_id",
    "nth_day",
    "d26_marrow_estimate",
    "weight",
    "high",
    "wbc",  # white blood count in each day
    "blood_platelet", # platelet count in each day
    "plt_recover_time", 
    "anc_recover_time"
  )){
    
    for ( j in seq(1,nrow(Induction2df))){
      Induction2df[[i]][j] <- convertCharacter2NumberOneCell_Induction(Induction2df[[i]][j])
      
    }
  }
  
  Induction2dfgoodName<- Induction2df %>% rename_all(.funs = function(x){str_c("induction2",str_trim(x,side = "both"),sep="_")})
  
  
  return(list(data=Induction2dfgoodName))
}


# 辅助函数，转换字符串到日期

convertOneCellCharacter2Date <- function(s){
  
  res <-NA
  s2 <- stringr::str_trim(s,side = "both")
  tryCatch({
    res<- as.Date(s2)
  },e= function(e){
    res <- NA
  },w = function(w){
    res <- NA 
  }
  )
  return(res)
}

# 主接口，获得survivaldata接口 -------------------
requestAndCleanDataframeArchive <- function(){
  
  #archiveJson <- jsonlite::fromJSON("http://localhost:9196/amltest/dataAnalysis/archiveData")
  archiveJson <- jsonlite::fromJSON("http://bigd.big.ac.cn/amltest/dataAnalysis/archiveData")
  
  archivedf <- archiveJson %>% 
              dplyr::select(patient_id,
                            follow_up_date,
                            dead_complication,
                            treatment,
                            is_relapse,
                            relapse_date,
                            is_dead,
                            dead_date,
                            is_abandon,
                            abandon_date
              )
  
  
  for(i in c(
    "follow_up_date",
    "relapse_date",
    "dead_date",
    "abandon_date"
  )){
    for (j in seq(1,nrow(archivedf))){
      
      archivedf[[i]][j] <- convertOneCellCharacter2Date(archivedf[[i]][j])
    }
  }
  
  return(archivedf)
  # 返回的日期接口是  
}



# 主接口，获得SeriesData接口 ----------------

requestAndCleanDataframeSeriesData<- function(PERIOD){
  Seriesdf <- PERIOD %>% dplyr::select(
    period_code, 
    period_id,
    patient_id,
    nth_day,
    weight,
    high,
    wbc,  
    plt,
    hb,
    anc,
    blood_platelet)%>%
    filter((period_code =="1"| period_code =="2") & nth_day !=-1) %>% 
    arrange(patient_id)
  
  
  for(i in c(
    "period_code", 
    "period_id",
    "patient_id",
    "nth_day",
    "weight",
    "high",
    "wbc", 
    "plt",
    "anc",
    "hb",
    "blood_platelet")){
    
    for ( j in seq(1,nrow(Seriesdf))){
      Seriesdf[[i]][j] <- convertCharacter2NumberOneCell_Induction(Seriesdf[[i]][j])
    }
  }
  
  SeriesdfGoodName<- Seriesdf %>% rename_all(.funs = function(x){stringr::str_c("Series",x,sep="_")})
  
  return(list(data=SeriesdfGoodName))
  
}
