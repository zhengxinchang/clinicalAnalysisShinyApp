
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
if(!suppressWarnings({require(htmlTable)})){print("Load Package htmlTable Failed")}




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
series_data_long <- data_list$series_data_long

# functions
gen_corr_table<-function(dat,by,name,rowvar,header=""){
  a1 <- list()
  label(dat[[rowvar]]) <- name
  a1[[name]] <-
    getTable1Stats(dat[[rowvar]],dat[[by]],)
  # t<-mergeDesc(a1) %>% 
  #   addHtmlTableStyle(css.cell = c("width: 5000px;")) %>% 
  #   htmlTable(caption  = header, tfoot = " ")
  # print(t)
  return(a1)
  
}


# 确定分组的数量，后续提升，调整顺序，放到dataprepare.r 添加每组数量信息
treat_group <- unified_data %>% dplyr::select(Treatment) %>% unique()
treat_group_list <- treat_group$Treatment  %>% 
  factor(levels = c("LDC","SDC","NonRandom_LDC","NonRandom_SDC","LDC_M7","RR_A","RR_B","RR_C","RR_D")) %>% as.character()%>%
  as.list()

subdat <- unified_data[unified_data$Treatment %in% c("SDC","LDC"),]

LDCdat <- subdat %>% filter(Treatment=="LDC")
SDCdat <- subdat %>% filter(Treatment=="SDC")


LDCind1<- gen_corr_table(LDCdat,by="induction1_outcome","Sex","sex_category","LDC Induction1")
LDCind2<- gen_corr_table(LDCdat,by="induction2_outcome","Sex","sex_category","LDC Induction2")
SDCind1<- gen_corr_table(SDCdat,by="induction1_outcome","Sex","sex_category","SDC Induction1")
SDCind2<- gen_corr_table(SDCdat,by="induction2_outcome","Sex","sex_category","SDC Induction2")

subdat$fakeoutcome <- sample(c("CR","PR","NR"),size = nrow(subdat),replace = T)
LDCindage<- gen_corr_table(LDCdat,by="fakeoutcome","Age","age","LDC Induction1")

# l1<- mergeDesc(b) %>% 
#     addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
#     htmlTable(caption  = "header", tfoot = " ")
# l2 <- mergeDesc(LDCind2) %>% 
#   addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
#   htmlTable(caption  = "header", tfoot = " ")

b<-cbind(LDCind1$Sex ,LDCind2$Sex)

c<- mergeDesc(b) %>% 
  addHtmlTableStyle(css.cell = c("width: 5000px;")) %>%
  htmlTable(caption  = "header", tfoot = " ")


d<-htmlTable(b, cgroup = c("Cgroup 1", "Cgroup 2"),  n.cgroup = c(2,2))

concatHtmlTables(list(d,d))




describeFactors(x = LDCdat$sex_category,
                     
                      digits = 0,
                      useNA = "always",
                      
)

source("./descriptionStats.R")


by(LDCdat$sex_category, LDCdat$induction1_outcome, FUN=class)


testDotFun<- function(text,...){
  additional <- list(...)
  for (i in additional){
    print(i)
  }
  
}