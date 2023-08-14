library(tidyverse)
library(stringr)

pretidy_pra<-function(x){
  setwd(x)
  dir() %>% str_subset("\\.csv$") -> fn
  data=list()
  for(i in 1:length(fn)){
    data[[i]]=read.csv(fn[i], fileEncoding = 'utf-8')
  }
  names(data)<-c("eE_Del","eE_Imm","iE_bl","iE_Imm","iE_Del","iM_Imm","iM_Del","Encoding","Test","Reactivation")
  subid=data$Reactivation[1,1]
  group=data$Reactivation[1,2]
  
  Encoding<-data$Encoding%>%
    select(c("SubId","Cities","Events"))
  PairTest <- data$Test[1:64,]%>%
    select(c("City","TrueOrFalse"))%>%
    rename(c("Cities"="City"))
  Encoding <- right_join(Encoding,PairTest)
  pairTfalse <- length(Encoding$TrueOrFalse[Encoding$TrueOrFalse=="F"])

  eE_Imm<-data$eE_Imm%>%
    select(c("Cities","Valence","Arousal","True_Gist","True_Details","FALSE.","confound"))%>%
    rename(c("eE_Valence_Imm"="Valence","eE_Arousal_Imm"="Arousal","Gist_Imm"="True_Gist","True_detail_Imm"="True_Details","False_detail_Imm"="FALSE.","confound_Imm"="confound"))
  eE_Del<-data$eE_Del%>%
    select(c("Cities","Valence","Arousal","True_Gist","True_Details","FALSE.","confound"))%>%
    rename(c("eE_Valence_Del"="Valence","eE_Arousal_Del"="Arousal","Gist_Del"="True_Gist","True_detail_Del"="True_Details","False_detail_Del"="FALSE.","confound_Del"="confound"))
  iE_bl<- data$iE_bl%>%
    mutate(Cities=sub('....$','',data$iE_bl$NegSce))%>%
    select(c("Cities","Resp1"))%>%
    rename(c("iE_Resp_Bl"="Resp1"))
  iE_bl$iE_Resp_Bl[is.na(iE_bl$iE_Resp_Bl)] <- 0.50
  
  iE_Imm<-data$iE_Imm%>%
    mutate(Cities=sub('....$','',data$iE_Imm$NegSce))%>%
    select(c("Cities","Resp1"))%>%
    rename(c("iE_Resp_Imm"="Resp1"))
  #缺失赋值为0.5
  iE_Imm$iE_Resp_Imm[is.na(iE_Imm$iE_Resp_Imm)] <- 0.50
  iE_Del<-data$iE_Del%>%
    mutate(Cities=sub('....$','',data$iE_Del$NegSce))%>%
    select(c("Cities","Resp1"))%>%
    rename(c("iE_Resp_Del"="Resp1"))
  iE_Del$iE_Resp_Del[is.na(iE_Del$iE_Resp_Del)] <- 0.50
  
  iM_Imm<-data$iM_Imm%>%
    select(c("SubId","NegSce","Conds","RT1","ACC","Resp2"))%>%
    rename(c("Conf_Imm"="Resp2","RT_Imm"="RT1","ACC_Imm"="ACC","Events"="NegSce"))%>%
    mutate(Conds=ifelse(Conds==1, "OLd","New"))
  
  iM_Del<-data$iM_Del%>%
    select(c("SubId","NegSce","Conds","RT1","ACC","Resp2"))%>%
    rename(c("Conf_Del"="Resp2","RT_Del"="RT1","ACC_Del"="ACC","Events"="NegSce"))%>%
    mutate(Conds=ifelse(Conds==1, "OLd","New"))
  
  #Reactivation变宽
  if (exists("Duration_Reminiscing",data$Reactivation)){
    Reactivation<-data$Reactivation%>%
      select(c("Group","Province","Cities","filmNum","Duration_Reminiscing","Vividness","Valence","Arousal","ACC"))%>%
      rename(c("VideoNum"="filmNum", "MemoryVividness"="Vividness", "MemoryValence"="Valence","MemoryArousal"="Arousal")) 
  }else {
    Reactivation<-data$Reactivation%>%
      select(c("Group","Province","Cities","filmNum","Vividness","Valence","Arousal","ACC"))%>%
      rename(c("VideoNum"="filmNum", "MemoryVividness"="Vividness", "MemoryValence"="Valence","MemoryArousal"="Arousal"))%>%
      mutate(Duration_Reminiscing=NA)}
  j<-1
  while (j<=4){
    i<- 10*j-9
    while (i<=10*j){
      Reactivation[i,c("VideoNum","Duration_Reminiscing","MemoryVividness","MemoryValence","MemoryArousal")]=Reactivation[10*j+1,c("VideoNum","Duration_Reminiscing","MemoryVividness","MemoryValence","MemoryArousal")]
      i<-i+1
    }
    Reactivation<-Reactivation[-c(10*j+1),]
    j<-j+1
  }
  ##Rea中答错的
  Reafalse <- length(Reactivation$ACC[Reactivation$ACC==FALSE])
  #合并表格
  datajoin1<-right_join (Reactivation,Encoding,by="Cities")
  datajoin1 <-datajoin1%>%
    mutate(Group=datajoin1[1,1],
           ReaConds = ifelse(is.na(VideoNum), "NonRea","Rea"))
  
  datajoin2<-right_join(eE_Imm,datajoin1,by="Cities")
  datajoin2<-right_join(eE_Del,datajoin2,by="Cities")
  datajoin3<-right_join(iE_bl,datajoin2,by="Cities")
  datajoin3<-right_join(iE_Imm,datajoin3,by="Cities")
  datajoin3<-right_join(iE_Del,datajoin3,by="Cities")
  iM_Imm<-left_join(iM_Imm,datajoin3[c("VideoNum","Events","ReaConds","ACC","TrueOrFalse")],by="Events")
  iM_Del<-left_join(iM_Del,datajoin3[c("VideoNum","Events","ReaConds","ACC","TrueOrFalse")],by="Events")
  iM<-full_join(iM_Imm,iM_Del)
  #删除中间变量
  rm(eE_Imm,eE_Del,iE_bl,iE_Imm,iE_Del,datajoin1,datajoin2,iM_Imm,iM_Del)
  rm(i,j)
  
  #分组求均值
  #######################################
  Reaout <- length(datajoin3$ACC[datajoin3$ACC == FALSE & !is.na(datajoin3$ACC)])
  datajoin3 <- datajoin3[datajoin3$ACC != FALSE | is.na(datajoin3$ACC), ]
  Pairoutmore <- length(datajoin3$TrueOrFalse[datajoin3$TrueOrFalse == "F" ])
  datajoin3 <- datajoin3[datajoin3$TrueOrFalse != "F" | is.na(datajoin3$TrueOrFalse), ]
  ######################################
  data1<-datajoin3 %>% 
    
    group_by(ReaConds,VideoNum) %>% 
    summarise(ReminiscingDuration = mean(Duration_Reminiscing),
              MemoryVividness=mean(MemoryVividness),
              MemoryValence=mean(MemoryValence),
              MemoryArousal=mean(MemoryArousal),
              iE_Resp_Bl=mean(iE_Resp_Bl),
              iE_Resp_Imm=mean(iE_Resp_Imm,na.rm=TRUE),
              iE_Resp_Del=mean(iE_Resp_Del,na.rm=TRUE),
              eE_Valence_Imm=mean(eE_Valence_Imm,na.rm=TRUE),
              eE_Arousal_Imm=mean(eE_Arousal_Imm,na.rm=TRUE),
              eE_Valence_Del=mean(eE_Valence_Del,na.rm=TRUE),
              eE_Arousal_Del=mean(eE_Arousal_Del,na.rm=TRUE),
              eE_TrueDetail_Imm=mean(True_detail_Imm,na.rm=TRUE),
              eE_FalseDetail_Imm=mean(False_detail_Imm,na.rm=TRUE),
              eE_TrueDetail_Del=mean(True_detail_Del,na.rm=TRUE),
              eE_FalseDetail_Del=mean(False_detail_Del,na.rm=TRUE),
              eE_True_Gist_Imm=mean(Gist_Imm,na.rm=TRUE),
              eE_True_Gist_Del=mean(Gist_Del,na.rm=TRUE)
    )
  data1$VideoNum<-as.character(data1$VideoNum)
  data2<-datajoin3 %>%
    group_by(ReaConds) %>% 
    summarise(ReminiscingDuration = mean(Duration_Reminiscing),
              MemoryVividness=mean(MemoryVividness),
              MemoryValence=mean(MemoryValence),
              MemoryArousal=mean(MemoryArousal),
              iE_Resp_Bl=mean(iE_Resp_Bl),
              iE_Resp_Imm=mean(iE_Resp_Imm,na.rm=TRUE),
              iE_Resp_Del=mean(iE_Resp_Del,na.rm=TRUE),
              eE_Valence_Imm=mean(eE_Valence_Imm,na.rm=TRUE),
              eE_Arousal_Imm=mean(eE_Arousal_Imm,na.rm=TRUE),
              eE_Valence_Del=mean(eE_Valence_Del,na.rm=TRUE),
              eE_Arousal_Del=mean(eE_Arousal_Del,na.rm=TRUE),
              eE_TrueDetail_Imm=mean(True_detail_Imm,na.rm=TRUE),
              eE_FalseDetail_Imm=mean(False_detail_Imm,na.rm=TRUE),
              eE_TrueDetail_Del=mean(True_detail_Del,na.rm=TRUE),
              eE_FalseDetail_Del=mean(False_detail_Del,na.rm=TRUE),
              eE_True_Gist_Imm=mean(Gist_Imm,na.rm=TRUE),
              eE_True_Gist_Del=mean(Gist_Del,na.rm=TRUE)
              
    )
  data2<-data2%>%
    mutate(VideoNum=ifelse(ReaConds=="NonRea", NA,"all"))
  Data<-full_join(data1,data2)
  
  rm(data1,data2)
  ##im
  iM.wide <- iM %>% 
    pivot_wider(names_from = Conds, values_from = c("RT_Imm","ACC_Imm","Conf_Imm","RT_Del","ACC_Del","Conf_Del") )
  iM_wide<-left_join(iM.wide,iM[c("ACC_Imm","ACC_Del","Events")],by="Events")
  #############################################################
  iM_wide <- iM_wide[iM_wide$ACC != FALSE | is.na(iM_wide$ACC), ]
  iM_wide <- iM_wide[iM_wide$TrueOrFalse != "F" | is.na(iM_wide$TrueOrFalse), ]
  ############################################################
  data_iM1<-iM_wide %>%
    group_by(ReaConds,VideoNum) %>%
    summarise(iM_Old_RT_Imm=mean(RT_Imm_OLd[ACC_Imm_OLd==1],na.rm=TRUE),
              iM_New_RT_Imm=mean(RT_Imm_New[ACC_Imm_New==1],na.rm=TRUE),
              iM_Old_RT_Del=mean(RT_Del_OLd[ACC_Del_OLd==1],na.rm=TRUE),
              iM_New_RT_Del=mean(RT_Del_New[ACC_Del_New==1],na.rm=TRUE),
              iM_Old_ACC_Imm=mean(ACC_Imm_OLd,na.rm=TRUE),
              iM_New_ACC_Imm=mean(ACC_Imm_New,na.rm=TRUE),
              iM_Old_ACC_Del=mean(ACC_Del_OLd,na.rm=TRUE),
              iM_New_ACC_Del=mean(ACC_Del_New,na.rm=TRUE),
              iM_Old_Conf_Imm=mean(Conf_Imm_OLd[ACC_Imm_OLd==1],na.rm=TRUE),
              iM_New_Conf_Imm=mean(Conf_Imm_New[ACC_Imm_New==1],na.rm=TRUE),
              iM_Old_Conf_Del=mean(Conf_Del_OLd[ACC_Del_OLd==1],na.rm=TRUE),
              iM_New_Conf_Del=mean(Conf_Del_New[ACC_Del_New==1],na.rm=TRUE),
              iM_ACC_Imm=mean(ACC_Imm,na.rm=TRUE),
              iM_ACC_Del=mean(ACC_Del,na.rm=TRUE)
    )
  data_iM1$VideoNum<-as.character(data_iM1$VideoNum)
  
  data_iM2<-iM_wide %>%
    group_by(ReaConds) %>%
    summarise(iM_Old_RT_Imm=mean(RT_Imm_OLd[ACC_Imm_OLd==1],na.rm=TRUE),
              iM_New_RT_Imm=mean(RT_Imm_New[ACC_Imm_New==1],na.rm=TRUE),
              iM_Old_RT_Del=mean(RT_Del_OLd[ACC_Del_OLd==1],na.rm=TRUE),
              iM_New_RT_Del=mean(RT_Del_New[ACC_Del_New==1],na.rm=TRUE),
              iM_Old_ACC_Imm=mean(ACC_Imm_OLd,na.rm=TRUE),
              iM_New_ACC_Imm=mean(ACC_Imm_New,na.rm=TRUE),
              iM_Old_ACC_Del=mean(ACC_Del_OLd,na.rm=TRUE),
              iM_New_ACC_Del=mean(ACC_Del_New,na.rm=TRUE),
              iM_Old_Conf_Imm=mean(Conf_Imm_OLd[ACC_Imm_OLd==1],na.rm=TRUE),
              iM_New_Conf_Imm=mean(Conf_Imm_New[ACC_Imm_New==1],na.rm=TRUE),
              iM_Old_Conf_Del=mean(Conf_Del_OLd[ACC_Del_OLd==1],na.rm=TRUE),
              iM_New_Conf_Del=mean(Conf_Del_New[ACC_Del_New==1],na.rm=TRUE),
              iM_ACC_Imm=mean(ACC_Imm,na.rm=TRUE),
              iM_ACC_Del=mean(ACC_Del,na.rm=TRUE)
    )
  
  data_iM2<-data_iM2%>%
    mutate(VideoNum=ifelse(ReaConds=="Rea","all",NA))
  Data_iM<-full_join(data_iM1,data_iM2)
  n<-2
  while(n<=7) {
    Data_iM[1,2*n]=Data_iM[6,2*n]
    Data_iM[7,2*n]=Data_iM[6,2*n]
    n<-n+1
  }
  Data_iM<-Data_iM[-6,]
  rm(iM,iM.wide,data_iM1,data_iM2)
  Data<-full_join(Data,Data_iM)
  Data<-mutate(Data,SubId=subid,Group=group,Reaout=Reaout,ReaFalse=Reafalse,PairTfalse=pairTfalse,Pairoutmore=Pairoutmore)
  #write.csv(Data,y)
  Data
}
#4,5,6,8,10,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,31,32,33,35,36,37,38,39,40,41,42,43,44,45,46
#4,5,10,14,15,16,18,21,26,28,29,31,35,36,38,39,41,42,43,45,46

oristring <- "D:\\文件\\memory andemotion\\Data\\01"
change_chars <- c("04","05","06","08","10","14","15","16","18","20","21","23","25","26","27","28","29","30","36","31","32","37","38","39","41","42","43")
new_strings <- sapply(change_chars, function(x) {
  # 将原始字符串的后两个字符替换为新字符
  new_string <- sub("..$", x, oristring)
  return(new_string)
})
new_strings
dataPre=list()
for(w in 1:length(change_chars)){
dataPre[[w]] <- pretidy_pra(new_strings[w])

}
dataPreProc <- dataPre[[1]]
for(q in 2:length(change_chars)){dataPreProc <- rbind(dataPreProc,dataPre[[q]])}
dataPreProc <- dataPreProc%>%
  mutate(iE_Resp_ImmMinusBl=iE_Resp_Imm-iE_Resp_Bl,
         iE_Resp_DelMinusBl=iE_Resp_Del-iE_Resp_Bl,
         iM_Old_RT_ImmMinusNew=iM_Old_RT_Imm-iM_New_RT_Imm,
         iM_Old_RT_DelMinusNew=iM_Old_RT_Del-iM_New_RT_Del)
dataPreProc <- arrange(dataPreProc,SubId,ReaConds)
write.csv(dataPreProc,"D:\\文件\\memory andemotion\\rater\\data_Total.csv")
###比较差异
data_CBX <- readxl::read_xlsx("D:\\文件\\memory andemotion\\Data\\DataPreProcCBX.xlsx")
data_MYF <- read.csv("D:\\文件\\memory andemotion\\Data\\dataPreProc_MYF.csv")
all(data_MYF,data_CBX)
data_MYF %in% data_CBX
identical(data_MYF,data_CBX)
diffs <- all.equal(data_MYF, data_CBX,check.attributes = FALSE)
