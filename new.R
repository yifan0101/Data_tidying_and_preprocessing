library(tidyverse)
library(stringr)
library(ggpubr)
library(readxl)
library(cowplot)
library(scales)
library(car)
data_Total<-read.csv("D:\\文件\\memory andemotion\\rater\\data_Total_new.csv")
data_1<-read.csv("D:\\文件\\memory andemotion\\rater\\data1.csv")
data_2<-read.csv("D:\\文件\\memory andemotion\\rater\\data2.csv")
data_3<-read.csv("D:\\文件\\memory andemotion\\rater\\data3.csv")
################################## all pairs#True_Gist
y_min <- min(c(min(data_Total$eE_True_Gist_Imm,na.rm = TRUE), min(data_Total$eE_True_Gist_Del,na.rm = TRUE),min(data_Total$forget_per_Imm,na.rm = TRUE), min(data_Total$forget_per_Del,na.rm = TRUE),min(data_Total$confound_per_Imm,na.rm = TRUE), min(data_Total$confound_per_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$eE_True_Gist_Imm,na.rm = TRUE), max(data_Total$eE_True_Gist_Del,na.rm = TRUE),max(data_Total$forget_per_Imm,na.rm = TRUE), max(data_Total$forget_per_Del,na.rm = TRUE),max(data_Total$confound_per_Imm,na.rm = TRUE), max(data_Total$confound_per_Del,na.rm = TRUE)))
Gist_total_Imm <- ggpaired(data_Total,x="ReaConds",y="eE_True_Gist_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "eE_True_Gist_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Gist_total_Del <- ggpaired(data_Total,x="ReaConds",y="eE_True_Gist_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "eE_True_Gist_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Gist_total_Imm , Gist_total_Del, ncol=2)

leveneTest(eE_True_Gist_Imm~ReaConds*Group,data=data_Total)
leveneTest(eE_True_Gist_Del~ReaConds*Group,data=data_Total)
fit <- aov(eE_True_Gist_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(eE_True_Gist_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#forget_per
y_min <- min(c(min(data_Total$forget_per_Imm,na.rm = TRUE), min(data_Total$forget_per_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$forget_per_Imm,na.rm = TRUE), max(data_Total$forget_per_Del,na.rm = TRUE)))
forget_per_Imm <- ggpaired(data_Total,x="ReaConds",y="forget_per_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "forget_per_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
forget_per_Del <- ggpaired(data_Total,x="ReaConds",y="forget_per_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "forget_per_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(forget_per_Imm , forget_per_Del, ncol=2)

leveneTest(forget_per_Imm~ReaConds*Group,data=data_Total)
leveneTest(forget_per_Del~ReaConds*Group,data=data_Total)
fit <- aov(forget_per_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(forget_per_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#confound_per
y_min <- min(c(min(data_Total$confound_per_Imm,na.rm = TRUE), min(data_Total$confound_per_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$confound_per_Imm,na.rm = TRUE), max(data_Total$confound_per_Del,na.rm = TRUE)))
confound_per_Imm <- ggpaired(data_Total,x="ReaConds",y="confound_per_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "confound_per_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
confound_per_Del <- ggpaired(data_Total,x="ReaConds",y="confound_per_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "confound_per_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(confound_per_Imm , confound_per_Del, ncol=2)

leveneTest(confound_per_Imm~ReaConds*Group,data=data_Total)
leveneTest(confound_per_Del~ReaConds*Group,data=data_Total)
fit <- aov(confound_per_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(confound_per_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#TrueDetails
y_min <- min(c(min(data_Total$eE_TrueDetail_Imm,na.rm = TRUE), min(data_Total$eE_TrueDetail_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$eE_TrueDetail_Imm,na.rm = TRUE), max(data_Total$eE_TrueDetail_Del,na.rm = TRUE)))
True_total_Imm<- ggpaired(data_Total,x="ReaConds",y="eE_TrueDetail_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_TrueDetail_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
True_total_Del <- ggpaired(data_Total,x="ReaConds",y="eE_TrueDetail_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "eE_TrueDetail_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(True_total_Imm , True_total_Del, ncol=2,align="v")

leveneTest(eE_TrueDetail_Imm~ReaConds*Group,data=data_Total)
leveneTest(eE_TrueDetail_Del~ReaConds*Group,data=data_Total)
fit <- aov(eE_TrueDetail_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(eE_TrueDetail_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#FalseDetails
y_min <- min(c(min(data_Total$eE_FalseDetail_Imm,na.rm = TRUE), min(data_Total$eE_FalseDetail_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$eE_FalseDetail_Imm,na.rm = TRUE), max(data_Total$eE_FalseDetail_Del,na.rm = TRUE)))
False_total_Imm<- ggpaired(data_Total,x="ReaConds",y="eE_FalseDetail_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "eE_FalseDetail_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_total_Del <- ggpaired(data_Total,x="ReaConds",y="eE_FalseDetail_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_FalseDetail_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_total_Imm , False_total_Del, ncol=2,align="v")
##########
leveneTest(eE_FalseDetail_Imm~ReaConds*Group,data=data_Total)
leveneTest(eE_FalseDetail_Del~ReaConds*Group,data=data_Total)
fit <- aov(eE_FalseDetail_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(eE_FalseDetail_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#Arousal
y_min <- min(c(min(data_Total$eE_Arousal_Imm,na.rm = TRUE), min(data_Total$eE_Arousal_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$eE_Arousal_Imm,na.rm = TRUE), max(data_Total$eE_Arousal_Del,na.rm = TRUE)))

Arousal_Total_Del <- ggpaired(data_Total,x="ReaConds",y="eE_Arousal_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                              ylab = "eE_Arousal_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)

Arousal_Total_Imm <- ggpaired(data_Total,x="ReaConds",y="eE_Arousal_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                              ylab = "eE_Arousal_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)

plot_grid(Arousal_Total_Imm, Arousal_Total_Del, ncol=2,align="v")

leveneTest(eE_Arousal_Del~ReaConds*Group,data=data_Total)
leveneTest(eE_Arousal_Imm~ReaConds*Group,data=data_Total)
fit <- aov(eE_Arousal_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(eE_Arousal_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#Valence
y_min <- min(c(min(data_Total$eE_Valence_Imm,na.rm = TRUE), min(data_Total$eE_Valence_Del,na.rm = TRUE)))
y_max <- max(c(max(data_Total$eE_Valence_Imm,na.rm = TRUE), max(data_Total$eE_Valence_Del,na.rm = TRUE)))

Valence_Total_Del <- ggpaired(data_Total,x="ReaConds",y="eE_Valence_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                              ylab = "eE_Valence_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Valence_Total_Imm <- ggpaired(data_Total,x="ReaConds",y="eE_Valence_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                              ylab = "eE_Valence_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Valence_Total_Imm, Valence_Total_Del, ncol=2,align="v")
leveneTest(eE_Valence_Del~ReaConds*Group,data=data_Total)
leveneTest(eE_Valence_Imm~ReaConds*Group,data=data_Total)
fit <- aov(eE_Valence_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(eE_Valence_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#iM_ACC
y_min <- min(c(min(data_Total$iM_ACC_Del,na.rm = TRUE), min(data_Total$iM_ACC_Imm,na.rm = TRUE)))
y_max <- max(c(max(data_Total$iM_ACC_Del,na.rm = TRUE), max(data_Total$iM_ACC_Imm,na.rm = TRUE)))
iM_ACC_Total_Del <- ggpaired(data_Total,x="ReaConds",y="iM_ACC_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_ACC_Total_Imm <- ggpaired(data_Total,x="ReaConds",y="iM_ACC_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_ACC_Total_Imm, iM_ACC_Total_Del, ncol=2,align="v")

leveneTest(iM_ACC_Del~ReaConds*Group,data=data_Total)
leveneTest(iM_ACC_Imm~ReaConds*Group,data=data_Total)
fit <- aov(iM_ACC_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(iM_ACC_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#iM_RT
y_min <- min(c(min(data_Total$iM_Old_RT_DelMinusNew,na.rm = TRUE), min(data_Total$iM_Old_RT_ImmMinusNew,na.rm = TRUE)))
y_max <- max(c(max(data_Total$iM_Old_RT_DelMinusNew,na.rm = TRUE), max(data_Total$iM_Old_RT_ImmMinusNew,na.rm = TRUE)))
iM_RT_Total_Del <- ggpaired(data_Total,x="ReaConds",y="iM_Old_RT_DelMinusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_DelMinusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_RT_Total_Imm <- ggpaired(data_Total,x="ReaConds",y="iM_Old_RT_ImmMinusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_ImmMinusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_RT_Total_Imm, iM_RT_Total_Del, ncol=2,align="v")

leveneTest(iM_Old_RT_DelMinusNew~ReaConds*Group,data=data_Total)
leveneTest(iM_Old_RT_ImmMinusNew~ReaConds*Group,data=data_Total)
fit <- aov(iM_Old_RT_DelMinusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(iM_Old_RT_ImmMinusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
################################## all pairs#iE
y_min <- min(c(min(data_Total$iE_Resp_DelMinusBl,na.rm = TRUE), min(data_Total$iE_Resp_ImmMinusBl,na.rm = TRUE)))
y_max <- max(c(max(data_Total$iE_Resp_DelMinusBl,na.rm = TRUE), max(data_Total$iE_Resp_ImmMinusBl,na.rm = TRUE)))
iE_Total_Del <- ggpaired(data_Total,x="ReaConds",y="iE_Resp_ImmMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_ImmMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iE_Total_Imm <- ggpaired(data_Total,x="ReaConds",y="iE_Resp_DelMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_DelMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iE_Total_Imm, iE_Total_Del, ncol=2,align="v")

leveneTest(iE_Resp_ImmMinusBl~ReaConds*Group,data=data_Total)
leveneTest(iE_Resp_DelMinusBl~ReaConds*Group,data=data_Total)
fit <- aov(iE_Resp_DelMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)
fit <- aov(iE_Resp_ImmMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_Total)
summary(fit)

################################## type1#TrueDetails
y_min <- min(c(min(data_1$eE_TrueDetail_Imm,na.rm = TRUE), min(data_1$eE_TrueDetail_Del,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_TrueDetail_Imm,na.rm = TRUE), max(data_1$eE_TrueDetail_Del,na.rm = TRUE)))
True_1_Del <- ggpaired(data_1,x="ReaConds",y="eE_TrueDetail_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_TrueDetail_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
True_1_Imm <- ggpaired(data_1,x="ReaConds",y="eE_TrueDetail_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_TrueDetail_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(True_1_Imm , True_1_Del, ncol=2,align="v")

leveneTest(eE_TrueDetail_Del~ReaConds*Group,data=data_1)
leveneTest(eE_TrueDetail_Imm~ReaConds*Group,data=data_1)
fit <- aov(eE_TrueDetail_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_TrueDetail_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#FalseDetails
y_min <- min(c(min(data_1$eE_FalseDetail_Imm,na.rm = TRUE), min(data_1$eE_FalseDetail_Del,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_FalseDetail_Imm,na.rm = TRUE), max(data_1$eE_FalseDetail_Del,na.rm = TRUE)))
False_1_Imm<- ggpaired(data_1,x="ReaConds",y="eE_FalseDetail_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                           ylab = "eE_FalseDetail_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_1_Del <- ggpaired(data_1,x="ReaConds",y="eE_FalseDetail_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_FalseDetail_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_1_Imm , False_1_Del, ncol=2,align="v")
##########
leveneTest(eE_FalseDetail_Imm~ReaConds*Group,data=data_1)
leveneTest(eE_FalseDetail_Del~ReaConds*Group,data=data_1)
fit <- aov(eE_FalseDetail_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_FalseDetail_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#TrueDetails/total_Num
y_min <- min(c(min(data_1$eE_TrueDetail_perN_Imm,na.rm = TRUE), min(data_1$eE_TrueDetail__perN_Del,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_TrueDetail_perN_Imm,na.rm = TRUE), max(data_1$eE_TrueDetail__perN_Del,na.rm = TRUE)))
True_1_Del_perN <- ggpaired(data_1,x="ReaConds",y="eE_TrueDetail__perN_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_TrueDetail_perN__Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
True_1_Imm_perN <- ggpaired(data_1,x="ReaConds",y="eE_TrueDetail_perN_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_TrueDetail_perN_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(True_1_Imm_perN , True_1_Del_perN, ncol=2,align="v")

leveneTest(eE_TrueDetail__perN_Del~ReaConds*Group,data=data_1)
leveneTest(eE_TrueDetail_perN_Imm~ReaConds*Group,data=data_1)
fit <- aov(eE_TrueDetail__perN_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_TrueDetail_perN_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#FalseDetails/total_Num
y_min <- min(c(min(data_1$eE_FalseDetail_perN_Imm,na.rm = TRUE), min(data_1$eE_FalseDetail__perN_Del,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_FalseDetail_perN_Imm,na.rm = TRUE), max(data_1$eE_FalseDetail__perN_Del,na.rm = TRUE)))
False_1_Del_perN <- ggpaired(data_1,x="ReaConds",y="eE_FalseDetail__perN_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_FalseDetail_perN__Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_1_Imm_perN <- ggpaired(data_1,x="ReaConds",y="eE_FalseDetail_perN_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_FalseDetail_perN_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_1_Imm_perN ,False_1_Del_perN, ncol=2,align="v")

leveneTest(eE_FalseDetail__perN_Del~ReaConds*Group,data=data_1)
leveneTest(eE_FalseDetail_perN_Imm~ReaConds*Group,data=data_1)
fit <- aov(eE_FalseDetail__perN_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_FalseDetail_perN_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#Arousal
y_min <- min(c(min(data_1$eE_Arousal_Imm,na.rm = TRUE), min(data_1$eE_Arousal_Del,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_Arousal_Imm,na.rm = TRUE), max(data_1$eE_Arousal_Del,na.rm = TRUE)))

Arousal_1_Del <- ggpaired(data_1,x="ReaConds",y="eE_Arousal_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Arousal_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Arousal_1_Imm <- ggpaired(data_1,x="ReaConds",y="eE_Arousal_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Arousal_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Arousal_1_Imm, Arousal_1_Del, ncol=2,align="v")

leveneTest(eE_Arousal_Del~ReaConds*Group,data=data_1)
leveneTest(eE_Arousal_Imm~ReaConds*Group,data=data_1)
fit <- aov(eE_Arousal_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_Arousal_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#Valence
y_min <- min(c(min(data_1$eE_Valence_Imm,na.rm = TRUE), min(data_1$eE_Valence_Del,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_Valence_Imm,na.rm = TRUE), max(data_1$eE_Valence_Del,na.rm = TRUE)))
Valence_1_Del <- ggpaired(data_1,x="ReaConds",y="eE_Valence_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Valence_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Valence_1_Imm <- ggpaired(data_1,x="ReaConds",y="eE_Valence_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Valence_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Valence_1_Imm, Valence_1_Del, ncol=2,align="v")

leveneTest(eE_Valence_Del~ReaConds*Group,data=data_1)
leveneTest(eE_Valence_Imm~ReaConds*Group,data=data_1)
fit <- aov(eE_Valence_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_Valence_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#iM_ACC
y_min <- min(c(min(data_1$iM_ACC_Del,na.rm = TRUE), min(data_1$iM_ACC_Imm,na.rm = TRUE)))
y_max <- max(c(max(data_1$iM_ACC_Del,na.rm = TRUE), max(data_1$iM_ACC_Imm,na.rm = TRUE)))
iM_ACC_1_Del <- ggpaired(data_1,x="ReaConds",y="iM_ACC_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_ACC_1_Imm <- ggpaired(data_1,x="ReaConds",y="iM_ACC_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_ACC_1_Imm, iM_ACC_1_Del, ncol=2,align="v")

leveneTest(iM_ACC_Del~ReaConds*Group,data=data_1)
leveneTest(iM_ACC_Imm~ReaConds*Group,data=data_1)
fit <- aov(iM_ACC_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(iM_ACC_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#iM_RT
y_min <- min(c(min(data_1$iM_Old_RT_Del_minusNew,na.rm = TRUE), min(data_1$iM_Old_RT_Imm_minusNew,na.rm = TRUE)))
y_max <- max(c(max(data_1$iM_Old_RT_Del_minusNew,na.rm = TRUE), max(data_1$iM_Old_RT_Imm_minusNew,na.rm = TRUE)))
iM_RT_1_Del <- ggpaired(data_1,x="ReaConds",y="iM_Old_RT_Del_minusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_Del_minusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_RT_1_Imm <- ggpaired(data_1,x="ReaConds",y="iM_Old_RT_Imm_minusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_Imm_minusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_RT_1_Imm, iM_RT_1_Del, ncol=2,align="v")

leveneTest(iM_Old_RT_Del_minusNew~ReaConds*Group,data=data_1)
leveneTest(iM_Old_RT_Imm_minusNew~ReaConds*Group,data=data_1)
fit <- aov(iM_Old_RT_Del_minusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(iM_Old_RT_Imm_minusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################## type1#iE
y_min <- min(c(min(data_1$iE_Resp_DelMinusBl,na.rm = TRUE), min(data_1$iE_Resp_ImmMinusBl,na.rm = TRUE)))
y_max <- max(c(max(data_1$iE_Resp_DelMinusBl,na.rm = TRUE), max(data_1$iE_Resp_ImmMinusBl,na.rm = TRUE)))
iE_1_Del <- ggpaired(data_1,x="ReaConds",y="iE_Resp_ImmMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_ImmMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iE_1_Imm <- ggpaired(data_1,x="ReaConds",y="iE_Resp_DelMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_DelMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iE_1_Imm, iE_1_Del, ncol=2,align="v")

leveneTest(iE_Resp_ImmMinusBl~ReaConds*Group,data=data_1)
leveneTest(iE_Resp_DelMinusBl~ReaConds*Group,data=data_1)
fit <- aov(iE_Resp_DelMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(iE_Resp_ImmMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)

################################## type2#Arousal
y_min <- min(c(min(data_2$eE_Arousal_Imm,na.rm = TRUE), min(data_2$eE_Arousal_Del,na.rm = TRUE)))
y_max <- max(c(max(data_2$eE_Arousal_Imm,na.rm = TRUE), max(data_2$eE_Arousal_Del,na.rm = TRUE)))

Arousal_2_Del <- ggpaired(data_2,x="ReaConds",y="eE_Arousal_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Arousal_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Arousal_2_Imm <- ggpaired(data_2,x="ReaConds",y="eE_Arousal_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Arousal_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Arousal_2_Imm, Arousal_2_Del, ncol=2,align="v")

leveneTest(eE_Arousal_Del~ReaConds*Group,data=data_2)
leveneTest(eE_Arousal_Imm~ReaConds*Group,data=data_2)
fit <- aov(eE_Arousal_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
fit <- aov(eE_Arousal_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
################################## type2#Valence
y_min <- min(c(min(data_2$eE_Valence_Imm,na.rm = TRUE), min(data_1$eE_Valence_Del,na.rm = TRUE)))
y_max <- max(c(max(data_2$eE_Valence_Imm,na.rm = TRUE), max(data_1$eE_Valence_Del,na.rm = TRUE)))
Valence_2_Del <- ggpaired(data_2,x="ReaConds",y="eE_Valence_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Valence_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Valence_2_Imm <- ggpaired(data_2,x="ReaConds",y="eE_Valence_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Valence_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Valence_2_Imm, Valence_2_Del, ncol=2,align="v")

leveneTest(eE_Valence_Del~ReaConds*Group,data=data_2)
leveneTest(eE_Valence_Imm~ReaConds*Group,data=data_2)
fit <- aov(eE_Valence_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
fit <- aov(eE_Valence_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
################################## type2#iM_ACC
y_min <- min(c(min(data_2$iM_ACC_Imm,na.rm = TRUE), min(data_2$iM_ACC_Del,na.rm = TRUE)))
y_max <- max(c(max(data_2$iM_ACC_Imm,na.rm = TRUE), max(data_2$iM_ACC_Del,na.rm = TRUE)))
iM_ACC_2_Del <- ggpaired(data_2,x="ReaConds",y="iM_ACC_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_ACC_2_Imm <- ggpaired(data_2,x="ReaConds",y="iM_ACC_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_ACC_2_Imm, iM_ACC_2_Del, ncol=2,align="v")

leveneTest(iM_ACC_Del~ReaConds*Group,data=data_2)
leveneTest(iM_ACC_Imm~ReaConds*Group,data=data_2)
fit <- aov(iM_ACC_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
fit <- aov(iM_ACC_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
################################## type2#iM_RT
y_min <- min(c(min(data_2$iM_Old_RT_Del_minusNew,na.rm = TRUE), min(data_2$iM_Old_RT_Imm_minusNew,na.rm = TRUE)))
y_max <- max(c(max(data_2$iM_Old_RT_Del_minusNew,na.rm = TRUE), max(data_2$iM_Old_RT_Imm_minusNew,na.rm = TRUE)))
iM_RT_2_Del <- ggpaired(data_2,x="ReaConds",y="iM_Old_RT_Del_minusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_Del_minusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_RT_2_Imm <- ggpaired(data_2,x="ReaConds",y="iM_Old_RT_Imm_minusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_Imm_minusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_RT_2_Imm, iM_RT_2_Del, ncol=2,align="v")

leveneTest(iM_Old_RT_Del_minusNew~ReaConds*Group,data=data_2)
leveneTest(iM_Old_RT_Imm_minusNew~ReaConds*Group,data=data_2)
fit <- aov(iM_Old_RT_Del_minusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
fit <- aov(iM_Old_RT_Imm_minusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
################################## type2#iE
y_min <- min(c(min(data_2$iE_Resp_ImmMinusBl,na.rm = TRUE), min(data_2$iE_Resp_DelMinusBl,na.rm = TRUE)))
y_max <- max(c(max(data_2$iE_Resp_ImmMinusBl,na.rm = TRUE), max(data_2$iE_Resp_DelMinusBl,na.rm = TRUE)))
iE_2_Del <- ggpaired(data_2,x="ReaConds",y="iE_Resp_ImmMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_ImmMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iE_2_Imm <- ggpaired(data_2,x="ReaConds",y="iE_Resp_DelMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_DelMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iE_2_Imm, iE_2_Del, ncol=2,align="v")

leveneTest(iE_Resp_ImmMinusBl~ReaConds*Group,data=data_2)
leveneTest(iE_Resp_DelMinusBl~ReaConds*Group,data=data_2)
fit <- aov(iE_Resp_DelMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
fit <- aov(iE_Resp_ImmMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_2)
summary(fit)
################################## type3#TrueDetails
y_min <- min(c(min(data_3$eE_TrueDetail_Imm,na.rm = TRUE), min(data_3$eE_TrueDetail_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_TrueDetail_Imm,na.rm = TRUE), max(data_3$eE_TrueDetail_Del,na.rm = TRUE)))
True_3_Del <- ggpaired(data_3,x="ReaConds",y="eE_TrueDetail_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_TrueDetail_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
True_3_Imm <- ggpaired(data_3,x="ReaConds",y="eE_TrueDetail_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_TrueDetail_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(True_3_Imm , True_3_Del, ncol=2,align="v")

leveneTest(eE_TrueDetail_Del~ReaConds*Group,data=data_3)
leveneTest(eE_TrueDetail_Imm~ReaConds*Group,data=data_3)
fit <- aov(eE_TrueDetail_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_TrueDetail_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#FalseDetails
y_min <- min(c(min(data_3$eE_FalseDetail_Imm,na.rm = TRUE), min(data_3$eE_FalseDetail_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_FalseDetail_Imm,na.rm = TRUE), max(data_3$eE_FalseDetail_Del,na.rm = TRUE)))
False_3_Imm<- ggpaired(data_3,x="ReaConds",y="eE_FalseDetail_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                       ylab = "eE_FalseDetail_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_3_Del <- ggpaired(data_3,x="ReaConds",y="eE_FalseDetail_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "eE_FalseDetail_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_3_Imm , False_3_Del, ncol=2,align="v")
##########
leveneTest(eE_FalseDetail_Imm~ReaConds*Group,data=data_3)
leveneTest(eE_FalseDetail_Del~ReaConds*Group,data=data_3)
fit <- aov(eE_FalseDetail_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_FalseDetail_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#TrueDetails/total_Num
y_min <- min(c(min(data_3$eE_TrueDetail_perN_Imm,na.rm = TRUE), min(data_3$eE_TrueDetail__perN_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_TrueDetail_perN_Imm,na.rm = TRUE), max(data_3$eE_TrueDetail__perN_Del,na.rm = TRUE)))
True_3_Del_perN <- ggpaired(data_3,x="ReaConds",y="eE_TrueDetail__perN_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_TrueDetail_perN__Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
True_3_Imm_perN <- ggpaired(data_3,x="ReaConds",y="eE_TrueDetail_perN_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_TrueDetail_perN_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(True_3_Imm_perN , True_3_Del_perN, ncol=2,align="v")

leveneTest(eE_TrueDetail__perN_Del~ReaConds*Group,data=data_3)
leveneTest(eE_TrueDetail_perN_Imm~ReaConds*Group,data=data_3)
fit <- aov(eE_TrueDetail__perN_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_TrueDetail_perN_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#FalseDetails/total_Num
y_min <- min(c(min(data_3$eE_FalseDetail_perN_Imm,na.rm = TRUE), min(data_3$eE_FalseDetail__perN_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_FalseDetail_perN_Imm,na.rm = TRUE), max(data_3$eE_FalseDetail__perN_Del,na.rm = TRUE)))
False_3_Del_perN <- ggpaired(data_3,x="ReaConds",y="eE_FalseDetail__perN_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                             ylab = "eE_FalseDetail_perN__Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_3_Imm_perN <- ggpaired(data_3,x="ReaConds",y="eE_FalseDetail_perN_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                             ylab = "eE_FalseDetail_perN_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_3_Imm_perN ,False_3_Del_perN, ncol=2,align="v")

leveneTest(eE_FalseDetail__perN_Del~ReaConds*Group,data=data_3)
leveneTest(eE_FalseDetail_perN_Imm~ReaConds*Group,data=data_3)
fit <- aov(eE_FalseDetail__perN_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_FalseDetail_perN_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#Arousal
y_min <- min(c(min(data_3$eE_Arousal_Imm,na.rm = TRUE), min(data_3$eE_Arousal_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_Arousal_Imm,na.rm = TRUE), max(data_3$eE_Arousal_Del,na.rm = TRUE)))

Arousal_3_Del <- ggpaired(data_3,x="ReaConds",y="eE_Arousal_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Arousal_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Arousal_3_Imm <- ggpaired(data_3,x="ReaConds",y="eE_Arousal_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Arousal_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Arousal_3_Imm, Arousal_3_Del, ncol=2,align="v")

leveneTest(eE_Arousal_Del~ReaConds*Group,data=data_3)
leveneTest(eE_Arousal_Imm~ReaConds*Group,data=data_3)
fit <- aov(eE_Arousal_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_Arousal_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#Valence
y_min <- min(c(min(data_3$eE_Valence_Imm,na.rm = TRUE), min(data_3$eE_Valence_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_Valence_Imm,na.rm = TRUE), max(data_3$eE_Valence_Del,na.rm = TRUE)))
Valence_3_Del <- ggpaired(data_3,x="ReaConds",y="eE_Valence_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Valence_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
Valence_3_Imm <- ggpaired(data_3,x="ReaConds",y="eE_Valence_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                          ylab = "eE_Valence_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(Valence_3_Imm, Valence_3_Del, ncol=2,align="v")

leveneTest(eE_Valence_Del~ReaConds*Group,data=data_3)
leveneTest(eE_Valence_Imm~ReaConds*Group,data=data_3)
fit <- aov(eE_Valence_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_Valence_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#iM_ACC
y_min <- min(c(min(data_3$iM_ACC_Del,na.rm = TRUE), min(data_3$iM_ACC_Del,na.rm = TRUE)))
y_max <- max(c(max(data_3$iM_ACC_Del,na.rm = TRUE), max(data_3$iM_ACC_Del,na.rm = TRUE)))
iM_ACC_3_Del <- ggpaired(data_3,x="ReaConds",y="iM_ACC_Del",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Del",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_ACC_3_Imm <- ggpaired(data_3,x="ReaConds",y="iM_ACC_Imm",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                         ylab = "iM_ACC_Imm",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_ACC_3_Imm, iM_ACC_3_Del, ncol=2,align="v")

leveneTest(iM_ACC_Del~ReaConds*Group,data=data_3)
leveneTest(iM_ACC_Imm~ReaConds*Group,data=data_3)
fit <- aov(iM_ACC_Del~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(iM_ACC_Imm~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#iM_RT
y_min <- min(c(min(data_3$iM_Old_RT_Del_minusNew,na.rm = TRUE), min(data_3$iM_Old_RT_Imm_minusNew,na.rm = TRUE)))
y_max <- max(c(max(data_3$iM_Old_RT_Del_minusNew,na.rm = TRUE), max(data_3$iM_Old_RT_Imm_minusNew,na.rm = TRUE)))
iM_RT_3_Del <- ggpaired(data_3,x="ReaConds",y="iM_Old_RT_Del_minusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_Del_minusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iM_RT_3_Imm <- ggpaired(data_3,x="ReaConds",y="iM_Old_RT_Imm_minusNew",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                        ylab = "iM_Old_RT_Imm_minusNew",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iM_RT_3_Imm, iM_RT_3_Del, ncol=2,align="v")

leveneTest(iM_Old_RT_Del_minusNew~ReaConds*Group,data=data_3)
leveneTest(iM_Old_RT_Imm_minusNew~ReaConds*Group,data=data_3)
fit <- aov(iM_Old_RT_Del_minusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(iM_Old_RT_Imm_minusNew~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## type3#iE
y_min <- min(c(min(data_3$iE_Resp_ImmMinusBl,na.rm = TRUE), min(data_3$iE_Resp_DelMinusBl,na.rm = TRUE)))
y_max <- max(c(max(data_3$iE_Resp_ImmMinusBl,na.rm = TRUE), max(data_3$iE_Resp_DelMinusBl,na.rm = TRUE)))
iE_3_Del <- ggpaired(data_3,x="ReaConds",y="iE_Resp_ImmMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_ImmMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
iE_3_Imm <- ggpaired(data_3,x="ReaConds",y="iE_Resp_DelMinusBl",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                     ylab = "iE_Resp_DelMinusBl",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(iE_3_Imm, iE_3_Del, ncol=2,align="v")

leveneTest(iE_Resp_ImmMinusBl~ReaConds*Group,data=data_3)
leveneTest(iE_Resp_DelMinusBl~ReaConds*Group,data=data_3)
fit <- aov(iE_Resp_DelMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(iE_Resp_ImmMinusBl~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################## eE_True_Detail/num
data_1 <- mutate(data_1,eE_True_Detail_Imm_per=eE_TrueDetail_Imm*True_per_Imm,
                 eE_True_Detail_Del_per=eE_TrueDetail_Imm*True_per_Del,
                 eE_False_Detail_Imm_per=eE_FalseDetail_Imm*True_per_Imm,
                 eE_False_Detail_Del_per=eE_FalseDetail_Del*True_per_Del)
data_3 <- mutate(data_3,eE_True_Detail_Imm_per=eE_TrueDetail_Imm*confound_per_Imm,
                 eE_True_Detail_Del_per=eE_TrueDetail_Imm*confound_per_Del,
                 eE_False_Detail_Imm_per=eE_FalseDetail_Imm*confound_per_Imm,
                 eE_False_Detail_Del_per=eE_FalseDetail_Del*confound_per_Del)
################################# type1 TrueDetails sum?
y_min <- min(c(min(data_1$eE_True_Detail_Imm_per,na.rm = TRUE), min(data_1$eE_True_Detail_Del_per,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_True_Detail_Imm_per,na.rm = TRUE), max(data_1$eE_True_Detail_Del_per,na.rm = TRUE)))
True_1_Del_perN <- ggpaired(data_1,x="ReaConds",y="eE_True_Detail_Del_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_True_Detail_Del_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
True_1_Imm_perN <- ggpaired(data_1,x="ReaConds",y="eE_True_Detail_Imm_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_True_Detail_Imm_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(True_1_Imm_perN , True_1_Del_perN, ncol=2,align="v")

leveneTest(eE_True_Detail_Del_per~ReaConds*Group,data=data_1)
leveneTest(eE_True_Detail_Imm_per~ReaConds*Group,data=data_1)
fit <- aov(eE_True_Detail_Del_per~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_True_Detail_Imm_per~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################# type3 TrueDetails sum?
data_3 <- na.omit(data_3)
y_min <- min(c(min(data_3$eE_True_Detail_Imm_per,na.rm = TRUE), min(data_3$eE_True_Detail_Del_per,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_True_Detail_Imm_per,na.rm = TRUE), max(data_3$eE_True_Detail_Del_per,na.rm = TRUE)))
True_3_Del_perN <- ggpaired(data_3,x="ReaConds",y="eE_True_Detail_Del_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_True_Detail_Del_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(method = "t.test",label='p.format',paired=TRUE)
True_3_Imm_perN <- ggpaired(data_3,x="ReaConds",y="eE_True_Detail_Imm_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_True_Detail_Imm_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(method = "t.test",label='p.format',paired=TRUE)
plot_grid(True_3_Imm_perN , True_3_Del_perN, ncol=2,align="v")

leveneTest(eE_True_Detail_Del_per~ReaConds*Group,data=data_3)
leveneTest(eE_True_Detail_Imm_per~ReaConds*Group,data=data_3)
fit <- aov(eE_True_Detail_Del_per~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_True_Detail_Imm_per~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
################################# type1 FalseDetails sum?
y_min <- min(c(min(data_1$eE_False_Detail_Imm_per,na.rm = TRUE), min(data_1$eE_False_Detail_Del_per,na.rm = TRUE)))
y_max <- max(c(max(data_1$eE_False_Detail_Imm_per,na.rm = TRUE), max(data_1$eE_False_Detail_Del_per,na.rm = TRUE)))
False_1_Del_perN <- ggpaired(data_1,x="ReaConds",y="eE_False_Detail_Del_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_False_Detail_Del_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_1_Imm_perN <- ggpaired(data_1,x="ReaConds",y="eE_False_Detail_Imm_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                            ylab = "eE_False_Detail_Imm_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_1_Imm_perN , False_1_Del_perN, ncol=2,align="v")

leveneTest(eE_False_Detail_Del_per~ReaConds*Group,data=data_1)
leveneTest(eE_False_Detail_Imm_per~ReaConds*Group,data=data_1)
fit <- aov(eE_False_Detail_Del_per~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
fit <- aov(eE_False_Detail_Imm_per~Group*ReaConds+Error(SubId/ReaConds),data=data_1)
summary(fit)
################################# type3 FalseDetails sum?
y_min <- min(c(min(data_3$eE_False_Detail_Imm_per,na.rm = TRUE), min(data_3$eE_False_Detail_Del_per,na.rm = TRUE)))
y_max <- max(c(max(data_3$eE_False_Detail_Imm_per,na.rm = TRUE), max(data_3$eE_False_Detail_Del_per,na.rm = TRUE)))
False_3_Del_perN <- ggpaired(data_3,x="ReaConds",y="eE_False_Detail_Del_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                             ylab = "eE_False_Detail_Del_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
False_3_Imm_perN <- ggpaired(data_3,x="ReaConds",y="eE_False_Detail_Imm_per",id="SubId",color="ReaConds",palette="jco",line.color="gray",line.size=0.4,  xlab = "",
                             ylab = "eE_False_Detail_Imm_per",facet.by="Group")+  ylim(c(y_min, y_max))+theme_classic()+stat_compare_means(label='p.format',paired=TRUE)
plot_grid(False_3_Imm_perN , False_3_Del_perN, ncol=2,align="v")

leveneTest(eE_False_Detail_Del_per~ReaConds*Group,data=data_3)
leveneTest(eE_False_Detail_Imm_per~ReaConds*Group,data=data_3)
fit <- aov(eE_False_Detail_Del_per~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
fit <- aov(eE_False_Detail_Imm_per~Group*ReaConds+Error(SubId/ReaConds),data=data_3)
summary(fit)
####################################################################################################################################################################
