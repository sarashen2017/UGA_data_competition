##### Radar Plot

Prediction <- read.csv("~/Desktop/UGA/DataScienceCompetition/Data Set (Imputed)/Unconditional_mean_impute_Test.csv")
summary(Prediction$Predict)
mylist <- split(Prediction, Prediction$Predict)
defalut1<-mylist$`1`
defalut0<-mylist$`0`
### Choose randomly 49% was rejected form predict default=1
smp_size <- floor(0.41 * nrow(defalut1))
## set the seed to make your partition reproducible
set.seed(123)
indx <- sample(seq_len(nrow(defalut1)), size = smp_size)
rej <- defalut1[indx, ]
norej <- defalut1[-indx, ]
acc<-rbind(defalut0,norej)

summary(Prediction)
library(fmsb)
par(mfrow=c(2,2))

#money
money<-c('tot_credit_debt','avg_card_debt','rep_income')
m.r<-as.data.frame(rbind(rep(100000,3),rep(1,3),colMeans(rej[,money])))
m.a<-as.data.frame(rbind(rep(100000,3),rep(1,3),colMeans(acc[,money])))
radarchart(m.r,axistype=1,pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,20000,5),cglwd=0.8,)
radarchart(m.a,axistype=1,pcol=rgb(0.3,0.6,0.2,0.9), pfcol=rgb(0.3,0.6,0.2,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,20000,5),cglwd=0.8)

#Credict Age
age<-c('credit_age','credit_good_age','card_age')
age.r<-as.data.frame(rbind(rep(300,3),rep(1,3),colMeans(rej[,age])))
age.a<-as.data.frame(rbind(rep(300,3),rep(1,3),colMeans(acc[,age])))
radarchart(age.r,axistype=1,pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,60,5),cglwd=0.8,)
radarchart(age.a,axistype=1,pcol=rgb(0.3,0.6,0.2,0.9), pfcol=rgb(0.3,0.6,0.2,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,60,5),cglwd=0.8)


# #of inqury and aaccunts
inq<-c('inq_12_month_num','card_inq_24_month_num', 'card_open_36_month_num', 'auto_open_36_month_num' )
inq.r<-as.data.frame(rbind(rep(4,4),rep(0,4),colMeans(rej[,inq])))
inq.a<-as.data.frame(rbind(rep(4,4),rep(0,4),colMeans(acc[,inq])))
radarchart(inq.r,axistype=1,pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,1,4),cglwd=0.8,)
radarchart(inq.a,axistype=1,pcol=rgb(0.2,0.6,0.5,0.9), pfcol=rgb(0.2,0.6,0.5,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,1,4),cglwd=0.8)

#uti
uti<-c( 'uti_card','uti_50plus_pct','uti_max_credit_line','uti_card_50plus_pct')
uti.r<-as.data.frame(rbind(rep(1,4),rep(0,4),colMeans(rej[,uti])))
uti.a<-as.data.frame(rbind(rep(1,4),rep(0,4),colMeans(acc[,uti])))
radarchart(uti.r,axistype=1,pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,20000,5),cglwd=0.8,)
radarchart(uti.a,axistype=1,pcol=rgb(0.3,0.6,0.2,0.9), pfcol=rgb(0.3,0.6,0.2,0.5),
           plwd=4,cglcol="grey", cglty=1, axislabcol="grey", 
           caxislabels=seq(0,20000,5),cglwd=0.8)


            
          
