library("ggplot2")
# read data
dftrain=read.csv("C://Users//12783//Downloads//Simulated_Data_Train.csv",
            colClasses=c(rep("numeric",5),rep("factor",3),"numeric",rep("factor",4),rep("numeric",4),"factor","numeric",rep("factor",2)))

dfvali=read.csv("C://Users//12783//Downloads//Simulated_Data_Validation.csv",
            colClasses=c(rep("numeric",5),rep("factor",3),"numeric",rep("factor",4),rep("numeric",4),"factor","numeric",rep("factor",2)))

dftest=read.csv("C://Users//12783//Downloads//Simulated_Data_Test.csv",
                colClasses=c(rep("numeric",5),rep("factor",3),"numeric",rep("factor",4),rep("numeric",4),"factor","numeric",rep("factor",2)))

library("naniar")
dftrain$non_mtg_acc_past_due_12_months_num=factor(dftrain$non_mtg_acc_past_due_12_months_num,ordered = TRUE)
dftrain$non_mtg_acc_past_due_6_months_num=factor(dftrain$non_mtg_acc_past_due_6_months_num,ordered = TRUE)
dftrain$mortgages_past_due_6_months_num=factor(dftrain$mortgages_past_due_6_months_num,ordered = TRUE)
dftrain$card_open_36_month_num=factor(dftrain$card_open_36_month_num,ordered = TRUE)
dftrain$auto_open_36_month_num=factor(dftrain$auto_open_36_month_num,ordered = TRUE)

dfvali$non_mtg_acc_past_due_12_months_num=factor(dfvali$non_mtg_acc_past_due_12_months_num,ordered = TRUE)
dfvali$non_mtg_acc_past_due_6_months_num=factor(dfvali$non_mtg_acc_past_due_6_months_num,ordered = TRUE)
dfvali$mortgages_past_due_6_months_num=factor(dfvali$mortgages_past_due_6_months_num,ordered = TRUE)
dfvali$card_open_36_month_num=factor(dfvali$card_open_36_month_num,ordered = TRUE)
dfvali$auto_open_36_month_num=factor(dfvali$auto_open_36_month_num,ordered = TRUE)

dftest$non_mtg_acc_past_due_12_months_num=factor(dftest$non_mtg_acc_past_due_12_months_num,ordered = TRUE)
dftest$non_mtg_acc_past_due_6_months_num=factor(dftest$non_mtg_acc_past_due_6_months_num,ordered = TRUE)
dftest$mortgages_past_due_6_months_num=factor(dftest$mortgages_past_due_6_months_num,ordered = TRUE)
dftest$card_open_36_month_num=factor(dftest$card_open_36_month_num,ordered = TRUE)
dftest$auto_open_36_month_num=factor(dftest$auto_open_36_month_num,ordered = TRUE)

# visualization scatter plot
ggplot(dftrain,
       aes(x = uti_card_50plus_pct,
           y = rep_income)) +
  geom_miss_point() +
  facet_wrap(~Default_ind)

library(mice)
# Imputing using cart
imputed_DataTrain <- mice(dftrain, m=5, maxit = 50, method = 'cart', seed = 500)
summary(imputed_Data)
completeDataTrain <- complete(imputed_DataTrain,5)
write.csv(completeDataTrain,"C:\\Users\\12783\\Downloads\\Cart_impute.csv")

imputed_DataVali <- mice(dfvali, m=5, maxit = 50, method = 'cart', seed = 500)
summary(imputed_Data)
completeDataVali <- complete(imputed_DataVali,5)
write.csv(completeDataVali,"C:\\Users\\12783\\Downloads\\Cart_impute_Vali.csv")

imputed_DataTest <- mice(dftest, m=5, maxit = 50, method = 'cart', seed = 500)
summary(imputed_Data)
completeDataTest <- complete(imputed_DataTest,5)
write.csv(completeDataTest,"C:\\Users\\12783\\Downloads\\Cart_impute_Test.csv")

# Imputing using rf

imputed_DataTrain <- mice(dftrain, m=5, maxit = 50, method = 'rf', seed = 500)
summary(imputed_Data)
completeDataTrain <- complete(imputed_DataTrain,5)
write.csv(completeDataTrain,"C:\\Users\\12783\\Downloads\\rf.csv")


imputed_DataVali <- mice(dfvali, m=5, maxit = 50, method = 'rf', seed = 500)
summary(imputed_Data)
completeDataVali <- complete(imputed_DataVali,5)
write.csv(completeDataVali,"C:\\Users\\12783\\Downloads\\rf_Vali.csv")

imputed_DataTest <- mice(dftest, m=5, maxit = 50, method = 'rf', seed = 500)
summary(imputed_Data)
completeDataTest <- complete(imputed_DataTest,5)
write.csv(completeDataTest,"C:\\Users\\12783\\Downloads\\rf_Test.csv")



library(tidyr)

# EMB imputing
library(Amelia)
amelia_fitTrain <- amelia(dftrain, m=5, parallel = "multicore",
                          idvars=c("non_mtg_acc_past_due_12_months_num", 
                                   "non_mtg_acc_past_due_6_months_num",
                                   "mortgages_past_due_6_months_num", 
                                   "inq_12_month_num", 
                                   "card_inq_24_month_num", 
                                   "card_open_36_month_num", 
                                   "auto_open_36_month_num",
                                   "ind_acc_XYZ", "States", "Default_ind"))

write.csv(amelia_fitTrain$imputations[[5]],"C:\\Users\\12783\\Downloads\\EMB_impute.csv")

amelia_fitVali <- amelia(dfvali, m=5, parallel = "multicore",
                          idvars=c("non_mtg_acc_past_due_12_months_num", 
                                   "non_mtg_acc_past_due_6_months_num",
                                   "mortgages_past_due_6_months_num", 
                                   "inq_12_month_num", 
                                   "card_inq_24_month_num", 
                                   "card_open_36_month_num", 
                                   "auto_open_36_month_num",
                                   "ind_acc_XYZ", "States", "Default_ind"))

write.csv(amelia_fitVali$imputations[[5]],"C:\\Users\\12783\\Downloads\\EMB_impute_Vali.csv")

amelia_fittest <- amelia(dftest, m=5, parallel = "multicore",
                         idvars=c("non_mtg_acc_past_due_12_months_num", 
                                  "non_mtg_acc_past_due_6_months_num",
                                  "mortgages_past_due_6_months_num", 
                                  "inq_12_month_num", 
                                  "card_inq_24_month_num", 
                                  "card_open_36_month_num", 
                                  "auto_open_36_month_num",
                                  "ind_acc_XYZ", "States", "Default_ind"))

write.csv(amelia_fittest$imputations[[5]],"C:\\Users\\12783\\Downloads\\EMB_impute_Test.csv")



## boostrap in section 4
bsprob=read.csv("C://Users//12783//Downloads//probility.csv")

#95% CI of 59 quantile in this question
x1rej=bsprob$X1[which(bsprob$X1>0.5)]
resize=length(x1rej)
data59quantile=quantile(x1rej,0.59)

bootstrapdiff=rep(0,5000)
for (i in 1:5000){
  new.sample=sample(x1rej, resize, replace = TRUE)
  new59quantile=quantile(new.sample,0.59)
  bootstrapdiff[i]=new59quantile-data59quantile
}
deltau=quantile(sort(bootstrapdiff),0.025)
deltal=quantile(sort(bootstrapdiff),0.975)

# Here shows 95% CI of 59 quantile
c(data59quantile-deltal, data59quantile-deltau)

