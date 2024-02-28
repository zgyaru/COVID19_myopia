
library(survival)
getTimeStatus = function(dataFrame){
  time = c()
  status = c()
  for( i in 1:nrow(dataFrame)){
    if(dataFrame[i,'myopia_2'] == 1){
      time = c(time,6)
      status = c(status,1)
    }else if(dataFrame[i,'myopia_3'] == 1){
      time = c(time,12)
      status = c(status,1)
    }else{
      time = c(time,12)
      status = c(status,0)
    }
  }
  return(list(time,status))
}


doMultiCox = function(dataFrame, if_month = T, myopia = T){
  if(if_month){
    if(myopia){
      res_cox = coxph(Surv(time, status) ~ 
                           grade+age+month+gender+habitation+key, 
                         data = dataFrame)
      res = summary(res_cox)
    }else{
      res_cox = coxph(Surv(time, status) ~ 
                           grade+age+month+gender+habitation+key, 
                         data = dataFrame)
      res = summary(res_cox) 
    }
  }else{
    if(myopia){
      res_cox = coxph(Surv(time, status) ~ 
                        grade+age+gender+habitation+key, 
                      data = dataFrame)
      res = summary(res_cox)
    }else{
      res_cox = coxph(Surv(time, status) ~ 
                        grade+age+gender+habitation+key, 
                      data = dataFrame)
      res = summary(res_cox) 
    }
  }
  
  res_df = res$conf.int
  res_df = cbind(res$coefficients, res_df[,3:4])
  res_df = as.data.frame(res_df)
  res_df$Pval = round(res_df$`Pr(>|z|)`,3)
  res_df$Pval[which(res_df$Pval == 0)] = '<.001'
  res_df$HR_CI = paste0(round(res_df$`exp(coef)`,3), 
                                " (", round(res_df$`lower .95`,3), 
                                "-", round(res_df$`upper .95`,3), 
                                ")", sep = " ")
  res_df[,c('HR_CI','Pval','exp(coef)','Pr(>|z|)')]
}


doUniCox = function(dataFrame, if_month = T){
  myopia_uni = data.frame()
  myopia_cox = coxph(Surv(time, status) ~ 
                       grade, 
                     data = dataFrame)
  uni1 = summary(myopia_cox)
  uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                               ' (',round(uni1$conf.int[3],3),
                               '-',round(uni1$conf.int[4],3),')'),
                        uni1$coefficients[5]),
                      row.names = c('HR_CI','Pval2')))
  rownames(uni1) = 'grade'
  myopia_uni = rbind(myopia_uni,uni1)
  myopia_cox = coxph(Surv(time, status) ~ 
                       age, 
                     data = dataFrame)
  uni1 = summary(myopia_cox)
  uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                               ' (',round(uni1$conf.int[3],3),
                               '-',round(uni1$conf.int[4],3),')'),
                        uni1$coefficients[5]),
                      row.names = c('HR_CI','Pval2')))
  rownames(uni1) = 'age'
  myopia_uni = rbind(myopia_uni,uni1)
  
  if(if_month){
    myopia_cox = coxph(Surv(time, status) ~ 
                         month, 
                       data = dataFrame)
    uni1 = summary(myopia_cox)
    uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                                 ' (',round(uni1$conf.int[3],3),
                                 '-',round(uni1$conf.int[4],3),')'),
                          round(uni1$coefficients[5],3)),
                        row.names = c('HR_CI','Pval2')))
    rownames(uni1) = 'month'
    myopia_uni = rbind(myopia_uni,uni1)
  }
  
  
  myopia_cox = coxph(Surv(time, status) ~ 
                       gender, 
                     data = dataFrame)
  uni1 = summary(myopia_cox)
  uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                               ' (',round(uni1$conf.int[3],3),
                               '-',round(uni1$conf.int[4],3),')'),
                        round(uni1$coefficients[5],3)),
                      row.names = c('HR_CI','Pval2')))
  rownames(uni1) = 'gender'
  myopia_uni = rbind(myopia_uni,uni1)
  
  myopia_cox = coxph(Surv(time, status) ~ 
                       habitation, 
                     data = dataFrame)
  uni1 = summary(myopia_cox)
  uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                               ' (',round(uni1$conf.int[3],3),
                               '-',round(uni1$conf.int[4],3),')'),
                        round(uni1$coefficients[5],3)),
                      row.names = c('HR_CI','Pval2')))
  rownames(uni1) = 'habitation'
  myopia_uni = rbind(myopia_uni,uni1)
  
  myopia_cox = coxph(Surv(time, status) ~ 
                       key, 
                     data = dataFrame)
  uni1 = summary(myopia_cox)
  uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                               ' (',round(uni1$conf.int[3],3),
                               '-',round(uni1$conf.int[4],3),')'),
                        round(uni1$coefficients[5],3)),
                      row.names = c('HR_CI','Pval2')))
  rownames(uni1) = 'key'
  myopia_uni = rbind(myopia_uni,uni1)
  #rownames(myopia_uni) = c('Grade','Age','Month','Gender','Habitant','Key')
  myopia_uni$Pval2 = as.character(myopia_uni$Pval2)
  myopia_uni$Pval2[which(myopia_uni$Pval2 == '0')] = '<.001'
  myopia_uni
}

#################################################################################################
#################################   define time and status    ###################################
#################################################################################################
# 2019.06[nonMyopia], 2019.12[nonMyopia], 2020.06[nonMyopia]:             time=12,  status=0
# 2019.06[nonMyopia], 2019.12[nonMyopia], 2020.06[Myopia]:                time=12,  status=1
# 2019.06[nonMyopia], 2019.12[Myopia], 2020.06[Myopia]:                   time=6,   status=1


## all data
comb = read.csv('E:/zz/myopia/1022/2020/results/1014/comb_723216.csv',row.names = 1)
## filtered data 最后没有用filtered data，直接用的comb
#------------------------------------------------------------
test1 = read.csv('E:/zz/myopia/1022/2020/results/1014/figure2/first_filteredData.csv',row.names = 1)
test1[,'myopia_1'] = rep(0,nrow(test1))
test1[which(test1$VA <= 4.9 & test1$OD_SE <= (-0.5)),'myopia_1'] = 1
test1[,'high_1'] = rep(0,nrow(test1))
test1[which(test1$VA <= 4.9 & test1$OD_SE <= (-6)),'high_1'] = 1

test2 = read.csv('E:/zz/myopia/1022/2020/results/1014/figure2/second_filteredData.csv',row.names = 1)
test2[,'myopia_2'] = rep(0,nrow(test2))
test2[which(test2$VA <= 4.9 & test2$OD_SE <= (-0.5)),'myopia_2'] = 1
test2[,'high_2'] = rep(0,nrow(test2))
test2[which(test2$VA <= 4.9 & test2$OD_SE <= (-6)),'high_2'] = 1

test3 = read.csv('E:/zz/myopia/1022/2020/results/1014/figure2/third_filteredData.csv',row.names = 1)
test3[,'myopia_3'] = rep(0,nrow(test3))
test3[which(test3$VA <= 4.9 & test3$OD_SE <= (-0.5)),'myopia_3'] = 1
test3[,'high_3'] = rep(0,nrow(test3))
test3[which(test3$VA <= 4.9 & test3$OD_SE <= (-6)),'high_3'] = 1

inter_id = intersect(rownames(test1), rownames(test2))
inter_id = intersect(inter_id, rownames(test3))
comb2 = cbind(test1[inter_id,],test2[inter_id,c('myopia_2','high_2')], test3[inter_id,c('myopia_3','high_3')])
#----------------------------------------------------------------------

comb = comb[inter_id,]
comb$d1 = comb$SE_2 - comb$SE_1
comb$d2 = comb$SE_3 - comb$SE_2


inter1 = intersect(rownames(comb),rownames(test1))
comb[inter1, 'myopia_1'] = test1[inter1, 'myopia_1']
comb[inter1, 'high_1'] = test1[inter1, 'high_1']

inter2 = intersect(rownames(comb),rownames(test2))
comb[inter2, 'myopia_2'] = test2[inter2, 'myopia_2']
comb[inter2, 'high_2'] = test2[inter2, 'high_2']

inter3 = intersect(rownames(comb),rownames(test3))
comb[inter3, 'myopia_3'] = test3[inter3, 'myopia_3']
comb[inter3, 'high_3'] = test3[inter3, 'high_3']




#comb = comb[which(comb$d1>-5),]
#comb = comb[which(comb$d1<0.5),]
#comb = comb[which(comb$d2>-5),]
#comb = comb[which(comb$d2<0.5),]


######## lets start !
myopia = comb[which(comb$myopia_1 == 0),]
#### time and status
time = rep(0,nrow(myopia))
status = rep(0,nrow(myopia))
## 第二次检测是近视，time=6，status=1
time[which(myopia$myopia_2 == 1)] = 6
status[which(myopia$myopia_2 == 1)] = 1
## 第二次检测不是近视，time肯定都是12
time[which(myopia$myopia_2 == 0)] = 12
## 第三次检测是1的status才是1
status[which(myopia$myopia_3 == 1)] = 1
myopia$time = time
myopia$status = status
#### educational level
level = rep(0,nrow(myopia))
level[which(myopia$grade>6)] = 1


### multi
#myopia$grade = factor(myopia$grade)
myopia$key = as.factor(myopia$key)
myopia$gender = as.factor(myopia$gender)
myopia$region = as.factor(myopia$region)
#myopia$age = as.factor(myopia$age)
myopia_cox = coxph(Surv(time, status) ~ 
                     grade+age+month+gender+region+key, 
                   data = myopia)
res1 = summary(myopia_cox)

######## 2021 0105  去掉month
myopia_cox = coxph(Surv(time, status) ~ 
                     grade+age+gender+habitation+key+month, 
                   data = myopia)
extractAIC(myopia_cox)
res1 = summary(myopia_cox)

phase1 = myopia[which(myopia$grade<7),]
phase2 = myopia[which(myopia$grade>6),]

phase1_cox = coxph(Surv(time, status) ~ 
                     grade+age+month+gender+key+region, 
                   data = phase1)
summary(phase1_cox)

phase2_cox = coxph(Surv(time, status) ~ 
                     grade+age+month+gender+key+region, 
                   data = phase2)
summary(phase2_cox)


myopia_cox_res = res1$conf.int
myopia_cox_res = cbind(res1$coefficients, myopia_cox_res[,3:4])
rownames(myopia_cox_res) = c('Grade','Age','Month','Gender','Habitant','Key')
myopia_cox_res = as.data.frame(myopia_cox_res)
myopia_cox_res$Pval = round(myopia_cox_res$`Pr(>|z|)`,3)
myopia_cox_res$Pval[which(myopia_cox_res$Pval == 0)] = '<.001'
myopia_cox_res$OR_CI = paste0(round(myopia_cox_res$`exp(coef)`,3), 
                                 " (", round(myopia_cox_res$`lower .95`,3), 
                                 "-", round(myopia_cox_res$`upper .95`,3), 
                                 ")", sep = " ")
myopia_cox_res$OR_CI[3] = '0.999 (0.997-1)'
myopia_del_month = myopia_cox_res[,c('OR_CI','Pval')]

write.csv(myopia_cox_res, 'E:/myopia/1022/2020/results/cox/figure2BC/myopia.csv')

## Univariable
myopia_uni = data.frame()
myopia_cox = coxph(Surv(time, status) ~ 
                     grade, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                           ' (',round(uni1$conf.int[3],3),
                           '-',round(uni1$conf.int[4],3),')'),
                    uni1$coefficients[5]),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'grade'
myopia_uni = rbind(myopia_uni,uni1)
myopia_cox = coxph(Surv(time, status) ~ 
                     age, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      uni1$coefficients[5]),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'age'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     month, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'month'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     gender, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'gender'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     region, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'Habitant'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     key, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
               ficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'gender'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     region, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'Habitant'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     key, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
               ficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'gender'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     region, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'Habitant'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     key, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
               ficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'gender'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     region, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','Pval2')))
rownames(uni1) = 'Habitant'
myopia_uni = rbind(myopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     key, 
                   data = myopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                uni1$coefficients[5]),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'grade'
highMyopia_uni = rbind(highMyopia_uni,uni1)
highMyopia_cox = coxph(Surv(time, status) ~ 
                     age, 
                   data = highMyopia)
uni1 = summary(highMyopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      uni1$coefficients[5]),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'age'
highMyopia_uni = rbind(highMyopia_uni,uni1)

highMyopia_cox = coxph(Surv(time, status) ~ 
                     month, 
                   data = highMyopia)
uni1 = summary(highMyopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'month'
highMyopia_uni = rbind(highMyopia_uni,uni1)

highMyopia_cox = coxph(Surv(time, status) ~ 
                     gender, 
                   data = highMyopia)
uni1 = summary(highMyopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'gender'
highMyopia_uni = rbind(highMyopia_uni,uni1)

myopia_cox = coxph(Surv(time, status) ~ 
                     region, 
                   data = highMyopia)
uni1 = summary(myopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'region'
highMyopia_uni = rbind(highMyopia_uni,uni1)

highMyopia_cox = coxph(Surv(time, status) ~ 
                     key, 
                   data = highMyopia)
uni1 = summary(highMyopia_cox)
uni1 = t(data.frame(c(paste0(round(uni1$conf.int[1],3),
                             ' (',round(uni1$conf.int[3],3),
                             '-',round(uni1$conf.int[4],3),')'),
                      round(uni1$coefficients[5],3)),
                    row.names = c('HR_CI','p value')))
rownames(uni1) = 'key'
highMyopia_uni = rbind(highMyopia_uni,uni1)
rownames(highMyopia_uni) = c('Grade','Age','Month','Gender','Habitant','Key')
write.csv(highMyopia_uni, 'E:/myopia/1022/2020/results/cox/figure2BC/highMyopia_uni.csv')





##### 2020 0105
aa = colnames(comb)
aa[which(aa == 'region')] = 'habitation'
colnames(comb) = aa
myopia = comb[which(comb$myopia_1 == 0),]
#### time and status
time = rep(0,nrow(myopia))
status = rep(0,nrow(myopia))
## 第二次检测是近视，time=6，status=1
time[which(myopia$myopia_2 == 1)] = 6
status[which(myopia$myopia_2 == 1)] = 1
## 第二次检测不是近视，time肯定都是12
time[which(myopia$myopia_2 == 0)] = 12
## 第三次检测是1的status才是1
status[which(myopia$myopia_3 == 1)] = 1
myopia$time = time
myopia$status = status
#### educational level
level = rep(0,nrow(myopia))
level[which(myopia$grade>6)] = 1

myopia_group1 = myopia[which(myopia$grade<7),]
myopia_group2 = myopia[which(myopia$grade>6),]



res_month = doMultiCox(myopia, if_month = F, myopia = T)
res_month_uni = doUniCox(myopia,if_month = F)
myopia_month = cbind(res_month, res_month_uni)
write.csv(myopia_month,'./results/figure2BC/noMonth/myopia.csv',quote=F)

res_month = doMultiCox(myopia_group1, if_month = F, myopia = T)
res_month_uni = doUniCox(myopia_group1,if_month = F)
myopia_month = cbind(res_month, res_month_uni)
write.csv(myopia_month,'./results/figure2BC/myopia_group1.csv',quote=F)

res_month = doMultiCox(myopia_group2, if_month = F, myopia = T)
res_month_uni = doUniCox(myopia_group2,if_month = F)
myopia_month = cbind(res_month, res_month_uni)
write.csv(myopia_month,'./results/figure2BC/noMonth/myopia_group2.csv',quote=F)




highMyopia = comb[which(comb$high_1 == 0),]
highMyopia$key = as.factor(highMyopia$key)
highMyopia$gender = as.factor(highMyopia$gender)
highMyopia$region = as.factor(highMyopia$region)


#### time and status
time = rep(0,nrow(highMyopia))
status = rep(0,nrow(highMyopia))
## 第二次检测是近视，time=6，status=1
time[which(highMyopia$high_2 == 1)] = 6
status[which(highMyopia$high_2 == 1)] = 1
## 第二次检测不是近视，time肯定都是12
time[which(highMyopia$high_2 == 0)] = 12
## 第三次检测是1的status才是1
status[which(highMyopia$high_3 == 1)] = 1
highMyopia$time = time
highMyopia$status = status
highMyopia = na.omit(highMyopia)

highMyopia_group1 = highMyopia[which(highMyopia$grade<7),]
highMyopia_group2 = highMyopia[which(highMyopia$grade>6),]

res_month = doMultiCox(highMyopia, if_month = F, myopia = T)
res_month_uni = doUniCox(highMyopia,if_month = F)
myopia_month = cbind(res_month, res_month_uni)
write.csv(myopia_month,'./results/figure2BC/noMonth/highMyopia.csv',quote=F)

res_month = doMultiCox(highMyopia_group1, if_month = F, myopia = T)
res_month_uni = doUniCox(highMyopia_group1,if_month = F)
myopia_month = cbind(res_month, res_month_uni)
write.csv(myopia_month,'./results/figure2BC/noMonth/highMyopia_group1.csv',quote=F)

res_month = doMultiCox(highMyopia_group2, if_month = F, myopia = T)
res_month_uni = doUniCox(highMyopia_group2,if_month = F)
myopia_month = cbind(res_month, res_month_uni)
write.csv(myopia_month,'./results/figure2BC/noMonth/highMyopia_group2.csv',quote=F)


#####################  2021 rebuttle
### figure 2BC
### 每个年级随机抽取5%，10%，20%，30%，40%，50%

######################################
######## functions   #################
######################################

#### stratidied sampling
# package: sampling
# example: 
#         sub_data = stratifiedSampling(comb, by='grade', ratio=0.05)
stratifiedSampling = function(data, by='grade', ratio=0.1){
  library(sampling)
  index = which(colnames(data) == `by`)
  data_by_order = unique(data[,index])
  sizes = round(table(data[,index])*ratio)
  sizes = sizes[rank(data_by_order)]    ## adjust the rank of size
  sub_random = sampling::strata(data,stratanames=(`by`),size=sizes,method="srswor")
  getdata(data,sub_random)
}

doLoopRatio = function(data, n=500, ratio=0.05){
  random_res = lapply(1:n, function(x){
    sub_data = c()
    while (T) {
      sub_data_tem = stratifiedSampling(data,ratio = ratio)
      if(length(unique(sub_data_tem$age)) == 15){
        sub_data = sub_data_tem
        break
      }
    }
    res = doMultiCox(sub_data, if_month = T, myopia = T)
    res$variable = rownames(res)
    res$ratio = rep(ratio,nrow(res))
    res
  })
  do.call('rbind',random_res)
}



doLoopRatio = function(data, n=500, ratio=0.05){
  random_res = lapply(1:n, function(x){
    sub_data = stratifiedSampling(data,ratio = ratio)
    res = doMultiCox(sub_data, if_month = T, myopia = T)
    res$variable = rownames(res)
    res$ratio = rep(ratio,nrow(res))
    res
  })
  do.call('rbind',random_res)
}


random_myopia_1000 = doLoopRatio(myopia,n=1000,ratio = 0.1)
write.csv(random_myopia_500,'./results/rebuttal_opthal/figure2BC/myopia_random_1000.csv',quote=F,row.names = F)
random_myopia_1000 = read.csv('./results/rebuttal_opthal/figure2BC/myopia_random_1000.csv')

random_highMyopia_1000 = doLoopRatio(highMyopia,n=1000,ratio = 0.1)
write.csv(random_highMyopia_500,'./results/rebuttal_opthal/figure2BC/highMyopia_random_1000.csv',quote=F,row.names = F)
random_highMyopia_1000 = read.csv('./results/rebuttal_opthal/figure2BC/highMyopia_random_1000.csv')

par(mfrow=c(2,2))
plot(density(random_myopia_1000[which(random_myopia_1000$variable == 'grade'),]$`Pr(>|z|)`),
     xlab = '',
     ylab = 'Distribution of P value',
     main = 'Grade')
plot(density(random_myopia_1000[which(random_myopia_1000$variable == 'age'),]$`Pr(>|z|)`),
     xlab = '',
     ylab = 'Distribution of P value',
     main = 'Age')
plot(density(random_highMyopia_1000[which(random_highMyopia_1000$variable == 'grade'),]$`Pr(>|z|)`),
     xlab = '',
     ylab = 'Distribution of P value',
     main = 'Grade')
plot(density(random_highMyopia_1000[which(random_highMyopia_1000$variable == 'age'),]$`Pr(>|z|)`),
     xlab = '',
     ylab = 'Distribution of P value',
     main = 'Age')

data = data.frame()
while(T){
  sub_data = stratifiedSampling(myopia,ratio = 0.1)
  res = doMultiCox(sub_data, if_month = T, myopia = T)
  if(res['grade','Pr(>|z|)'] > 0.1){
    data = sub_data
    print(res$Pval)
    break
  }
}







random_myopia_1000$`-log10(P)` = -log10(as.numeric(random_myopia_1000$`Pr(>|z|)`))
random_highMyopia_1000$`-log10(P)` = -log10(as.numeric(random_highMyopia_1000$`Pr(>|z|)`))
par(mfrow=c(2,2))
plot(density(random_myopia_1000[which(random_myopia_1000$variable == 'grade'),]$`-log10(P)`),
     xlab = '',
     ylab = 'Distribution of P value (-log10)',
     main = 'Grade')
abline(v=1.3,col='brown')
plot(density(random_myopia_1000[which(random_myopia_1000$variable == 'age'),]$`-log10(P)`),
     xlab = '',
     ylab = 'Distribution of P value (-log10)',
     main = 'Age')
abline(v=1.3,col='brown')
plot(density(random_highMyopia_1000[which(random_highMyopia_1000$variable == 'grade'),]$`-log10(P)`),
     xlab = '',
     ylab = 'Distribution of P value (-log10)',
     main = 'Grade')
abline(v=1.3,col='brown')
plot(density(random_highMyopia_1000[which(random_highMyopia_1000$variable == 'age'),]$`-log10(P)`),
     xlab = '',
     ylab = 'Distribution of P value (-log10)',
     main = 'Age')
abline(v=1.3,col='brown')



library(dplyr)
myopia_1000 = group_by(random_myopia_1000, variable)
myopia_1000_summary = summarise(myopia_1000,
                                #myopia_mean_pval = mean(`Pr(>|z|)`),
                                myopia_median_pval = median(`Pr(>|z|)`),
                                myopia_sd_pval = sd(`Pr(>|z|)`))
summPval = as.data.frame(myopia_1000_summary)
high_1000 = group_by(random_highMyopia_1000, variable)
high_1000_summary = summarise(high_1000,
                              #mean_pval = mean(`Pr(>|z|)`),
                              high_median_pval = median(`Pr(>|z|)`),
                              high_sd_pval = sd(`Pr(>|z|)`))
summPval= cbind(summPval, as.data.frame(high_1000_summary))
numberCI(summPval$myopia_sd_pval,rep(1000,6))
write.csv(summPval[,c(2,3,5,6)],'./results/rebuttal_opthal/random_1000_20210118',quote = F)




library(ggplot2)

random_myopia_500$`-log10(P)` = -log10(random_myopia_500$`Pr(>|z|)`)
ggplot(random_myopia_500, aes(x=variable,y=`Pr(>|z|)`))+geom_boxplot()
random_myopia_1000$variable = factor(random_myopia_1000$variable,
                                     levels = c('age','grade','month','gender','habitation','key'))
box_1 = ggplot(random_myopia_1000, aes(x=variable,y=`-log10(P)`))+
  theme_linedraw(base_size=10)+
  geom_boxplot(outlier.shape = NA,varwidth = T,width=0.5)+ylim(0,25)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray'),
        panel.border = element_blank(),
        axis.line  = element_line(),
        legend.position = 'right',
        legend.key.size = unit(10,'pt'),
        legend.title = element_blank(),
        legend.margin = margin(0,5,5,5),
        legend.text = element_text(size=10),
        axis.text = element_text(size=10),
        legend.background = element_rect(color = 'gray'))+
  scale_x_discrete(labels=c('Age','Grade','Month','Gender','Habitation','Key'))+
  ylab('Distribution of P value (-log10P)')+
  xlab('')
random_highMyopia_1000$variable = factor(random_highMyopia_1000$variable,
                                     levels = c('age','grade','month','gender','habitation','key'))
box_2 = ggplot(random_highMyopia_1000, aes(x=variable,y=`-log10(P)`))+
  theme_linedraw(base_size=10)+
  geom_boxplot(outlier.shape = NA,varwidth = T,width=0.5)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = 'gray'),
        panel.border = element_blank(),
        axis.line  = element_line(),
        legend.position = 'right',
        legend.key.size = unit(10,'pt'),
        legend.title = element_blank(),
        legend.margin = margin(0,5,5,5),
        legend.text = element_text(size=10),
        axis.text = element_text(size=10),
        legend.background = element_rect(color = 'gray'))+
  scale_x_discrete(labels=c('Age','Grade','Month','Gender','Habitation','Key'))+
  ylab('Distribution of P value (-log10P)')+
  scale_y_continuous(
    limits = c(0,15),
    breaks=seq(0,15,2.5))+
  xlab('')
ggdraw()+
  draw_plot(box_1, 0.02,0,0.48,0.86)+
  draw_plot(box_2, 0.52,0,0.48,0.86)+
  draw_plot_label(c('A    Myopia',
                    'B    High myopia'),
                  c(0.02,0.52),c(1,1),size=10,
                  fontface = 1,
                  hjust = 0,vjust = 2)



sum(random_myopia_1000[which(random_myopia_1000$variable == 'grade'),]$Pr...z.<0.01)/1000
sum(random_myopia_1000[which(random_myopia_1000$variable == 'age'),]$Pr...z.<0.01)/1000

random_highMyopia_500$`-log10(P)` = -log10(random_highMyopia_500$`Pr(>|z|)`)
ggplot(random_highMyopia_500, aes(x=variable,y=`Pr(>|z|)`))+geom_boxplot()
ggplot(random_highMyopia_1000, aes(x=variable,y=`-log10(P)`))+geom_boxplot()
sum(random_highMyopia_1000[which(random_highMyopia_1000$variable == 'grade'),]$Pr...z.<0.01)/1000
sum(random_highMyopia_1000[which(random_highMyopia_1000$variable == 'age'),]$Pr...z.<0.01)/1000

sub_myopia = stratifiedSampling(myopia,ratio = 0.1)
table(sub_myopia$grade)
nrow(sub_myopia)
doMultiCox(sub_myopia, if_month = T, myopia = T)

sub_highMyopia = stratifiedSampling(highMyopia,ratio = 0.1)
table(sub_highMyopia$grade)
nrow(sub_highMyopia)
doMultiCox(sub_highMyopia, if_month = T, myopia = F)
myopia_uni = doUniCox(myopia,if_month = F)

library(sampling)
aa = sapply()
comb$grade = factor(comb$grade,levels = 1:11)
comb$grade = as.numeric(comb$grade)
aa = unique(comb$grade)
size = round(table(comb$grade)*0.1)
size = size[rank(aa)]
sub_train=sampling::strata(comb,"grade",size,"srswor")
sub_data = getdata(comb,sub_train)


###
表格给出样本量