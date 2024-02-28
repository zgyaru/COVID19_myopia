library(dplyr)
library(ggthemes)
library(cowplot)
source('./plot_theme.R')

#bar_color = c('#2472A9','#399938')
bar_color = c('#DBD3C3','#86AAB8')
errorbar_color = c('#898071','#2D545F')

CI = function(p,n, z=1.96){
  ## @param p: rate
  ## @param n: number of samples
  sd = z*sqrt(p*(1-p)/n)
  return(sd)
}


data1 = read.table('E:/myopia2020/2020/data/2.1.调查问卷模型信息-右眼SE降低.txt',sep='\t',header = T,row.names = 1)
data2 = read.table('E:/myopia2020/2020/data/2.2.调查问卷模型信息-右眼SE反常.txt',sep='\t',header = T,row.names = 1)
##########################################
############### 数据整合 #################
##########################################
questionnaire = rbind(data1,data2)
colnames(questionnaire) = c('gender', 'grade', 'SE1', 'SE2', 'SE3', 'high', 'weigh', 'bornStyle', 'childEyeHealth',
                            'firstShortSight', 'studyPost', 
                            'readTimeAfterSchool_norm', 'onlineTime_norm', 'outdoorTime_norm',
                            'sleepTime', 'eatingHabit', 
                            'fatherEducation', 'fatherEyeHealth', 
                            'motherEducation','motherEyeHealth', 'siblingEyeCondition', 
                            'onlineTime_covid', 'onlineEquipment_covid', 'outdoorTime_covid')
questionnaire$d1 = questionnaire$SE1 - questionnaire$SE2
questionnaire$d2 = questionnaire$SE2 - questionnaire$SE3
questionnaire$d3 = questionnaire$d2 - questionnaire$d1
questionnaire$ID = rownames(questionnaire)


### 过滤掉年进展率的极端值
questionnaire = questionnaire[which(questionnaire$d1>-5),]
questionnaire = questionnaire[which(questionnaire$d1<0.5),]
questionnaire = questionnaire[which(questionnaire$d2>-5),]
questionnaire = questionnaire[which(questionnaire$d2<0.5),]

### 把时间换算成，值越大时间越长
questionnaire$readTimeAfterSchool_norm = 5 - questionnaire$readTimeAfterSchool_norm
questionnaire$onlineTime_norm = 5 - questionnaire$onlineTime_norm
questionnaire$outdoorTime_norm = 5 - questionnaire$outdoorTime_norm
questionnaire$sleepTime = 5 - questionnaire$sleepTime
questionnaire$outdoorTime_covid = 5 - questionnaire$outdoorTime_covid

### 把时间换算成具体的值
questionnaire$onlineTime_covid_value = questionnaire$onlineTime_covid
questionnaire$onlineTime_covid_value[which(questionnaire$onlineTime_covid_value == 1)] = 0
questionnaire$onlineTime_covid_value[which(questionnaire$onlineTime_covid_value == 2)] = 1
questionnaire$onlineTime_covid_value[which(questionnaire$onlineTime_covid_value == 3)] = 3
questionnaire$onlineTime_covid_value[which(questionnaire$onlineTime_covid_value == 4)] = 4

questionnaire$onlineTime_norm_value = questionnaire$onlineTime_norm
questionnaire$onlineTime_norm_value[which(questionnaire$onlineTime_norm_value == 1)] = 0.5
questionnaire$onlineTime_norm_value[which(questionnaire$onlineTime_norm_value == 2)] = 1.5
questionnaire$onlineTime_norm_value[which(questionnaire$onlineTime_norm_value == 3)] = 2.5
questionnaire$onlineTime_norm_value[which(questionnaire$onlineTime_norm_value == 4)] = 3

questionnaire$onlineTime_covid_add = questionnaire$onlineTime_covid_value + questionnaire$onlineTime_norm_value

### outdoor time more than  one hour
questionnaire_low = questionnaire[which(questionnaire$grade<=6),]
questionnaire_high = questionnaire[which(questionnaire$grade>6),]
c1 = c()
c1 = c(c1, length(which(questionnaire_low$outdoorTime_norm >1))/nrow(questionnaire_low))
c1 = c(c1, length(which(questionnaire_low$outdoorTime_covid >1))/nrow(questionnaire_low))
c1 = c(c1, length(which(questionnaire_high$outdoorTime_norm >1))/nrow(questionnaire_high))
c1 = c(c1, length(which(questionnaire_high$outdoorTime_covid >1))/nrow(questionnaire_high))
d1 = data.frame(percent = c1, 
                grade = factor(c('low','low','high','high'),levels = c('low','high')), 
                n = c(nrow(questionnaire_low),nrow(questionnaire_low),
                      nrow(questionnaire_high),nrow(questionnaire_high)),
                period = factor(c('normal','covid','normal','covid'),levels = c('normal','covid')))
d1$se = CI(d1$percent,d1$n)
ggplot(d1)+
  geom_bar(aes(x=grade,y=percent,fill = period),
           position = position_dodge(width=0.7),width = 0.58,stat="identity",alpha=0.75)+
  geom_errorbar(mapping = aes(x=grade,y=percent,color = period,
                              ymin=percent-se,ymax=percent+se),
                position = position_dodge(width=0.7),
                width = 0.2,size=1,
                stat="identity" )+
  geom_text(aes(x=grade,y=percent,fill = period,
                label = round(d1$percent*100, 1)),position=position_dodge(width = 0.7),size = 5,vjust = -1,hjust = 0.5)+
  scale_fill_manual(values = bar_color)+
  scale_discrete_manual(values = errorbar_color,aesthetics = c("colour"))+
  scale_x_discrete(breaks=c('low','high'),labels=c('Grade Group I','Grade Group II'))+
  theme(axis.text.x = element_text(angle = 30))+
  scale_y_continuous(breaks=seq(0,0.5,0.1),labels=seq(0,0.5,0.1)*100)+
  ylab('Percent (%)')+
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 0.5)), aes(x, y))+theme_Publication(base_size=18)


### online time 累加
c2=c()
c2=c(c2, length(which(questionnaire_low$onlineTime_norm_value > 2))/nrow(questionnaire_low))
c2=c(c2, length(which(questionnaire_low$onlineTime_covid_add > 2))/nrow(questionnaire_low))
c2=c(c2, length(which(questionnaire_high$onlineTime_norm_value > 2))/nrow(questionnaire_high))
c2=c(c2, length(which(questionnaire_high$onlineTime_covid_add > 2))/nrow(questionnaire_high))
d2=data.frame(percent = c2, 
              grade = factor(c('low','low','high','high'),levels = c('low','high')), 
              n = c(nrow(questionnaire_low),nrow(questionnaire_low),
                    nrow(questionnaire_high),nrow(questionnaire_high)),
              period = factor(c('normal','covid','normal','covid'),levels = c('normal','covid')))
d2$se = CI(d2$percent,d2$n)
p2 = ggplot(d2)+
  geom_bar(mapping =  aes(x=grade,y=percent,fill = period),
           position = position_dodge(width=0.7),width = 0.58,stat="identity",alpha=0.75)+
  geom_errorbar(mapping = aes(x=grade,y=percent,color = period,
                              ymin=percent-se,ymax=percent+se),
                position = position_dodge(width=0.7),
                width = 0.2,size=1,
                stat="identity" )+
  geom_text(aes(x=grade,y=percent,fill = period,
                label = round(percent*100, 1)),position=position_dodge(width = 0.7),size = 5,vjust = -1,hjust = 0.5)+
  #scale_fill_manual(values = bar_color)+
  scale_discrete_manual(values = bar_color,aesthetics = c("colour", "fill"))+
  scale_x_discrete(breaks=c('low','high'),labels=c('Phase I','Phase II'))+
  scale_y_continuous(breaks=seq(0,1,0.25),labels=seq(0,1,0.25)*100)+
  ylab('Percent (%)')+
  geom_rangeframe(data=data.frame(x=c(1, 2), y=c(0, 1)), aes(x, y))+theme_Publication(base_size=18)


pdf('E:/myopia2020/2020/results/1020/figures/figure3_covid_time_errorBar.pdf',height = 5,width  = 6)
plot_grid(
  p2, p1, 
  rel_widths = c(0.5, 0.5), 
  align = "h",
  labels = c('Online Time (>2H)','Outdoor Time (>1H)'))
dev.off()
