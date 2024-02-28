## myopia prevalence
## line chart with error bar
library(dplyr)
library(RColorBrewer)
mycolor <- brewer.pal(8,"Paired")
mycolor1 <- brewer.pal(9,"Set1")

CI = function(p,n, z=1.96){
  ## @param p: rate
  ## @param n: number of samples
  sd = z*sqrt(p*(1-p)/n)
  return(sd)
}


####画折线图并添加误差线，还有图例添加p_value值
plot.error1 <- function(x,y,s1,s2,len = 1,col = "black") { 
  len <- len * 0.05
  arrows(x0 = x, y0 = y, x1 = x, y1 = s1, col = col, angle = 90, length = len)
  arrows(x0 = x, y0 = y, x1 = x, y1 = s2, col = col, angle = 90, length = len)
  
}

getRate = function(data, splitBy){
  #print(splitBy)
  index = which(colnames(data) == splitBy)
  colnames(data)[index] = 'splitBy'
  data_groupby = group_by(data,splitBy)
  data_groupby =  summarise(data_groupby,
                          count = n(),                #个数
                          sum_high = sum(high),
                          sum_nonHigh = sum(nonHigh)) 
  data_groupby = as.data.frame(data_groupby)
  data_groupby$high_rate = data_groupby$sum_high/data_groupby$count
  data_groupby$nonHigh_rate = data_groupby$sum_nonHigh/data_groupby$count
  data_groupby$all_rate = (data_groupby$sum_high+data_groupby$sum_nonHigh)/data_groupby$count
  data_groupby$CI_sd_high = sapply(1:nrow(data_groupby), function(i){
    CI(data_groupby$high_rate[i], data_groupby$count[i])
  })
  data_groupby$CI_sd_nonHigh = sapply(1:nrow(data_groupby), function(i){
    CI(data_groupby$nonHigh_rate[i], data_groupby$count[i])
  })
  data_groupby$CI_sd_all = sapply(1:nrow(data_groupby), function(i){
    CI(data_groupby$all_rate[i], data_groupby$count[i])
  })
  data_groupby$CI_highMyopia_lower = data_groupby$high_rate - data_groupby$CI_sd_high
  data_groupby$CI_highMyopia_upper = data_groupby$high_rate + data_groupby$CI_sd_high
  data_groupby$CI_nonHighMyopia_lower = data_groupby$nonHigh_rate - data_groupby$CI_sd_nonHigh
  data_groupby$CI_nonHighMyopia_upper = data_groupby$nonHigh_rate + data_groupby$CI_sd_nonHigh
  data_groupby$CI_all_rate_lower = data_groupby$all_rate - data_groupby$CI_sd_all
  data_groupby$CI_all_rate_upper = data_groupby$all_rate + data_groupby$CI_sd_all
  return(data_groupby)
}

stage = 'first'

first_data = read.csv(paste0('E:/myopia2020/2020/results/1014/figure2/',stage,'_filteredData.csv'))
#first_data = na.omit(first_data)
first_groupby = getRate(first_data)
write.csv(first_groupby,paste0('E:/myopia2020/2020/results/1014/figure2_doc/',stage,'_all.csv'))

## plot revalence rate
pdf(paste0('E:/myopia2020/2020/results/1014/supp2/',stage,'_all.pdf'))
x <- 1:nrow(first_groupby)
par(las = 1)
plot(first_groupby$high_rate,type = "b",xaxt="n",axes=F,yaxt="n",ylim = c(0,1),
     ylab="Myopia Prevalence(%)",xlab="Grade",col=mycolor[8],
     lwd = 4,pch = 16,cex.axis=2.5,cex.lab=1.5,cex.main=1.5)
axis(1,x,labels=first_groupby$grade)
axis(2, seq(0,1,0.2), labels = paste0(seq(0,1,0.2)*100,'%'))
plot.error1(x,first_groupby$high_rate,
            first_groupby$high_rate - first_groupby$CI_sd_high,
            first_groupby$high_rate + first_groupby$CI_sd_high,
            len = 1,
            col=mycolor[8])
par(new = T)
plot(first_groupby$nonHigh_rate,type = "b",xlab = "",ylab = "",
     col=mycolor[2],lwd = 4,pch = 16,xaxt="n",
     axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,first_groupby$nonHigh_rate,
            first_groupby$nonHigh_rate - first_groupby$CI_sd_nonHigh,
            first_groupby$nonHigh_rate + first_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[2])
par(new = TRUE)
plot(first_groupby$all_rate,type = "b",xlab = "",ylab = "",col=mycolor1[9],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,first_groupby$all_rate,
            first_groupby$all_rate - first_groupby$CI_sd_all,
            first_groupby$all_rate + first_groupby$CI_sd_all,
            len = 1,
            col=mycolor1[9])
legend("top",legend = c("High myopia","Non-high Myopia","All myopia"),
       col = c(mycolor[8],mycolor[2],mycolor1[9]),
       pch = 16,bty="n",xpd = T,ncol = 3)
dev.off()



### gender
male = first_data[which(first_data$gender == 0),]
female = first_data[which(first_data$gender == 1),]
male_groupby = getRate(male)
female_groupby = getRate(female)
write.csv(rbind(male_groupby,female_groupby),
          paste0('E:/myopia2020/2020/results/1014/figure2_doc/',stage,'_gender.csv'))
pdf(paste0('E:/myopia2020/2020/results/1014/supp2/',stage,'_gender.pdf'))
x <- 1:nrow(male_groupby)
par(las = 1)
plot(male_groupby$nonHigh_rate,type = "b",xaxt="n",axes=F,yaxt="n",ylim = c(0,1),
     ylab="Myopia Prevalence(%)",xlab="Grade",col=mycolor[1],
     lwd = 4,pch = 16,cex.axis=2.5,cex.lab=1.5,cex.main=1.5)
axis(1,x,labels=male_groupby$grade)
axis(2, seq(0,1,0.2), labels = paste0(seq(0,1,0.2)*100,'%'))
plot.error1(x,male_groupby$nonHigh_rate,
            male_groupby$nonHigh_rate - male_groupby$CI_sd_nonHigh,
            male_groupby$nonHigh_rate + male_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[1])
par(new = T)
plot(female_groupby$nonHigh_rate,type = "b",xlab = "",ylab = "",
     col=mycolor[2],lwd = 4,pch = 16,xaxt="n",
     axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,female_groupby$nonHigh_rate,
            female_groupby$nonHigh_rate - female_groupby$CI_sd_nonHigh,
            female_groupby$nonHigh_rate + female_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[2])
par(new = TRUE)
plot(male_groupby$high_rate,type = "b",xlab = "",ylab = "",col=mycolor[7],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,male_groupby$high_rate,
            male_groupby$high_rate - male_groupby$CI_sd_high,
            male_groupby$high_rate + male_groupby$CI_sd_high,
            len = 1,
            col=mycolor[7])
par(new = TRUE)
plot(female_groupby$high_rate,type = "b",xlab = "",ylab = "",col=mycolor[8],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,female_groupby$high_rate,
            female_groupby$high_rate - female_groupby$CI_sd_high,
            female_groupby$high_rate + female_groupby$CI_sd_high,
            len = 1,
            col=mycolor[8])
legend("top",legend = c("Non-High myopia (Female)","High Myopia (Female)",
                        "Non-high myopia (Male)","High Myopia (Male)"),
       col = c(mycolor[2],mycolor[8],mycolor[1],mycolor[7]),
       pch = 16,bty="n",ncol = 2,
       cex = 1,y.intersp = 1.3,xpd = T,xjust = 2,yjust = 2)
dev.off()



### rural or town
town = first_data[which(first_data$region == 1),]
rural = first_data[which(first_data$region == 0),]
town_groupby = getRate(town)
rural_groupby = getRate(rural)
write.csv(rbind(town_groupby,rural_groupby),
          paste0('E:/myopia2020/2020/results/1014/figure2_doc/',stage,'_region.csv'))
pdf(paste0('E:/myopia2020/2020/results/1014/supp2/',stage,'_region.pdf'))
x <- 1:nrow(rural_groupby)
par(las = 1)
plot(rural_groupby$nonHigh_rate,type = "b",xaxt="n",axes=F,yaxt="n",ylim = c(0,1),
     ylab="Myopia Prevalence(%)",xlab="Grade",col=mycolor[1],
     lwd = 4,pch = 16,cex.axis=2.5,cex.lab=1.5,cex.main=1.5)
axis(1,x,labels=rural_groupby$grade)
axis(2, seq(0,1,0.2), labels = paste0(seq(0,1,0.2)*100,'%'))
plot.error1(x,rural_groupby$nonHigh_rate,
            rural_groupby$nonHigh_rate - rural_groupby$CI_sd_nonHigh,
            rural_groupby$nonHigh_rate + rural_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[1])
par(new = T)
plot(town_groupby$nonHigh_rate,type = "b",xlab = "",ylab = "",
     col=mycolor[2],lwd = 4,pch = 16,xaxt="n",
     axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,town_groupby$nonHigh_rate,
            town_groupby$nonHigh_rate - town_groupby$CI_sd_nonHigh,
            town_groupby$nonHigh_rate + town_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[2])

par(new = TRUE)
plot(rural_groupby$high_rate,type = "b",xlab = "",ylab = "",col=mycolor[7],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,rural_groupby$high_rate,
            rural_groupby$high_rate - rural_groupby$CI_sd_high,
            rural_groupby$high_rate + rural_groupby$CI_sd_high,
            len = 1,
            col=mycolor[7])

par(new = TRUE)
plot(town_groupby$high_rate,type = "b",xlab = "",ylab = "",col=mycolor[8],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,town_groupby$high_rate,
            town_groupby$high_rate - town_groupby$CI_sd_high,
            town_groupby$high_rate + town_groupby$CI_sd_high,
            len = 1,
            col=mycolor[8])

legend("top",legend = c("Non-High myopia (Town)","High Myopia (Town)",
                        "Non-high myopia (Rural)","High Myopia (Rural)"),
       col = c(mycolor[2],mycolor[8],mycolor[1],mycolor[7]),
       pch = 16,bty="n",ncol = 2,
       cex = 1,y.intersp = 1.3,xpd = T,xjust = 2,yjust = 2)
dev.off()



### key or non-key
key = first_data[which(first_data$key == 1),]
noKey = first_data[which(first_data$key == 0),]
key_groupby = getRate(key)
noKey_groupby = getRate(noKey)
write.csv(rbind(key_groupby,noKey_groupby),
          paste0('E:/myopia2020/2020/results/1014/figure2_doc/',stage,'_key.csv'))
pdf(paste0('E:/myopia2020/2020/results/1014/supp2/',stage,'_key.pdf'))
x <- 1:nrow(noKey_groupby)
par(las = 1)
plot(noKey_groupby$nonHigh_rate,type = "b",xaxt="n",axes=F,yaxt="n",ylim = c(0,1),
     ylab="Myopia Prevalence(%)",xlab="Grade",col=mycolor[1],
     lwd = 4,pch = 16,cex.axis=2.5,cex.lab=1.5,cex.main=1.5)
axis(1,x,labels=noKey_groupby$grade)
axis(2, seq(0,1,0.2), labels = paste0(seq(0,1,0.2)*100,'%'))
plot.error1(x,noKey_groupby$nonHigh_rate,
            noKey_groupby$nonHigh_rate - noKey_groupby$CI_sd_nonHigh,
            noKey_groupby$nonHigh_rate + noKey_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[1])
par(new = T)
plot(key_groupby$nonHigh_rate,type = "b",xlab = "",ylab = "",
     col=mycolor[2],lwd = 4,pch = 16,xaxt="n",
     axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,key_groupby$nonHigh_rate,
            key_groupby$nonHigh_rate - key_groupby$CI_sd_nonHigh,
            key_groupby$nonHigh_rate + key_groupby$CI_sd_nonHigh,
            len = 1,
            col=mycolor[2])

par(new = TRUE)
plot(noKey_groupby$high_rate,type = "b",xlab = "",ylab = "",col=mycolor[7],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,noKey_groupby$high_rate,
            noKey_groupby$high_rate - noKey_groupby$CI_sd_high,
            noKey_groupby$high_rate + noKey_groupby$CI_sd_high,
            len = 1,
            col=mycolor[7])

par(new = TRUE)
plot(key_groupby$high_rate,type = "b",xlab = "",ylab = "",col=mycolor[8],
     lwd = 4,pch = 16,xaxt="n",axes=F,yaxt="n",ylim = c(0,1))
plot.error1(x,key_groupby$high_rate,
            key_groupby$high_rate - key_groupby$CI_sd_high,
            key_groupby$high_rate + key_groupby$CI_sd_high,
            len = 1,
            col=mycolor[8])

legend("top",legend = c("Non-High myopia (Key)","High Myopia (Key)",
                        "Non-high myopia (Non-key)","High Myopia (Non-key)"),
       col = c(mycolor[2],mycolor[8],mycolor[1],mycolor[7]),
       pch = 16,bty="n",ncol = 2,
       cex = 1,y.intersp = 1.3,xpd = T,xjust = 2,yjust = 2)
dev.off()




