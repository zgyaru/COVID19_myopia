install.packages("forestplot")
library(forestplot)

yj_forest=read.table("C:/Users/Baosq/Desktop/yj/forrest plot.txt", header=T, sep="\t")

forest_text=data.frame(datasets=yj_forest$data,
                       factor=yj_forest$myopia.VS.control, 
                       OR.95CI=paste(round(yj_forest$OR,3), "(", round(yj_forest$CI.lower,3), "-", round(yj_forest$CI.higher.,3), ")", sep = " "),
                       P_value=round(yj_forest$P.value,3))

load('./data/yj_forest.Rdata')
forest_text = data.frame(OR=yj_forest$OR, CI.lower=yj_forest$CI.lower, upper=yj_forest$CI.higher.)
forestplot(forest_text, mean = yj_forest$OR, lower = yj_forest$CI.lower, upper = yj_forest$CI.higher.,
           graph.pos = 3,
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex = 0.9),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero = 1, lwd.zero=2, boxsize=0.3, lwd.ci=2, colgap=unit(2,"mm"),
           line.margin = unit(5,"mm"),
           lineheight = unit(8,"mm"),
           ci.vertices=TRUE, ci.vertices.height = 0.15)


table1 = data.frame(HR = c(1.05, 2.01, 1.69, 1.11, 1.04),
                    lower = c(1.02, 1.89, 1.53, 1.07, 0.996),
                    upper = c(1.08, 2.15, 1.86, 1.16, 1.09))
table1$HR.95CI=paste(round(table1$HR,3), "(", round(table1$lower,3), "-", round(table1$upper,3), ")", sep = " ")
table1$name = c('Female VS Male', 'Junior VS Primary', 
                'Senior VS Primary', 'Key VS Non-Key', 
                'Urban VS Rural')
pdf('H:/myopia2020/2020/covid/results/Table4.pdf',height = 2,width  = 8)
forestplot(labeltext = as.matrix(table1[,c(5,4)]), 
           mean = table1$HR, 
           lower = table1$lower, 
           upper = table1$upper,
           graph.pos = 2,
           xlab="Adjusted HR",
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=0.9),
           col=fpColors(box="black", lines="black", zero="gray50"),
           zero=1, lwd.zero=2, boxsize=0.3, 
           lwd.ci=2, colgap=unit(7,"mm"),
           line.margin = unit(2,"mm"),
           lineheight = unit(10,"mm"),
           ci.vertices=TRUE, 
           ci.vertices.height = 0.15)
dev.off()


table1 = data.frame(HR = c(1.25, 0.76, 0.40, 1.08, 1.18),
                    lower = c(1.23, 0.74, 0.38, 1.06, 1.15),
                    upper = c(1.27, 0.79, 0.43, 1.11, 1.20))
table1$HR.95CI=paste(round(table1$HR,3), "(", round(table1$lower,3), "-", round(table1$upper,3), ")", sep = " ")
table1$name = c('Female VS Male', 'Junior VS Primary', 
                'Senior VS Primary', 'Key VS Non-Key', 
                'Urban VS Rural')
pdf('H:/myopia2020/2020/covid/results/Table3.pdf',height = 2,width  = 8)
forestplot(labeltext = as.matrix(table1[,c(5,4)]), 
           mean = table1$HR, 
           lower = table1$lower, 
           upper = table1$upper,
           graph.pos = 2,
           xlab="Adjusted HR",
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=0.9),
           col=fpColors(box="black", lines="black", zero="gray50"),
           zero=1, lwd.zero=2, boxsize=0.3, 
           lwd.ci=2, colgap=unit(7,"mm"),
           line.margin = unit(2,"mm"),
           lineheight = unit(10,"mm"),
           ci.vertices=TRUE, 
           ci.vertices.height = 0.15)
dev.off()


or1 = read.table('H:/myopia2020/2020/covid/data/or.txt',sep='\t')
or1$OR = sapply(or1$V6, function(x) strsplit(x, split=' ')[[1]][1])
or1$lower = sapply(or1$V6, function(x){
  aa = strsplit(x, split = '-')[[1]][1]
  strsplit(aa, split = '\\(')[[1]][2]
})
or1$upper = sapply(or1$V6, function(x){
  aa = strsplit(x, split = '-')[[1]][2]
  strsplit(aa, split = '\\)')[[1]][1]
})
or1$summary = is.na(or1$OR)
pdf('H:/myopia2020/2020/covid/results/forestPlot/Table5.pdf',height = 8,width  = 10)
forestplot(labeltext = as.matrix(or1[,c('V1','V6')]), 
           mean = as.numeric(or1$OR), 
           lower = as.numeric(or1$lower), 
           upper = as.numeric(or1$upper),
           is.summary=or1$summary,
           graph.pos = 2,
           xlab="Odds Ratio",
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=0.9),
           col=fpColors(box="black", lines="black", zero="gray50"),
           zero=1, lwd.zero=2, boxsize=0.3, 
           lwd.ci=2, colgap=unit(5,"mm"),
           line.margin = unit(2,"mm"),
           lineheight = unit(9,"mm"),
           ci.vertices=TRUE, 
           ci.vertices.height = 0.15)
dev.off()


or2 = read.table('H:/myopia2020/2020/covid/data/or2.txt',sep='\t')
or2$OR = sapply(or2$V6, function(x) strsplit(x, split=' ')[[1]][1])
or2$lower = sapply(or2$V6, function(x){
  aa = strsplit(x, split = '-')[[1]][1]
  strsplit(aa, split = '\\(')[[1]][2]
})
or2$upper = sapply(or2$V6, function(x){
  aa = strsplit(x, split = '-')[[1]][2]
  strsplit(aa, split = '\\)')[[1]][1]
})
or2$summary = is.na(or2$OR)
pdf('H:/myopia2020/2020/covid/results/forestPlot/Table6.pdf',height = 8,width  = 10)
forestplot(labeltext = as.matrix(or2[,c('V1','V6')]), 
           mean = as.numeric(or2$OR), 
           lower = as.numeric(or2$lower), 
           upper = as.numeric(or2$upper),
           is.summary=or2$summary,
           graph.pos = 2,
           xlab="Adjusted HR",
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=0.9),
           col=fpColors(box="black", lines="black", zero="gray50"),
           zero=1, lwd.zero=2, boxsize=0.3, 
           lwd.ci=2, colgap=unit(5,"mm"),
           line.margin = unit(2,"mm"),
           lineheight = unit(9,"mm"),
           ci.vertices=TRUE, 
           grid=TRUE,
           ci.vertices.height = 0.15)
dev.off()




# regression
mixed_linear_progression.nlme =lme(SE_diff~COVID19+baseline+grade+age+birth_month+sex+areaname+school_key+school_town,random = ~1|ID,data= data_progression3)
summary(mixed_linear_progression.nlme)




### figure2 cox BC
plot = read.csv("E:/myopia2020/2020/results/cox/figure2BC/myopia.csv")
#plot$OR = round(plot$exp.coef.,3)
#plot$OR_CI = paste0(plot$exp.coef.,' (',plot$lower..95,',',plot$upper..95,')')
#plot$Pval = c('4.05e-25','0.2','0.63','1.89e-170','3.72e-34','6.23e-07')
#plot$Pval = c('1.24e-57','0.04','0.91','0.15','0.34','6.19e-06')
pdf("E:/myopia2020/2020/results/cox/figure2BC/myopia.pdf",height=5)
forestplot(labeltext = as.matrix(plot[,c('X','OR_CI','Pval')]), 
           mean = plot$exp.coef., 
           lower = plot$lower..95, 
           upper = plot$upper..95,
           graph.pos = 4,
           zero=1,
           boxsize = 0.3,
           #grid = T,
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=1),
           #hrzl_lines =T,
           hrzl_lines = list("1" = gpar(lty=1, lwd= 2, columns=c(1:3),col='black'),
                             "2" = gpar(lty=1, columns=c(1:3),col='gray'),
                             "3" = gpar(lty=1, columns=c(1:3),col='gray'),
                             "4" = gpar(lty=1, columns=c(1:3),col='gray'),
                             "5" = gpar(lty=1, columns=c(1:3),col='gray'),
                             "6" = gpar(lty=1, columns=c(1:3),col='gray')),
           graphwidth=unit(c(50,10,10,10),"mm"),
           colgap=unit(5,"mm"),
           lineheight = unit(10,"mm"),
           #xticks.digits=10,
           xticks = c(0.95,1,1.2),
           #xticks = c(0.9,1,1.4),
           col = fpColors(box = '#2E5662',zero='black',lines = 'black'))
dev.off()




##### figure 4 AC
plot = read.table('E:/myopia2020/2020/results/myl/high.txt',sep = '\t')
colnames(plot) = c('name','num_normal','incidence_normal','num_covid','incidence_covid','OR_CI','Pval')
plot$OR_CI = gsub('-',',',plot$OR_CI)
plot$Pval = gsub(' ','',plot$Pval)
plot$OR = sapply(plot$OR_CI, function(x) as.numeric(strsplit(x,split = ' ')[[1]][1]))
plot$lower = sapply(plot$OR_CI, function(x){
  a = strsplit(x,split = '\\(')[[1]][2]
  as.numeric(strsplit(a,split = ',')[[1]][1])
})
plot$upper = sapply(plot$OR_CI, function(x){
  a = strsplit(x,split = '\\)')[[1]][1]
  as.numeric(strsplit(a,split = ',')[[1]][2])
})
plot$summary = is.na(plot$OR)
pdf("E:/myopia2020/2020/results/cox/figure4AC/high.pdf",width = 16)
forestplot(labeltext = as.matrix(plot[,c('name','incidence_normal','incidence_covid','OR_CI','Pval')]), 
           mean = plot$OR, 
           lower = plot$lower, 
           upper = plot$upper,
           is.summary = plot$summary,
           graph.pos = 5,
           zero=1,
           boxsize = 0.3,
           #grid = T,
           txt_gp = fpTxtGp(ticks = gpar(cex=0.9), cex=1),
           #hrzl_lines =T,
           hrzl_lines = list("1" = gpar(lty=1, lwd= 2, columns=c(1:4),col='black'),
                             "2" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "3" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "4" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "5" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "6" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "7" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "8" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "9" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "10" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "11" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "12" = gpar(lty=1, columns=c(1:4),col='gray'),
                             "13" = gpar(lty=1, columns=c(1:4),col='gray')),
           #graphwidth=unit(50,"mm"),
           colgap=unit(5,"mm"),
           lineheight = unit(10,"mm"),
           graphwidth = unit(c(60,10,10,10,10,10),"mm"),
           #xticks.digits=10,
           #xticks = c(1,1.2),
           #xticks = c(0.9,1,1.4),
           col = fpColors(box = '#2E5662',zero='black',lines = 'black'))
dev.off()
