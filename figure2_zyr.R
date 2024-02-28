## Prevalence of non-high myopia and high myopia among different birth-year
## stacked bar
library(RColorBrewer)
library(ggplot2)
mycolor <- brewer.pal(12,"Paired")#Set3
mycolor1 <- brewer.pal(11,"BrBG")
namecolour <- c(rep(mycolor[1],12),rep(mycolor[2],12),rep(mycolor[3],12),rep(mycolor[4],12),rep(mycolor[5],12),rep(mycolor[6],12),rep(mycolor[7],12),rep(mycolor[8],12),rep(mycolor[9],12),rep(mycolor[10],12),rep(mycolor[11],12),rep(mycolor[12],12))

jamaCol = c('#2E5662','#86AAB8')

first_data = read.csv('E:/myopia2020/2020/results/1014/figure2/first_myopiaRate.csv')
#first_data = first_data[which(first_data$year %in% 2001:2012),]
first_data = na.omit(first_data)
first_data$year_month = paste(first_data$year, first_data$month, sep = '_')

first_plot_data = data.frame(c(first_data$nonHigh_rate,
                               first_data$high_rate))
colnames(first_plot_data) = c('rate')
first_plot_data$year_month = factor(c(first_data$year_month,first_data$year_month),
                                    levels = first_data$year_month)
first_plot_data$level = factor(c(rep('non-high myopia',nrow(first_data)),
                                 rep('high myopia',nrow(first_data))),
                               level=c('high myopia','non-high myopia'))

pdf('E:/myopia2020/2020/results/1014/figure2/second.pdf',width = 11,height = 6)
p <- ggplot(first_plot_data,
            aes(x=year_month,y=rate,fill = level))
p+geom_bar(stat = "identity",position = position_stack(),width = 0.8)+
  scale_y_continuous(labels = scales::percent)+
  theme_bw()+theme_classic()+
  theme(axis.text.x = element_text(colour = namecolour,angle=90,vjust = 0.5),
        axis.text = element_text(face = "bold"),
        axis.text.y = element_text(colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(face = "bold"),
        axis.line.x = element_blank(),
        axis.ticks.x = element_line(linetype = 3,size = 1))+
  xlab("date of birth")+ylab("Myopia prevalence(%)")+
  scale_fill_manual(values = jamaCol)



pbuild <- ggplot_build(plot = p)
y.range <- diff(x = pbuild$layout$panel_params[[1]]$y.range)
y.pos <- max(pbuild$layout$panel_params[[1]]$y.range) + y.range * 0.015
y.max <- y.pos + 0.02 * y.range
p +
  annotation_raster(
    raster = t(x = mycolor),
    xmin = -Inf,
    xmax = Inf,
    ymin = y.pos,
    ymax = y.max
  ) +
  coord_cartesian(ylim = c(0, y.max), clip = 'off') +
  scale_color_manual(values = mycolor)

dev.off()

