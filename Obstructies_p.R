data <- read.delim("obstructies.txt", header =TRUE, sep = "\t")
data <- data[1:14,]

data1 <- data.frame( c(rep("Control", 14*6), rep("BOS",14*6), rep("RAS-B",14*3), rep("RAS-R",14*3)), 
                c(rep(1:14,18)), c( data[,2], data[,3], data[,4], data[,5], data[,6],data[,7], data[,8], data[,9],
                                    data[,10], data[,11], data[,12], data[,13],data[,14], data[,15],
                                    data[,16], data[,17], data[,18], data[,19]))
colnames(data1) <- c("Lungtype", "Generation", "Ratio")
data1$Lungtype <- factor(data1$Lungtype)
data1[,1] <- factor(data1[,1], levels = c("Control", "BOS", "RAS-B", "RAS-R"))


model <- lm(formula = (Ratio ~ Generation + Generation:Lungtype + Lungtype) , data = data1)
summary(model)
library(ggplot2)
ggplot(data1, aes(x=Generation, y=Ratio, group = Lungtype, color= Lungtype)) +
  #stat_summary(fun=mean, geom = "point") + #adds points
  stat_summary(fun=mean, geom = "line", aes(group = Lungtype), size =1.2) + #adds lines for different groups
  scale_color_manual(breaks = c("Control","BOS", "RAS-B", "RAS-R"),
                     values=c("black","red3","springgreen3", "dodgerblue3"))+ #colors groups
  stat_summary(fun.data= mean_se, geom = "errorbar", width= .2, size = 1)+ # confidence interval
  labs(y= expression(paste("Ratio of obstructed airways")), x="Generation") + #label axes
  #ggtitle(cell_codes_sig[cell,1]) + #plot title
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA), #"black"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(size = 22))+
  theme(legend.position= c(.15, .8), legend.title=element_blank(), 
        legend.direction = "vertical", legend.key = element_rect(fill = NA, color = NA)) + ## legende opmaak
  theme(plot.margin = margin(.5,2,.5,1.5, "cm")) + #margins around plot (top, right, bottom, left)
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 0.8))





library(ggplot2)
ggplot(data=data, aes(x=Generation, y=Ratio, color=Lungtype))+
  geom_point() +
  geom_smooth(method="lm")