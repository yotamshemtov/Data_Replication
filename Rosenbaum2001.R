
rm(list=ls())

library(ggplot2)
library(ggthemes)
library(estCI)

#############################################
### The data used by Rosenbaum (2001)
#############################################

data(tunca.and.egeli.1996)
y = tunca.and.egeli.1996$y
tr= tunca.and.egeli.1996$tr

results = aveCI(y,tr, print=TRUE)
length.gain = 1 - (results$sattCI[[2]]-results$sattCI[[1]])/(results$neyman[[2]]-results$neyman[[1]])
cat("Percentage length gain for SATT: ",round(length.gain*100),"%","\n",sep="")

length.gain = 1 - (results$satcCI[[2]]-results$satcCI[[1]])/(results$neyman[[2]]-results$neyman[[1]])
cat("Percentage length gain for SATC: ",round(length.gain*100),"%","\n",sep="")


### plot CI

dp = data.frame(ymax = c(results$sattCI[[2]],
                         results$satcCI[[2]],
                         results$neymanCI[[2]]),
                ymin = c(results$sattCI[[1]],
                         results$satcCI[[1]],
                         results$neymanCI[[1]]),
                parameter = c("SATT","SATC","SATE"),
                ave.diff = rep(mean(y[tr==1])-mean(y[tr==0]),3)
                )

p <- ggplot(dp, aes(x=parameter, y=ave.diff, colour=parameter))+ 
  geom_point(size=3)+
  geom_errorbar(aes(ymin=ymin, ymax=ymax, colour=parameter), size=1, width=.1)+
  labs(
    x = "\n Estimand of interest",
    y = "Average treatment effects \n ( prediction\\confidence interval ) \n"
  )+scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(), 
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))+
  theme( legend.position = "bottom" )

p <- p + guides(col=guide_legend(title=""))

ggsave(file="~/Dropbox/att_CI/figures/rosenbaum2001_CI.pdf", plot = p)
















