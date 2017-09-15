############################################################
# Description: Make figures for the Monte-Carlo simulations
# Date: July 16, 2017
#
#
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)

dirData = "~/Dropbox/att_CI/Rcode/sim_results"
dirFigures = "~/Dropbox/att_CI/figures/"

### Load data
setwd(dirData)
load("binary_gain_sim_July2017.rda")
binary = d
load("random_coefficient_gain_sim_July2017.rda")
coef = d
load("tobit_gain_sim_July2017.rda")
tobit = d
rm(list = ls()[! ls() %in% c("binary","coef","tobit","dirData","dirFigures") ])

############################################################
# Figures - Tobit
############################################################

############################################################
# Gains Figure --- comparison to sharp bound and Neyman
############################################################

dp <- tobit %>%
  group_by(tau) %>%
  summarise(
    gain.satt.ci = mean(gain.satt.ci),
    gain.satt.sharp.ci = mean(gain.satt.sharp.ci),
    gain.sharp.ci = mean(gain.sharp.ci),
    gain.satt.rho.ci = mean(gain.satt.rho.ci),
    #
    size.indicator.satt = 1 - mean(size.indicator.satt),
    size.indicator.sate.in.satt = 1 - mean(size.indicator.sate.in.satt)
  ) %>% ungroup %>% data.frame() %>%
  gather( c(2:3,5), key = "type", value = "gain"  )

dp$type = as.factor(dp$type)
levels(dp$type) = c("Gains from Neyman's", "Gains from true corr" ,"Gains from sharp bound")

### Simulation results - need to improve figure

p <- ggplot(dp, aes(y=gain*100 ,x = tau, col = type, shape = type))+
  geom_point(size=3.5, alpha = 0.6)+
  geom_line(aes(group=type), alpha = 0.7, lty=1)+
  labs(
    x = expression(tau),
    y = "SATT length gains (%) \n"
  )+
  scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=20),
        #
        legend.position = c(0.7,0.15),
        legend.text = element_text(size=12))

p <- p + guides(col=guide_legend(title=""),
                shape=guide_legend(title=""),
                alpha=guide_legend(title=""))

setwd(dirFigures)
ggsave(file="tobit_gain_sim_July2017.pdf", plot = p)


############################################################
# Type-I Figure --- comparison of SATE and SATT cover 
#                      by SATT prediction interval
############################################################

dp <- tobit %>%
  group_by(tau) %>%
  summarise(
    gain.satt.ci = mean(gain.satt.ci),
    gain.satt.sharp.ci = mean(gain.satt.sharp.ci),
    gain.sharp.ci = mean(gain.sharp.ci),
    #
    size.indicator.satt = 1 - mean(size.indicator.satt),
    size.indicator.sate.in.satt = 1 - mean(size.indicator.sate.in.satt)
  ) %>% ungroup %>% data.frame() %>%
  gather( 5:6, key = "estimand", value = "size"  )

dp$estimand = as.factor(dp$estimand)
levels(dp$estimand) = c("Coverage SATE", "Coverage SATT")

### Simulation results - need to improve figure

p <- ggplot(dp, aes(y=size ,x = tau, col = estimand, shape = estimand))+
  geom_point(size=3)+
  geom_line(aes(group=estimand), alpha = 0.7,lty=1)+
  labs(
    x = "SATE",
    y = "Average estimand coverage \n"
  )+
  scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  ylim(0,1)+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        #
        legend.position = c(0.7,0.15),
        legend.text = element_text(size=12))

p <- p + guides(col=guide_legend(title=""),
                shape=guide_legend(title=""),
                alpha=guide_legend(title=""))

#print(p)

setwd(dirFigures)
ggsave(file="tobit_error_sim_July2017.pdf", plot = p)


############################################################
# Figures - Binary
############################################################

############################################################
# Gains Figure --- comparison to sharp bound and Neyman
############################################################

dp <- binary %>%
  group_by(sate) %>%
  summarise(
    gain.satt.ci = mean(gain.satt.ci),
    gain.satt.sharp.ci = mean(gain.satt.sharp.ci),
    gain.sharp.ci = mean(gain.sharp.ci),
    gain.satt.rho.ci = mean(gain.satt.rho.ci),
    #
    size.indicator.satt = 1 - mean(size.indicator.satt),
    size.indicator.sate.in.satt = 1 - mean(size.indicator.sate.in.satt)
  ) %>% ungroup %>% data.frame() %>%
  gather( c(2:3,5), key = "type", value = "gain"  )

dp$type = as.factor(dp$type)
levels(dp$type) = c("Gains from Neyman's", "Gains from true corr" ,"Gains from sharp bound")

### Simulation results 
p <- ggplot(dp, aes(y=gain*100 ,x = sate, col = type, shape = type))+
  geom_point(size=3)+
  geom_line(aes(group=type),alpha = 0.7, lty=1)+
  labs(
    x = "\n SATE",
    y = "SATT length gains (%) \n"
  )+
  scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        #
        legend.position = c(0.70,0.15),
        legend.text = element_text(size=12))

p <- p + guides(col=guide_legend(title=""),
                shape=guide_legend(title=""),
                alpha=guide_legend(title=""))


setwd(dirFigures)
ggsave(file="binary_gain_sim_July2017.pdf", plot = p)

############################################################
# Type-I Figure --- comparison of SATE and SATT cover 
#                      by SATT prediction interval
############################################################

dp <- binary %>%
  group_by(sate) %>%
  summarise(
    gain.satt.ci = mean(gain.satt.ci),
    gain.satt.sharp.ci = mean(gain.satt.sharp.ci),
    gain.sharp.ci = mean(gain.sharp.ci),
    #
    size.indicator.satt = 1 - mean(size.indicator.satt),
    size.indicator.sate.rho = 1 - mean(size.indicator.sate.rho),
    size.indicator.sate.in.satt = 1 - mean(size.indicator.sate.in.satt)
  ) %>% ungroup %>% data.frame() %>%
  gather( c(5,7), key = "estimand", value = "size"  )

dp$estimand = as.factor(dp$estimand)
levels(dp$estimand) = c("Coverage SATE", "Coverage SATT")

cat( mean(binary$size.indicator.sate.rho)  ,"\n ")

### Simulation results - need to improve figure

p <- ggplot(dp, aes(y=size ,x = sate, col = estimand, shape = estimand))+
  geom_point(size=3)+
  geom_line(aes(group=estimand), alpha = 0.7, lty=1)+
  labs(
    x = "\n SATE",
    y = "Average estimand coverage \n"
  )+
  scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  ylim(0,1)+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        #
        legend.position = c(0.7,0.15),
        legend.text = element_text(size=12))

p <- p + guides(col=guide_legend(title=""),
                shape=guide_legend(title=""),
                alpha=guide_legend(title=""))

setwd(dirFigures)
ggsave(file="binary_error_sim_July2017.pdf", plot = p)








############################################################
# Figures - Random coefficient
############################################################


############################################################
# Gains Figure --- comparison to sharp bound and Neyman
############################################################

dp <- coef %>%
  group_by(beta.var) %>%
  summarise(
    gain.satt.ci = mean(gain.satt.ci),
    gain.satt.sharp.ci = mean(gain.satt.sharp.ci),
    gain.sharp.ci = mean(gain.sharp.ci),
    gain.satt.rho.ci = mean(gain.satt.rho.ci),
    #
    size.indicator.satt = 1 - mean(size.indicator.satt),
    size.indicator.sate.in.satt = 1 - mean(size.indicator.sate.in.satt)
  ) %>% ungroup %>% data.frame() %>%
  gather( c(2:3,5), key = "type", value = "gain"  )

dp$type = as.factor(dp$type)
levels(dp$type) = c("Gains from Neyman's", "Gains from true corr" ,"Gains from sharp bound")


p <- ggplot(dp, aes(y=gain*100 ,x = beta.var, col = type, shape = type))+
  geom_point(size=3)+
  geom_line(aes(group=type), alpha = 0.7, lty=1)+
  labs(
    x = "Variance of treatment effect",
    y = "SATT length gains (%) \n"
  )+
  scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        #
        legend.position = c(0.70,0.15),
        legend.text = element_text(size=12))

p <- p + guides(col=guide_legend(title=""),
                shape=guide_legend(title=""),
                alpha=guide_legend(title=""))
setwd(dirFigures)
ggsave(file="random_coefficient_gain_sim_July2017.pdf", plot = p)


############################################################
# Type-I Figure --- comparison of SATE and SATT cover 
#                      by SATT prediction interval
############################################################

dp <- coef %>%
  group_by(beta.var) %>%
  summarise(
    gain.satt.ci = mean(gain.satt.ci),
    gain.satt.sharp.ci = mean(gain.satt.sharp.ci),
    gain.sharp.ci = mean(gain.sharp.ci),
    #
    size.indicator.satt = 1 - mean(size.indicator.satt),
    size.indicator.sate.in.satt = 1 - mean(size.indicator.sate.in.satt)
  ) %>% ungroup %>% data.frame() %>%
  gather( 5:6, key = "estimand", value = "size"  )

dp$estimand = as.factor(dp$estimand)
levels(dp$estimand) = c("Coverage SATE", "Coverage SATT")

### Simulation results - need to improve figure

p <- ggplot(dp, aes(y=size ,x = beta.var, col = estimand, shape = estimand))+
  geom_point(size=3)+
  geom_line(aes(group=estimand), alpha = 0.7, lty=1)+
  labs(
    x = "Variance of treatment effect",
    y = "Average estimand coverage \n"
  )+
  scale_colour_grey( start = 0, end = 0.5 )+
  theme_bw()+
  ylim(0,1)+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14),
        #
        legend.position = c(0.7,0.15),
        legend.text = element_text(size=12))

p <- p + guides(col=guide_legend(title=""),
                shape=guide_legend(title=""),
                alpha=guide_legend(title=""))

#setwd(dirFigures)
ggsave(file="random_coefficient_error_sim_July2017.pdf", plot = p)


