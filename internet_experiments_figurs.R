
rm(list=ls())

library(ggplot2)
library(ggthemes)
library(estCI)
library(dplyr)
library(tidyr)

dirFigures = "~/Dropbox/att_CI/figures"

d = read.csv("~/Dropbox/att_CI/data_sets_potential/internet/SampleExperiments.csv")
d = distinct(d)
glimpse(d)

d$n = d$cnt_l_nonnull + d$cnt_r_nonnull
length(unique(d$n))
#d = d[!duplicated(d$n),]


d[,"att.gain"] = NA
d[,"atc.gain"] = NA

d[,"atc.len"] = NA
d[,"att.len"] = NA
d[,"ate.len"] = NA

d[,"atc.tstat"] = NA
d[,"att.tstat"] = NA
d[,"ate.tstat"] = NA
d[,"shortest.tstat"] = NA

d[,"atc.reject.null"] = NA
d[,"att.reject.null"] = NA
d[,"ate.reject.null"] = NA

### Length gains
f.exp = function(x){
  r = aveCI( 
    mu1 = x[,"mu_r_nonnull"],
    mu0 = x[,"mu_l_nonnull"],
    var1 = x[,"var_r_nonnull"],
    var0 = x[,"var_l_nonnull"],
    n = x[,"n"],
    m = x[,"cnt_r_nonnull"],
    Sharp.CI = FALSE,
    stats.only = TRUE,
    print = FALSE)
  
  atc.len = abs(r$satcCI[[1]]-r$satcCI[[2]])
  atc.reject.null = r$satcCI$ci.lower.satc > 0 | r$satcCI$ci.upper.satc < 0
  atc.tstat = r$satcCI$tstat.satc
  
  att.len = abs(r$sattCI[[1]]-r$sattCI[[2]])
  att.reject.null = r$sattCI$ci.lower.satt > 0 | r$sattCI$ci.upper.satt < 0
  att.tstat = r$sattCI$tstat.satt
  
  ate.len = abs(r$neymanCI[[1]]-r$neymanCI[[2]])
  ate.reject.null = r$neymanCI$ci.lower.neyman > 0 | r$neymanCI$ci.upper.neyman < 0
  ate.tstat = r$neymanCI$tstat.neyman
  
  shortest.tstat = r$shortestCI$tstat.shortest
  
  rr = data_frame( 
    att.gain = 1-att.len/ate.len,
    atc.gain = 1-atc.len/ate.len,
    atc.len,
    atc.reject.null,
    att.len,
    att.reject.null,
    ate.len,
    ate.reject.null,
    shortest.tstat,
    atc.tstat,
    att.tstat,
    ate.tstat
  )
  return(rr)
}

for (i in c(1:dim(d)[1]) ){
  if( i %% 1000 == 0 ){ cat("Iteration: ", i, "\n") }
  
  if( sum(is.na(d[i, !colnames(d) %in% c( "att.gain","atc.gain","atc.len","atc.reject.null", "att.len", "att.reject.null", "ate.len", "ate.reject.null", "shortest.tstat","atc.tstat", "att.tstat","ate.tstat" ) ])) > 0 ){
    r = rep(NA, 8)
  } else {
    r = f.exp( d[i,] )
    d[ i , c( "att.gain","atc.gain","atc.len","atc.reject.null", "att.len", "att.reject.null", "ate.len", "ate.reject.null",
              "shortest.tstat","atc.tstat", "att.tstat","ate.tstat") ] = r
  }
  
}

### Keep only experiments without missing values
dp = d %>% filter( complete.cases(.) )
cat(dim(dp), "\n")
cat(dim(d), "\n")

# Define diff-in-means and variance ratio
dp$td = dp$mu_r_nonnull - dp$mu_l_nonnull 
dp$var.ratio = dp$var_r_nonnull/dp$var_l_nonnull


### Remove a few very extreme outliers
# print( outlier <- quantile( dp$var.ratio, prob = .99, na.rm = TRUE ) )
# cat( sum( dp$var.ratio > outlier ) ,"\n")

print( outlier1 <- quantile( abs(dp$td), prob = .99, na.rm = TRUE ) )
cat( sum( abs(dp$td) > outlier1 ) ,"\n")

print( outlier2 <- quantile( abs(dp$var.ratio), prob = .99, na.rm = TRUE ) )
cat( sum( abs(dp$var.ratio) > outlier2 ) ,"\n")

dim(dp)
dp = filter( dp, (abs(td) <= outlier1 | abs(var.ratio) <= outlier2) & !is.na(var.ratio) & att.gain < 500  )
dim(dp)

### One more filtering
# bin things into groups:
summary(dp$var.ratio)
table(is.na(dp$var.ratio))
dp$var.ratio.g = cut(dp$var.ratio, breaks = as.numeric(quantile( dp$var.ratio, prob = seq(0.01, 0.99, length = 200), na.rm = TRUE )) )
table(is.na(dp$var.ratio.g))
dp = filter(dp, !is.na(var.ratio.g) )
table(is.na(dp$var.ratio.g))

### Raw distribution of the difference-in-means:

p = ggplot(select(dp,td), aes(x = td) )+
  xlim(-.2,.2)+
  geom_density()+
  labs(
    x="Difference-in-means",
    y = "Density",
    title = "")+
  geom_vline( xintercept  = 0, col = "grey10", lty=2 )+
  theme_bw()+ 
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(axis.text.x = element_text(colour="grey20",size=13))+
  theme(axis.text.y = element_text(colour="grey20",size=13))+
  theme(axis.title.y = element_text(colour="grey20",size=13))+
  theme(axis.title.x = element_text(colour="grey20",size=13))+
  theme(panel.border = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8))

ggsave(p, file = "~/Dropbox/att_CI/figures/internet_experiments_diff_means_dist.pdf", width = 5, height=4)


#########################################
# Variance gains/difference figures
#########################################


#########################################
# Figure 1 (efficiency/variance gains)
#########################################

dp0 = group_by(dp,  var.ratio.g) %>%
  summarise(
    att.gain.ave = mean(att.gain)*100,
    var.ratio.ave = mean(var.ratio)
  ) %>% ungroup()

p = ggplot( dp0, aes( x = var.ratio.ave , y = att.gain.ave ) )+
  geom_point(size=2, alpha=0.4)+geom_line()+
  geom_vline( xintercept  = 1, col = "grey10", lty=2 )+
  labs(
    x=expression(paste("\n \n Variance ratio (",sigma[1]^2/sigma[0]^2,")")),
    y = "Difference in CI/PI length (%) \n",
    title = "")+
  xlim(0.1,2.5)+
  theme_bw()+ 
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(axis.text.x = element_text(colour="grey20",size=13))+
  theme(axis.text.y = element_text(colour="grey20",size=13))+
  theme(axis.title.y = element_text(colour="grey20",size=13))+
  theme(axis.title.x = element_text(colour="grey20",size=13))+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color="black", size = 0.8))+
  theme(title = element_text(size=15))
ggsave(p, file = "~/Dropbox/att_CI/figures/internet_experiments_gains_variance_ratio.pdf", width = 5, height=4)


#########################################
# Figure 2 (T-stat)
#########################################

dp0 = group_by(dp,  var.ratio.g) %>%
  summarise(
    ate.reject.ave = mean( abs(ate.tstat) > 1.96, an.rm = TRUE ),
    ato.reject.ave = mean( abs(shortest.tstat) > 1.96, an.rm = TRUE ),
    var.ratio.ave = mean(var.ratio)
  ) %>% ungroup() %>%
  select(-var.ratio.g) %>%
  gather( key = estimand, value = rejection.rate ,-var.ratio.ave )

dp0$estimand = as.factor(dp0$estimand)
levels(dp0$estimand) = c("SATE", "SATO")

p = ggplot( dp0, aes( x = var.ratio.ave , y = rejection.rate, color = estimand, lty = estimand ) )+
  geom_smooth(se = FALSE)+
  labs(
    x=expression(paste("\n \n Variance ratio (",sigma[1]^2/sigma[0]^2,")")),
    y = "Rejection of the null (%) \n",
    title = "")+
  scale_colour_grey( start = 0, end = 0.5 )+
  xlim(0.1,2.5)+
  theme_bw()+ 
  theme( panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(axis.text.x = element_text(colour="grey20",size=15))+
  theme(axis.text.y = element_text(colour="grey20",size=15))+
  theme(axis.title.y = element_text(colour="grey20",size=16))+
  theme(axis.title.x = element_text(colour="grey20",size=16))+
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color="black", size = 0.8))+
  theme(legend.position = c(0.75,0.8), 
        legend.text = element_text(size=16))+ # legend positvjust=0.5ion
  geom_vline(xintercept = 1, col = "black", lty = 4)+
  guides(col = guide_legend(title=""), lty = guide_legend(title=""))

ggsave(p, file = "~/Dropbox/att_CI/figures/rejection_rate_unique_rows.pdf", width = 5, height=4)
















