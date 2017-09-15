
rm(list = ls())

library(ggplot2)
library(tidyr)
library(dplyr)

install.packages("devtools")
library(devtools)
install_github("yotamshemtov/estCI")
library(estCI)

set.seed(12345)

############################################ 
# Simulation 
############################################

# Parameters
n=1000
num.simulations.finite = 1000
num.draws.super = 1000
sigma0 = 1
beta.var = seq(0,5,by = 0.5)

f.finite.sample = function(k){
  
  ### Generate data
  y0 = rnorm(n,mean=10,sd=sigma0)
  beta = rnorm(n,mean=0,sd=k)
  y1 = beta+y0
  tau = y1-y0
  
  sate = mean((y1-y0))
  rho0 = cor(y1,y0)
  tau = y1-y0
  
  gain.satt.vec <- gain.sharp.vec <- gain.satt.sharp.vec <- gain.satt.rho.vec  <- rep(NA,num.simulations.finite)
  reject.satt.vec <- reject.sharp.vec <- reject.sate.neyman.vec <- reject.wilcox.vec <- reject.ks.vec <- rep(NA,num.simulations.finite) #new
  type1.error.satt.vec <- type1.error.satc.vec <- type1.error.sate.rho.vec <- rep(NA,num.simulations.finite)
  error.sate.in.satt.vec <- error.satt.in.sate.vec <- rep(NA,num.simulations.finite)
  
  for (i in 1:num.simulations.finite){
    
    tr = rep(0,n)
    tr.index = sample(c(1:n),size=n/2,replace=FALSE)
    tr[tr.index]=1
    
    y = tr*y1 + y0*(1-tr)
    
    result = aveCI(y,tr, print=FALSE, rho = rho0 )
    
    length.satt.ci = result$sattCI[[2]]-result$sattCI[[1]]
    length.sharp.ci = result$sharpCI[[2]]-result$sharpCI[[1]]
    length.neyman.ci = result$neymanCI[[2]]-result$neymanCI[[1]]
    length.rho.ci = result$sate.rho$ci.upper.sate.rho-result$sate.rho$ci.lower.sate.rho
    
    satt = mean((y1-y0)[tr==1])
    satc = mean((y1-y0)[tr==0])
    
    type1.error.satt.vec[i] = satt> result$sattCI[[2]] | satt< result$sattCI[[1]]
    type1.error.satc.vec[i] = satc> result$satcCI[[2]] | satc< result$satcCI[[1]]
    type1.error.sate.rho.vec[i] = sate > result$sate.rho$ci.upper.sate.rho | sate < result$sate.rho$ci.lower.sate.rho
    
    gain.satt.vec[i] = 1-length.satt.ci/length.neyman.ci
    gain.sharp.vec[i] = 1-length.sharp.ci/length.neyman.ci
    gain.satt.sharp.vec[i] = 1-length.satt.ci/length.sharp.ci
    gain.satt.rho.vec[i] = 1 - length.satt.ci/length.rho.ci
    
    error.sate.in.satt.vec[i] = sate> result$sattCI[[2]] | sate< result$sattCI[[1]]
    error.satt.in.sate.vec[i] = satt > result$neymanCI[[2]] | satt< result$neymanCI[[1]]
    
    # Rejection vectors
    reject.satt.vec[i] <- as.numeric(result$sattCI$tstat.satt>1.96)
    reject.sharp.vec[i] <- as.numeric(result$sharpCI$tstat.sharp>1.96)
    reject.sate.neyman.vec[i] <-  as.numeric(result$sharpCI$tstat.sharp>1.96)
    reject.wilcox.vec[i] <-  as.numeric(wilcox.test(y[tr==1],y[tr==0])$p.value)
    reject.ks.vec[i] <-  as.numeric(ks.test(y[tr==1],y[tr==0])$p.value)
  }
  
  r = data.frame(
    type1.error.satt.ave = mean(type1.error.satt.vec),
    type1.error.satc.ave = mean(type1.error.satc.vec),
    type1.error.sate.rho.ave = mean(type1.error.sate.rho.vec),
    gain.satt.ave = mean(gain.satt.vec),
    gain.sharp.ave = mean(gain.sharp.vec),
    gain.satt.sharp.ave = mean(gain.satt.sharp.vec),
    gain.satt.rho.ave = mean(gain.satt.rho.vec),
    error.sate.in.satt.ave = mean(error.sate.in.satt.vec),
    error.satt.in.sate.ave = mean(error.satt.in.sate.vec),
    var.tau = var(tau),
    #
    reject.satt.vec = mean(reject.satt.vec),
    reject.sharp.vec = mean(reject.sharp.vec),
    reject.sate.neyman.vec = mean(reject.sate.neyman.vec),
    reject.wilcox.vec = mean(reject.wilcox.vec),
    reject.ks.vec = mean(reject.ks.vec)
  )
  
  return(r)
}

# check
print(f.finite.sample(1.5))

len.par.vec = length(beta.var)
d = data.frame(
  gain.satt.ci = rep(NA,num.draws.super * len.par.vec),
  gain.satt.sharp.ci =  rep(NA,num.draws.super * len.par.vec),
  gain.sharp.ci = rep(NA,num.draws.super * len.par.vec),
  gain.satt.rho.ci = rep(NA,num.draws.super * len.par.vec),
  beta.var = rep(beta.var,each= num.draws.super),
  size.indicator.satt = rep(NA,num.draws.super * len.par.vec ),
  size.indicator.sate.in.satt = rep(NA,num.draws.super * len.par.vec ),
  size.indicator.satc = rep(NA,num.draws.super * len.par.vec ),
  size.indicator.satt.in.sate = rep(NA,num.draws.super * len.par.vec ),
  size.indicator.sate.rho = rep(NA,num.draws.super * len.par.vec),
  #
  reject.satt = rep(NA,num.draws.super * len.par.vec),
  reject.sharp = rep(NA,num.draws.super * len.par.vec),
  reject.sate.neyman = rep(NA,num.draws.super * len.par.vec),
  reject.wilcox = rep(NA,num.draws.super * len.par.vec),
  reject.ks = rep(NA,num.draws.super * len.par.vec)
)
dim(d)

### Simulation 
for (i in c(1:length(beta.var))){
  for (s in c(1:num.draws.super)){
    if(s%%10==0){cat("Iteration: ",s,"\n")}
    
    result = f.finite.sample(beta.var[i])
    #
    d$gain.satt.ci[(i-1)*num.draws.super + s] = result$gain.satt.ave
    d$gain.sharp.ci[(i-1)*num.draws.super + s] = result$gain.sharp.ave
    d$gain.satt.sharp.ci[(i-1)*num.draws.super + s] = result$gain.satt.sharp.ave
    d$gain.satt.rho.ci[(i-1)*num.draws.super + s] = result$gain.satt.rho.ave
    #
    d$size.indicator.satt[(i-1)*num.draws.super + s] = result$type1.error.satt.ave
    d$size.indicator.satc[(i-1)*num.draws.super + s] = result$type1.error.satc.ave
    d$size.indicator.sate.in.satt[(i-1)*num.draws.super + s] = result$error.sate.in.satt.ave
    d$size.indicator.satt.in.sate[(i-1)*num.draws.super + s] = result$error.satt.in.sate.ave
    d$size.indicator.sate.rho[(i-1)*num.draws.super + s] = result$type1.error.sate.rho.ave
    #
    d$reject.satt[(i-1)*num.draws.super + s] = result$reject.satt.vec
    d$reject.sharp[(i-1)*num.draws.super + s] = result$reject.sharp.vec
    d$reject.sate.neyman[(i-1)*num.draws.super + s] = result$reject.sate.neyman.vec
    d$reject.wilcox[(i-1)*num.draws.super + s] = result$reject.wilcox.vec
    d$reject.ks[(i-1)*num.draws.super + s] = result$reject.ks.vec
  }
}

d$beta.var = factor(d$beta.var)

save.image(file = "random_coefficient_gain_sim_July2017.rda")






