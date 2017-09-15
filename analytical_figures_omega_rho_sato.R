
###################################################
# Analytical figures
###################################################

rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)

# NEW omega calculations
f.omega = function(ratio, rho){
  omega = (ratio^2 - rho * ratio)/(ratio^2 + 1 - 2 * rho * ratio)
  omega = max(0,min(1,omega))
  return(omega)
}

f.data = function(rho.vec, ratio.vec){
  d = data.frame(
    rho = rho.vec[1],
    omega = sapply( ratio.vec,  function(x){ return( f.omega(ratio = x, rho = rho.vec[1] ) ) } ),
    ratio = ratio.vec
  )
  
  for (i in rho.vec[-1] ){
    tmp = data.frame(
      rho = i,
      omega = sapply( ratio.vec,  function(x){ return( f.omega(ratio = x , rho = i ) ) } ),
      ratio = ratio.vec
    )
    
    d = rbind(d,tmp)
    
  }
  
  d$rho = as.factor(d$rho)
  return( d )
}

rho.vec = c(-1,0,1)
ratio.vec = seq(0,2,by=0.05)

d = f.data(rho.vec = rho.vec, ratio.vec = ratio.vec)

p = ggplot(d, aes( x = ratio, y = omega, col = rho, shape = rho, lty = rho  ) )+
  geom_point(size=3, alpha = 0.6)+
  geom_line()+
  labs(
    x = expression(paste("Variance ratio (",sigma[1]^2/sigma[0]^2,")")),
    y = "Omega ( weight on SATT ) \n"
  )+
  theme_bw()+
  theme(axis.text.x = element_text(colour="grey20",size=14))+
  theme(axis.text.y = element_text(colour="grey20",size=14))+
  theme(axis.title.y = element_text(colour="grey20",size=16))+
  theme(axis.title.x = element_text(colour="grey20",size=16))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(title = element_text(size=12))+
  scale_colour_grey( start = 0, end = 0.5 )+
  guides(col=guide_legend(title="cor(Y(1),Y(0)): "),shape=guide_legend(title="cor(Y(1),Y(0)): "),
         lty=guide_legend(title="cor(Y(1),Y(0)): "))+ # adding legend title
  theme(legend.position = c(0.8,0.3), 
        legend.text = element_text(size=14))+ # legend positvjust=0.5ion
  geom_vline(xintercept = 1, col = "black", lty = 2)

ggsave(p, file = "~/Dropbox/att_CI/figures/sato_omega_rho.pdf", width = 6, height = 4)


###################################################
# Comparison of SATO and SATE when sigma1=sigma0
###################################################

### Figure 1

f.ratio = function(omega, rho, sigma=1, p = 0.3, n = 100){
  var0 <- var1 <- sigma^2
  k.n.m = 1/(p*(1-p)*n)
  sigma0 = sqrt(var0)
  sigma1 = sqrt(var1)
  var.tau = var1 + var0 - 2 * sigma0 * sigma1 * rho
  const = 1/( p * (1-p) * n )
  element1 = const * (  var0*p^2 + var1*(1-p)^2 + 2 * p * (1-p) * rho * sigma0 * sigma1  )
  element2 = ( var.tau/n ) * (  omega^2 *  ( (1-p)/p ) + (1-omega)^2 * ( p/(1-p) ) -2 * omega*(1-omega)  )
  element3 = (-2/n) * (  (omega/p ) * ( var1 - rho * sigma1 * sigma0 ) -  var.tau  + ((1-omega)/(1-p) ) * ( var0 - rho * sigma0 * sigma1 )    )
  
  tmp = element1+element2+element3
  if (tmp < 0.1^10){
    tmp = 0
  }
  sd.td.sato = sqrt( tmp )
  
  sd.rho = sqrt( k.n.m * ( p^2*var0 + (1-p)^2*var1 + 2*p*(1-p)*rho*sqrt(var0)*sqrt(var1) ) ) 
  ratio =  sd.td.sato/sd.rho
  r = list(
    sd.td.sato = sd.td.sato,
    sd.rho = sd.rho,
    ratio = ratio,
    p = p,
    rho = rho
  )
  return(ratio)
}

f.ratio(omega=0.5, rho = -1, n= 100)
f.omega(ratio=1, rho = -1)

f.data = function(rho.vec, omega.vec){
  d = data.frame(
    rho = rho.vec[1],
    ratio = sapply( omega.vec,  function(x){ return( f.ratio(omega = x, rho = rho.vec[1] ) ) } ),
    omega = omega.vec
  )
  
  for (i in rho.vec[-1] ){
    tmp = data.frame(
      rho = i,
      ratio = sapply( omega.vec,  function(x){ return( f.ratio(omega = x , rho = i ) ) } ),
      omega = omega.vec
    )
    
    d = rbind(d,tmp)
    
  }
  
  d$rho = as.factor(d$rho)
  return( d )
}

rho.vec = c(-1,0,1)
omega.vec = seq(0,1,by=0.05)

d = f.data(rho.vec = rho.vec, omega.vec = omega.vec)
d$gain.percent = 1 - d$ratio

p = ggplot(d, aes( y = gain.percent, x = omega, col = rho, shape = rho, lty = rho  ) )+
  geom_point(size=3, alpha = 0.6)+
  geom_line()+
  labs(
    y = "Accuracy gain (%)",
    x = "Omega (weight on SATT) \n"
  )+
  theme_bw()+
  theme(axis.text.x = element_text(colour="grey20",size=14))+
  theme(axis.text.y = element_text(colour="grey20",size=14))+
  theme(axis.title.y = element_text(colour="grey20",size=16))+
  theme(axis.title.x = element_text(colour="grey20",size=16))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(title = element_text(size=12))+
  scale_colour_grey( start = 0, end = 0.5 )+
  guides(col=guide_legend(title="cor(Y(1),Y(0)): "),shape=guide_legend(title="cor(Y(1),Y(0)): "),
         lty=guide_legend(title="cor(Y(1),Y(0)): "))+ # adding legend title
  theme(legend.position = c(0.85,0.85), 
        legend.text = element_text(size=14))+ # legend positvjust=0.5ion
  geom_vline(xintercept = 0.3, col = "black", lty = 2,lwd=1)+
  geom_vline(xintercept = 0.5, col = "black", lty = 3,lwd=1)

p = p + annotate("segment", x = 0.7, xend = 0.51, y = -1, yend = -0.6, colour="grey20", size=1, arrow=arrow())+
  annotate("text", x = 0.7, y = -1.07, label = "SATO weight", colour="grey20", size=5)+
  annotate("segment", x = 0.1, xend = 0.29, y = 0.7, yend = 0.4, colour="grey20", size=1, arrow=arrow())+
  annotate("text", x = 0.1, y = 0.77, label = "SATE weight", colour="grey20", size=5)


ggsave(p, file = "~/Dropbox/att_CI/figures/gain_sato_rho1.pdf", width = 5, height = 4)

### Figure 2

f.ratio = function( rho, sigma=1, n = 100, p ){
  omega = f.omega(ratio=1, rho)
  
  var0 <- var1 <- sigma^2
  k.n.m = 1/(p*(1-p)*n)
  sigma0 = sqrt(var0)
  sigma1 = sqrt(var1)
  var.tau = var1 + var0 - 2 * sigma0 * sigma1 * rho
  const = 1/( p * (1-p) * n )
  element1 = const * (  var0*p^2 + var1*(1-p)^2 + 2 * p * (1-p) * rho * sigma0 * sigma1  )
  element2 = ( var.tau/n ) * (  omega^2 *  ( (1-p)/p ) + (1-omega)^2 * ( p/(1-p) ) -2 * omega*(1-omega)  )
  element3 = (-2/n) * (  (omega/p ) * ( var1 - rho * sigma1 * sigma0 ) -  var.tau  + ((1-omega)/(1-p) ) * ( var0 - rho * sigma0 * sigma1 )    )
  
  tmp = element1+element2+element3
  if (tmp < 0.1^10){
    tmp = 0
  }
  sd.td.sato = sqrt( tmp )
  
  sd.rho = sqrt( k.n.m * ( p^2*var0 + (1-p)^2*var1 + 2*p*(1-p)*rho*sqrt(var0)*sqrt(var1) ) ) 
  ratio =  sd.td.sato/sd.rho
  r = list(
    sd.td.sato = sd.td.sato,
    sd.rho = sd.rho,
    ratio = ratio,
    p = p,
    rho = rho
  )
  return(ratio)
}

f.ratio( rho = -0.8, p = 0.5)

f.data = function(rho.vec, p.vec){
  d = data.frame(
    p = p.vec[1],
    ratio = sapply( rho.vec,  function(x){ return( f.ratio(rho = x, p = p.vec[1] ) ) } ),
    rho = rho.vec
  )
  
  for (i in p.vec[-1] ){
    tmp = data.frame(
      p = i,
      ratio = sapply( rho.vec,  function(x){ return( f.ratio(rho = x , p = i ) ) } ),
      rho = rho.vec
    )
    
    d = rbind(d,tmp)
    
  }
  
  d$p = as.factor(d$p)
  return( d )
}

rho.vec = seq(-0.95,.95,by=0.1)
p.vec = c(0.5,0.7,0.9)

d = f.data(rho.vec = rho.vec, p.vec = p.vec)
d$gain.percent = 1 - d$ratio

p = ggplot(d, aes( y = gain.percent, x = rho, col = p , shape = p, lty = p  ) )+
  geom_point(size=3, alpha = 0.6)+
  geom_line()+
  labs(
    y = "Accuracy gain (%)",
    x = "cor(Y(1),Y(0)) \n"
  )+
  theme_bw()+
  theme(axis.text.x = element_text(colour="grey20",size=14))+
  theme(axis.text.y = element_text(colour="grey20",size=14))+
  theme(axis.title.y = element_text(colour="grey20",size=16))+
  theme(axis.title.x = element_text(colour="grey20",size=16))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme(title = element_text(size=12))+
  scale_colour_grey( start = 0, end = 0.5 )+
  guides(col=guide_legend(title="Pr(T=1): "),shape=guide_legend(title="Pr(T=1): "),
         lty=guide_legend(title="Pr(T=1): "))+ # adding legend title
  theme(legend.position = c(0.85,0.85), 
        legend.text = element_text(size=14)) # legend positvjust=0.5ion
  
p = p+annotate("text", x = 0.3  , y = 0.5, label = "Optimal weight is always 0.5", size=5)
  

ggsave(p, file = "~/Dropbox/att_CI/figures/gain_sato_rho2.pdf", width = 5, height = 4)














































