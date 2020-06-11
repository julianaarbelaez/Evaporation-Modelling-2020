## ----------------------------------------------------------------------------------
## Script name: Rentention curve and Hydraulic conductivities
## Purpose of script:
## Author: Juliana Arbelaez Gaviria
## Date Created: 2020-03-12
## Copyright (c) Juliana Arbelaez Gaviria, 2020
## Email: arbelaez@fzp.czu.cz
## -----------------------------------------------------------------------------------
## Notes: Imput parameters Benchmark Case Msc. Thesis
## -----------------------------------------------------------------------------------
library(ggplot2)
library(ggpubr)

#------------Functions----------------------------------------------------------------
#Unsaturated Hydraulic conductivity: van Genuchten and mualem equation
UHC <- function(k_s,alpha,n,h){
  m <- 1-(1/n)
  t <- 1-(-alpha*h)^(n*m)
  p <- (1 + (-alpha*h)^n)
  k<- k_s*(((t*(p^(-m))^2)/p^(m/2)))
}

#Water retentation curve: van Genuchten’s  equation
WRC <- function(alpha,n,theta_s, theta_r,h){
  m <- 1-(1/n)
  c <- theta_r + (theta_s-theta_r)/(1+(-alpha*h)^n)^m
}

# thermal conductivity  [Wm^-1 C°^-1]
THC <- function(b1,b2,b3,theta_l){
  kappa <- b1 + b2*theta_l + b3*theta_l^0.5
}

#-------------Run functions--------------------------------------------------------------------
#pressure head and liquid water content
h <- seq(-1000,0,1)
theta_l <- seq(0,1,0.001)

#Silty Clay
alpha_clay <- 5e-2 #[1/m]
n_clay <- 1.09 #[-]
k_s_clay <- 5.7e-6 #[m/s]
theta_r_clay <- 0.07 #[-]
theta_s_clay <- 0.36 #[-]
b1_clay <- -0.197
b2_clay <- -0.962
b3_clay <- 2.521
wat_clay <- WRC(alpha_clay,n_clay,theta_s_clay, theta_r_clay,-15000)
k_clay <- UHC(k_s_clay,alpha_clay,n_clay,h)
kappa_clay <- THC(b1_clay,b2_clay,b3_clay,theta_l)

#Loam

alpha_loam <- 3e-2 #[1/m]
n_loam <- 1.56 #[-]
k_s_loam <- 2.88e-8 #[m/s]
theta_r_loam <- 0.078 #[-]
theta_s_loam <- 0.43 #[-]
b1_loam <- 0.243
b2_loam <- 0.393
b3_loam <- 1.534
wat_loam<- WRC(alpha_loam,n_loam,theta_s_loam, theta_r_loam,h)
k_loam <- UHC(k_s_loam,alpha_loam,n_loam,h)
kappa_loam <- THC(b1_loam,b2_loam,b3_loam,theta_l)


#---------Plots--------------------------------------------------------------------------------

DATA <- data.frame(h,theta_l,wat_clay,k_clay,kappa_clay,wat_loam,k_loam,kappa_loam )


ggplot(DATA,aes(x=-h, y=wat_clay, linetype = Texture)) +
  geom_line(aes(x=-h, y = wat_clay, linetype = "Silty Clay"))+
  geom_line(aes(x=-h, y = wat_loam, linetype = "Loam"))+
  labs(x = '-pressure head [m]', y ='Liquid Water Content [-]')+
  theme_bw() +
  theme(legend.position = c(0.3, 0.3),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3))+
  scale_x_log10() 

ggplot(DATA,aes(x=-h, y=wat_clay, linetype = Texture)) +
  geom_line(aes(x=-h, y = wat_clay, linetype = "Silty Clay"))+
  labs(x = 'log -pressure head [m]', y ='Liquid Water Content [-]')+
  theme_bw() +
  theme(legend.position = "none")+
  # theme(legend.position = c(0.8, 0.8),
  #       legend.justification = c("right", "bottom"),
  #       legend.box.just = "right",
  #       legend.margin = margin(3, 3, 3, 3))+
  scale_x_log10() 



ggplot(DATA,aes(x=theta_l, y=kappa_clay, linetype = Texture)) +
  geom_line(aes(x=theta_l, y = kappa_clay, linetype = "Silty Clay"))+
  geom_line(aes(x=theta_l, y = kappa_loam, linetype = "Loam"))+
  labs(x = 'liquid water content [-]', y ='thermal conductivity [W/m C°]')+
  theme_bw() +
  theme(legend.position = c(0.3, 0.7),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3))


ggplot(DATA,aes(x=theta_l, y=kappa_clay, linetype = Texture)) +
  geom_line(aes(x=theta_l, y = kappa_clay, linetype = "Silty Clay"))+
  labs(x = 'liquid water content [-]', y ='thermal conductivity [W/m C°]')+
  theme_bw() +
  theme(legend.position = c(0.8, 0.5),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3))

clay <- ggplot(DATA,aes(x=h, y=k_clay)) +
  geom_line()+
  ggtitle("Silty Clay") +
  labs(x = 'pressure head [m]', y ='Hydraulic Conductivity [m/s]')+
  theme_bw()  

ggplot(DATA,aes(x=h, y=k_clay, linetype = Texture)) +
  geom_line(aes(x=h, y = k_clay, linetype = "Silty Clay"))+
  labs(x = 'pressure head [m]', y ='Hydraulic Conductivity [m/s]')+
  theme_bw()+
  theme(legend.position = "none")
  # theme(legend.position = c(0.8, 0.8),
  #       legend.justification = c("right", "bottom"),
  #       legend.box.just = "right",
  #       legend.margin = margin(3, 3, 3, 3))

loam <- ggplot(DATA,aes(x=h, y=k_loam)) +
  geom_line()+
  ggtitle("Loam") +
  labs(x = 'pressure head [m]', y ='Hydraulic Conductivity [m/s]')+
  theme_bw() 
  
ggarrange(clay, loam,
          ncol = 2, nrow = 1)
