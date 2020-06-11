## ------------------------------------------------------------------------------------
## Script name: plots results RE + PM
## Purpose of script:
## Author: Juliana Arbelaez Gaviria
## Date Created: 2020-03-25
## Copyright (c) Juliana Arbelaez Gaviria, 2020
## Email: arbelaez@fzp.czu.cz
## ------------------------------------------------------------------------------------
## Notes: Plotting results of Msc for model PM + RE
## ------------------------------------------------------------------------------------
setwd("~/ownCloud/Thesis /Benchmark Case")
library(ggplot2)
library(ggpubr)
library(data.table)
library(viridis)
library(stringr)

cols <-viridis_pal(option = "D")(14)

#-----------------------Read Data Files -----------------------------------------------
#Silty Clay 
#------------------------Case Study Rn=0----------------------------------------------

obspt_BC <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /CS01_SC/obspt_RE_matrix-6.out',
                       skip = 10,
                        header = FALSE, 
                        dec = ".",
                        sep = "",
                        col.names = c('Time', 'h','theta', 'darcy flux','cum. flux')) 

#---------------------------------------------------------------------------------------------------------------

temp_press1 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /CS01_SC/', 
                          pattern = 'RE_matrix_press_head-', full.names = TRUE),
                       numeric = TRUE)
temp_press1 <- temp_press1[1:14]
CS01_press <- lapply(temp_press1,
                     read.delim, 
                     sep = "", 
                     header = FALSE,  
                     col.names = c('node', 'x-coord','Pressure Head'))

CS01_press <- rbindlist(l = CS01_press,
                        idcol = 'Day')
#---------------------------------------------------------------------------------------------------------------------

temp_theta1 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /CS01_SC/', 
                          pattern = 'RE_matrix_theta-', full.names = TRUE), 
                        numeric = TRUE)

CS01_theta <- lapply(temp_theta1,
                     read.delim, 
                     sep = "", 
                     header = FALSE,  
                     col.names = c('node', 'x-coord','theta'))

CS01_theta <- rbindlist(l = CS01_theta,
                        idcol = 'Day')

#------------------------------------------------------------------------------------------------------

CS01_evap <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /CS01_SC/evap.rate',
                      header = FALSE, 
                      dec = ".",
                      sep = "",
                      colClasses = 'numeric',
                      col.names = c('Time', 'Evap rate','Heat Flux', 'Net Radiation')) 

CS01_evap$Time <- CS01_evap$Time/86400

#-----------------------------------------------------------------------------------------------------------------------

#Plots No solar radiation
p1 <- ggplot(data = CS01_press) + 
  geom_line(mapping = aes( x = Pressure.Head,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'pressure head [m]', y ='Depth [m]')+
  ggtitle("No Rs")+
  theme_bw()+
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))
  

  
p2 <- ggplot(data = CS01_theta) + 
  geom_line(mapping = aes( x = theta,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'water content [-]', y ='Depth [m]')+
  #ggtitle("No Rs")+
  theme_bw()+
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))

#------------------------Case Study Rn=constant--------------------------------------------------------------------


obspt2_BC <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /CS02_SC/obspt_RE_matrix-6.out',
                       skip = 10,
                       header = FALSE, 
                       dec = ".",
                       sep = "",
                       col.names = c('Time', 'h','theta', 'darcy flux','cum. flux')) 

#------------------------------------------------------------------------------------------------------------------
temp_press2 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /CS02_SC/', 
                          pattern = 'RE_matrix_press_head-', full.names = TRUE), 
                        numeric = TRUE)
temp_press2 <- temp_press2[1:11]
CS02_press <- lapply(temp_press2,
                     read.delim, 
                     sep = "", 
                     header = FALSE,  
                     col.names = c('node', 'x-coord','Pressure Head'))

CS02_press <- rbindlist(l = CS02_press,
                        idcol = 'Day')
#-------------------------------------------------------------------------------------------------------------

temp_theta2 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /CS02_SC/', 
                          pattern = 'RE_matrix_theta-', full.names = TRUE),
                        numeric = TRUE)

CS02_theta <- lapply(temp_theta2,
                     read.delim, 
                     sep = "", 
                     header = FALSE,  
                     col.names = c('node', 'x-coord','theta'))

CS02_theta <- rbindlist(l = CS02_theta,
                        idcol = 'Day')

#-------------------------------------------------------------------------------------------------------------------

CS02_evap <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /CS02_SC/evap.rate',
                        header = FALSE, 
                        dec = ".",
                        sep = "",
                        colClasses = 'numeric',
                        col.names = c('Time', 'Evap rate','Heat Flux', 'Net Radiation')) 

CS02_evap$Time <- CS02_evap$Time/86400

#-------------------------------------------------------------------------------------------------------------------

#Plots with solar radiation
p3 <- ggplot(data = CS02_press) + 
  geom_line(mapping = aes( x = Pressure.Head,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'pressure head [m]', y ='Depth [m]')+
  theme_bw()+
  ggtitle("with Rs")+
  theme(axis.title.y=element_blank(),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))



p4 <- ggplot(data = CS02_theta) + 
  geom_line(mapping = aes( x = theta,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'water content [-]', y ='Depth [m]')+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        legend.title = element_text(size = 5),
        legend.text = element_text(size = 5))

  #ggtitle("with Rs")


ggarrange(p1, p3,p2,p4,
          ncol = 2, nrow = 2,
          common.legend = TRUE,
          legend = 'right')

#------------------------Plots -------------------------------------------------------------

#Surface boundary observation point

evap_D <- cum_evap(CS01_evap$Time,CS01_evap$Evap.rate)
evap_D <- evap_D[2:length(evap_D)]
evap_S <- cum_evap(CS02_evap$Time,CS02_evap$Evap.rate)
evap_S <- evap_S[2:length(evap_S)]


ggplot(NULL,aes(x= CS01_evap$Time, y= evap_D, color = Radiation)) +
  geom_line(aes(x=CS01_evap$Time, y= evap_D/10, color = 'no Rs'))+
  geom_line(aes(x=CS02_evap$Time, y = evap_S/10, color = 'with Rs'))+
  labs(x = 'time [days]', y ='cumulative evaporation [cm]')+
  scale_color_manual(values=c(cols[1],cols[5]))+
  theme_bw()+
  ylim(0,6)+
  xlim(0,14)+
  theme(legend.position = c(0.3, 0.75),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3))



plot(CS01_evap$Time, evap_D/10)
lines(CS02_evap$Time, evap_S/10)

obspt_BC$Time <- obspt_BC$Time/86400
obspt2_BC$Time <- obspt2_BC$Time/86400

DATA_BC <- data.table(obspt_BC$Time,
                      obspt_BC$h, 
                      obspt_BC$theta,
                      obspt2_BC$h, 
                      obspt2_BC$theta)

colnames(DATA_BC) <- c('time', 
                       'h_1',
                       'theta_1',
                       'h_2',
                       'theta_2')

p5 <- ggplot(DATA_BC,aes(x= DATA_BC$time, y= DATA_BC$h_1, color = Radiation)) +
  geom_line(aes(x=DATA_BC$time, y= DATA_BC$h_1, color = 'no Rs'))+
  geom_line(aes(x=DATA_BC$time, y = DATA_BC$h_2, color = 'with Rs'))+
  labs(x = 'Time [days]', y ='pressure head [m]')+
  scale_color_manual(values=c(cols[1],cols[5]))+
  theme_bw()+
  ylim(-170,0)+
  xlim(0,12)+
  theme(legend.position = c(0.9, 0.75),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3))
 

p6<- ggplot(DATA_BC,aes(x= DATA_BC$time, y= DATA_BC$theta_1, color = Radiation)) +
  geom_line(aes(x=DATA_BC$time, y= DATA_BC$theta_1, color = 'no Rs'))+
  geom_line(aes(x=DATA_BC$time, y = DATA_BC$theta_2, color = 'with Rs'))+
  labs(x = 'Time [days]', y ='water content [-]')+
  scale_color_manual(values=c(cols[1],cols[5]))+
  theme_bw()+
  ylim(0.15,0.38)+
  xlim(0,11.9)+
  theme(legend.position = 'none')


#Cumulative evaporation

cum_evap <- function(t,E){
  Ecum <-  0
  for (i in 1:length(E)){
    if (i==1){
      Ecum[i+1] <- Ecum[i] + E[i]*t[i]}
    else{
      Ecum[i+1] <- Ecum[i] + E[i]*(t[i]-t[i-1])}
  }
  return(Ecum)
}

ggarrange(p5,p6,
          ncol = 2, nrow = 1)
