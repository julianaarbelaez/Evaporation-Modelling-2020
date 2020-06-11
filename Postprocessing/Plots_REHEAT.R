## ---------------------------------------------------------------------------------------------------------------
## Script name: plots results RE + Heat
## Purpose of script:
## Author: Juliana Arbelaez Gaviria
## Date Created: 2020-03-27
## Copyright (c) Juliana Arbelaez Gaviria, 2020
## Email: arbelaez@fzp.czu.cz
## ----------------------------------------------------------------------------------------------------------------
## Notes:Plotting results of Msc for water model coupled with heat
## -----------------------------------------------------------------------------------------------------------------
setwd("~/ownCloud/Thesis /Benchmark Case")
library(ggplot2)
library(ggpubr)
library(data.table)
library(viridis)
library(stringr)
cols <-viridis_pal(option = "D")(17)
#-------------------------------------------------------------------------------------------------------------------
#Function provided by J.R Blocher
clean_seb <- function(filename){
  dta <- read.table(filename, skip = 6)
  dta_new <- as.data.frame(matrix(ncol = ncol(dta), nrow = length(unique(dta$V1))))
  k <- 1
  for(rows in 1:(nrow(dta)-1)){
    t_old <- dta[rows,1]
    t_new <- dta[rows+1,1]
    
    if(t_new == t_old){
      
    }else{
      dta_new[k,] <- dta[rows,]
      k <- k+1
    }
    if(rows == (nrow(dta)-1)){
      if(t_new == t_old){
        dta_new[k,] <- dta[rows+1,]
      }else{
        dta_new[k,] <- dta[rows+1,]
      }
    }
  }
  return(dta_new)
}
#------------------------------------------------------------------------------------------------------------------
#--------------------------DARK SCENARIO---------------------------------------------------------------------------
#ID : CS#D_variable SEB: Surface Energy Balance 
#Case of study 1 (CS_dark_30) Rs=0, ra=30 s/m
#Case of study 2 (CS_dark_100) Rs=0, ra=100 s/m
#Case of study 3 (CS_dark_300) Rs=0, ra=300 s/m

#------------------Reading files------------------------------------------------------------------------------------

#Preasure Head
hpress2 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra100/', 
                                   pattern = 'RE_matrix_press_head-', full.names = TRUE),
                        numeric = TRUE)
hpress2 <- hpress2[1:17]

CS02D_hpress2 <- lapply(hpress2,
                     read.delim, 
                     sep = "", 
                     header = FALSE,  
                     col.names = c('node', 'x-coord','Pressure Head'))

CS02D_hpress2 <- rbindlist(l = CS02D_hpress2,
                        idcol = 'Day')

#Temperature
temp2 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra100/', 
                               pattern = 'heat_temperature-', full.names = TRUE),
                    numeric = TRUE)
temp2 <- temp2[1:17]
CS02D_temp2 <- lapply(temp2,
                       read.delim, 
                       sep = "", 
                       header = FALSE,  
                       col.names = c('node', 'x-coord','Temperature'))

CS02D_temp2 <- rbindlist(l = CS02D_temp2,
                          idcol = 'Day')


#Water content
theta2 <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra100/', 
                                   pattern = 'RE_matrix_theta-', full.names = TRUE), 
                        numeric = TRUE)

CS02D_theta <- lapply(theta2,
                     read.delim, 
                     sep = "", 
                     header = FALSE,  
                     col.names = c('node', 'x-coord','theta'))

CS02D_theta <- rbindlist(l = CS02D_theta,
                        idcol = 'Day')


#Observation point in the boundary #6
#Heat 
CS01Dobspt_T <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra30/obspt_heat-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'Temperature','heat flux','cum. flux')) 

#Pressure
CS01Dobspt_h <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra30/obspt_RE_matrix-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'h','theta', 'darcy flux','cum. flux'))

#Heat 
CS02Dobspt_T <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra100/obspt_heat-6.out',
                       skip = 10,
                       header = FALSE, 
                       dec = ".",
                       sep = "",
                       col.names = c('Time', 'Temperature','heat flux','cum. flux')) 

#Pressure
CS02Dobspt_h <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra100/obspt_RE_matrix-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'h','theta', 'darcy flux','cum. flux')) 
#Heat 
CS03Dobspt_T <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra300/obspt_heat-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'Temperature','heat flux','cum. flux')) 

#Pressure
CS03Dobspt_h <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra300/obspt_RE_matrix-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'h','theta', 'darcy flux','cum. flux'))
#Surface Energy Balance

#ra=30
file_SEBD_30 <- '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra30/surface_energy.out'
SEBD_30 <- clean_seb(file_SEBD_30)
names(SEBD_30) <- c("time","Rad", "Hs", "LE","E", "G")

#ra=100
file_SEBD_100 <- '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra100/surface_energy.out'
SEBD_100 <- clean_seb(file_SEBD_100)
names(SEBD_100 ) <- c("time","Rad", "Hs", "LE","E", "G")

#ra=300
file_SEBD_300 <-'~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra300/surface_energy.out'
SEBD_300 <- clean_seb(file_SEBD_300)
names(SEBD_300) <- c("time","Rad", "Hs", "LE","E", "G")

#--------------------------Constant Rad SCENARIO---------------------------------------------------------------------------
#ID : CS#S_variable SEB: Surface Energy Balance 
#Case of study 1 (CS_sun_30) Rs=0, ra=30 s/m
#Case of study 2 (CS_sun_100) Rs=0, ra=100 s/m
#Case of study 3 (CS_sun_300) Rs=0, ra=300 s/m

#------------------Reading files------------------------------------------------------------------------------------

#Preasure Head
hpress2s <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra100/', 
                               pattern = 'RE_matrix_press_head-', full.names = TRUE),
                    numeric = TRUE)
hpress2s <- hpress2s[1:17]
CS02S_hpress2 <- lapply(hpress2s,
                        read.delim, 
                        sep = "", 
                        header = FALSE,  
                        col.names = c('node', 'x-coord','Pressure Head'))

CS02S_hpress2 <- rbindlist(l = CS02S_hpress2,
                           idcol = 'Day')

#Temperature
temp2s <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra100/', 
                             pattern = 'heat_temperature-', full.names = TRUE),
                  numeric = TRUE)
temp2s <- temp2s[1:17] 
CS02S_temp2 <- lapply(temp2s,
                      read.delim, 
                      sep = "", 
                      header = FALSE,  
                      col.names = c('node', 'x-coord','Temperature'))

CS02S_temp2 <- rbindlist(l = CS02S_temp2,
                         idcol = 'Day')


#Water content
theta2s <- str_sort(list.files(path = '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra100/', 
                              pattern = 'RE_matrix_theta-', full.names = TRUE), 
                   numeric = TRUE)

CS02S_theta <- lapply(theta2s,
                      read.delim, 
                      sep = "", 
                      header = FALSE,  
                      col.names = c('node', 'x-coord','theta'))

CS02S_theta <- rbindlist(l = CS02S_theta,
                         idcol = 'Day')


#Observation point in the boundary #6

#Heat 
CS02Sobspt_T <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra100/obspt_heat-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'Temperature','heat flux','cum. flux')) 

#Pressure
CS02Sobspt_h <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra100/obspt_RE_matrix-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'h','theta', 'darcy flux','cum. flux')) 

#Heat 
CS01Sobspt_T <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra30/obspt_heat-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'Temperature','heat flux','cum. flux')) 

#Pressure
CS01Sobspt_h <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Dark_ra30/obspt_RE_matrix-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'h','theta', 'darcy flux','cum. flux')) 

#Heat 
CS03Sobspt_T <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra300/obspt_heat-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'Temperature','heat flux','cum. flux')) 

#Pressure
CS03Sobspt_h <- read.delim(file =  '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra300/obspt_RE_matrix-6.out',
                           skip = 10,
                           header = FALSE, 
                           dec = ".",
                           sep = "",
                           col.names = c('Time', 'h','theta', 'darcy flux','cum. flux'))


#Surface Energy Balance

#ra=30
file_SEBS_30 <- '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra30/surface_energy.out'
SEBS_30 <- clean_seb(file_SEBS_30)
names(SEBS_30) <- c("time","Rad", "Hs", "LE","E", "G")

#ra=100
file_SEBS_100 <- '~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra100/surface_energy.out'
SEBS_100 <- clean_seb(file_SEBS_100)
names(SEBS_100) <- c("time","Rad", "Hs", "LE","E", "G")

#ra=300
file_SEBS_300 <-'~/ownCloud/Thesis /Benchmark Case/Results /Rad_ra300/surface_energy.out'
SEBS_300 <- clean_seb(file_SEBS_300)
names(SEBS_300) <- c("time","Rad", "Hs", "LE","E", "G")

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


cum_evap_100D <- cum_evap(SEBD_100$time/86400,SEBD_100$E*86400*100)
cum_evap_100D <- cum_evap_100D[2:length(cum_evap_100D)]
cum_evap_30D <- cum_evap(SEBD_30$time/86400,SEBD_30$E*86400*100)
cum_evap_30D <- cum_evap_30D[2:length(cum_evap_30D)]
cum_evap_300D <- cum_evap(SEBD_300$time/86400,SEBD_300$E*86400*100)
cum_evap_300D <- cum_evap_300D[2:length(cum_evap_300D)]


cum_evap_100S <- cum_evap(SEBS_100$time/86400,SEBS_100$E*86400*100)
cum_evap_100S <- cum_evap_100S[2:length(cum_evap_100S)]
cum_evap_30S <- cum_evap(SEBS_30$time/86400,SEBS_30$E*86400*100)
cum_evap_30S <- cum_evap_30S[2:length(cum_evap_30S)]
cum_evap_300S <- cum_evap(SEBS_300$time/86400,SEBS_300$E*86400*100)
cum_evap_300S <- cum_evap_300S[2:length(cum_evap_300S)]

#Soil Relative Humidity

rel_hum<- function(h,Temp){
  Temp <- Temp + 273.15
  relhum <- rep(NA,length.out = length(h))
  for (i in 1:length(h)) {
    relhum[i] = exp((h[i]*0.018015*9.81)/(8.314*Temp[i]))
  }
  return(relhum)
}

vap_density <- function(T){
  T <- T + 273.15
  vapdensity <- rep(NA,length.out = length(T))
  for (i in 1:length(T)){
    vapdensity[i] <- 1e-3*exp(31.37 - (6014.79/T[i]) - 7.92e-3*T[i])/(T[i])
  }
  return(vapdensity)
}

RH_D30 <- rel_hum(CS01Dobspt_h$h,CS01Dobspt_T$Temperature)
RH_D100 <-rel_hum(CS02Dobspt_h$h,CS02Dobspt_T$Temperature)
RH_D300 <-rel_hum(CS03Dobspt_h$h,CS03Dobspt_T$Temperature)

RH_S100 <-rel_hum(CS02Sobspt_h$h,CS02Sobspt_T$Temperature)
RH_S30 <-rel_hum(CS01Sobspt_h$h,CS01Sobspt_T$Temperature)
RH_S300 <-rel_hum(CS03Sobspt_h$h,CS03Sobspt_T$Temperature)

sat_D30 <- vap_density(CS01Dobspt_T$Temperature)*RH_D30
sat_D100 <-vap_density(CS02Dobspt_T$Temperature)*RH_D100
sat_D300 <-vap_density(CS03Dobspt_T$Temperature)*RH_D300

sat_S100 <-vap_density(CS02Sobspt_T$Temperature)*RH_S100
sat_S30 <-vap_density(CS01Sobspt_T$Temperature)*RH_S30
sat_S300 <-vap_density(CS03Sobspt_T$Temperature)*RH_S300


#----------------PLOTS------------------------------------------------------------------------------------------


##

DEN1 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
  geom_line(data = CS01Dobspt_T, aes(x= Time/86400, y = sat_D30,  color = '30 s/m'))+
  geom_line(data = CS02Dobspt_T, aes(x=Time/86400, y = sat_D100, color = '100 s/m'))+
  geom_line(data = CS03Dobspt_T, aes(x=Time/86400, y = sat_D300,color = '300 s/m'))+
  labs(x = 'time [days]', y ='vapor density [kg/m3]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
  theme_bw()+
  ggtitle('no Rs')+
  theme(legend.position = c(0.95, 0.5),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3),
        axis.title.x=element_blank(),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

DEN2 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
  geom_line(data = CS01Sobspt_T, aes(x= Time/86400, y = sat_S30,  color = '30 s/m'))+
  geom_line(data = CS02Sobspt_T, aes(x=Time/86400, y = sat_S100, color = '100 s/m'))+
  geom_line(data = CS03Sobspt_T, aes(x=Time/86400, y = sat_S300,color = '300 s/m'))+
  labs(x = 'time [days]', y ='vapor density [kg/m3]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
  theme_bw()+
  xlim(0,14)+
  ggtitle('with Rs')+
  theme(legend.position = 'none',
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

RH1 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
  geom_line(data = CS01Dobspt_T, aes(x= Time/86400, y = RH_D30,  color = '30 s/m'))+
  geom_line(data = CS02Dobspt_T, aes(x=Time/86400, y = RH_D100, color = '100 s/m'))+
  geom_line(data = CS03Dobspt_T, aes(x=Time/86400, y = RH_D300,color = '300 s/m'))+
  labs(x = 'time [days]', y ='relative humidity [-]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
  theme_bw()+
  theme(legend.position = "none")

RH2 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
  geom_line(data = CS01Sobspt_h, aes(x= Time/86400, y = RH_S30,  color = '30 s/m'))+
  geom_line(data = CS02Sobspt_h, aes(x=Time/86400, y = RH_S100, color = '100 s/m'))+
  geom_line(data = CS03Sobspt_h, aes(x=Time/86400, y = RH_S300,color = '300 s/m'))+
  labs(x = 'time [days]', y ='relative humidity [-]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title.y=element_blank())



## DARK ESCENARIO


cume1 <- ggplot(NULL,aes(x=time/86400 , y=E*86400*100, color = Air_resistance)) +
  geom_line(data = SEBD_30, aes(x= time/86400, y = cum_evap_30D,  color = '30 s/m'))+
  geom_line(data = SEBD_100, aes(x=time/86400, y = cum_evap_100D, color = '100 s/m'))+
  geom_line(data = SEBD_300, aes(x=time/86400, y = cum_evap_300D ,color = '300 s/m'))+
  labs(x = 'time [days]', y ='cumulative evaporation flux [cm]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
  theme_bw()+
  theme(legend.position = c(0.9, 0.3),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

cume2 <- ggplot(NULL,aes(x=time/86400 , y=E*86400*100, color = Air_resistance)) +
  geom_line(data = SEBS_30, aes(x= time/86400, y = cum_evap_30S,  color = '30 s/m'))+
  geom_line(data = SEBS_100, aes(x=time/86400, y = cum_evap_100S, color = '100 s/m'))+
  geom_line(data = SEBS_300, aes(x=time/86400, y = cum_evap_300S,color = '300 s/m'))+
  labs(x = 'time [days]', y ='cumulative evaporation flux [cm]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
  theme_bw()+
  theme(legend.position = 'none',
        axis.title.y=element_blank())




#Evaporation rate dark


h1_all <- ggplot(data = CS02D_hpress2) + 
  geom_line(mapping = aes( x = Pressure.Head,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'pressure head [m]', y ='Depth [m]')+
  theme_bw()+
  ggtitle("no Rs")+
  theme(legend.position = "none")


theta1_all <- ggplot(data = CS02D_theta) + 
  geom_line(mapping = aes( x = theta,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'water content [-]', y ='Depth [m]')+
  theme_bw()+
  # ggtitle("no Rs")
  theme(legend.position = "none")


T1_all <- ggplot(data = CS02D_temp2) + 
  geom_line(mapping = aes( x = Temperature,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'temperature [°C]', y ='Depth [m]')+
  theme_bw()+
  # ggtitle("no Rs")+
  theme(legend.position = "none")


e1 <- ggplot(NULL,aes(x=time/86400 , y=E*86400*100, color = Air_resistance)) +
      geom_line(data = SEBD_30, aes(x= time/86400, y = E*86400*100,  color = '30 s/m'))+
      geom_line(data = SEBD_100, aes(x=time/86400, y = E*86400*100, color = '100 s/m'))+
      geom_line(data = SEBD_300, aes(x=time/86400, y = E*86400*100,color = '300 s/m'))+
      labs(x = 'time [days]', y ='evaporation rate [cm/d]')+
      scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
      theme_bw()+
      ylim(0,2)+
      xlim(0,17)+
      ggtitle('No Rs')+
      theme(legend.position = c(0.9, 0.3),
            legend.justification = c("right", "bottom"),
            legend.box.just = "right",
            legend.margin = margin(3, 3, 3, 3),
            axis.title.x=element_blank())

#temperature

T1 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
      geom_line(data = CS01Dobspt_T, aes(x= Time/86400, y = Temperature,  color = '30 s/m'))+
    geom_line(data = CS02Dobspt_T, aes(x=Time/86400, y = Temperature, color = '100 s/m'))+
    geom_line(data = CS03Dobspt_T, aes(x=Time/86400, y = Temperature,color = '300 s/m'))+
    geom_line(data = CS03Dobspt_T, aes(x=Time/86400, y = 25, color = 'Air Temperature'))+
    labs(x = 'time [days]', y ='Temperature [°C]')+
    scale_color_manual(values=c(cols[1],cols[5],cols[15], 'black'))+
    theme_bw()+
    ylim(17.5,28)+
    xlim(0,17)+
    #ggtitle('No Rs')+
    theme(legend.position = "none")
    # theme(legend.position = c(0.98, 0.01),
    #       legend.justification = c("right", "bottom"),
    #       legend.box.just = "right",
    #       legend.margin = margin(3, 3, 3, 3))

#pressure head

h1 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
      geom_line(data = CS01Dobspt_h, aes(x= Time/86400, y = h,  color = '30 s/m'))+
      geom_line(data = CS02Dobspt_h, aes(x=Time/86400, y = h, color = '100 s/m'))+
      geom_line(data = CS03Dobspt_h, aes(x=Time/86400, y = h,color = '300 s/m'))+
      labs(x = 'time [days]', y ='Pressure Head [m]')+
      scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
      theme_bw()+
      ylim(-1000,0)+
      xlim(0,17)+
      ggtitle('No Rs')+
      theme(legend.position = "none",
            axis.title.x=element_blank())
      # theme(legend.position = c(0.9, 0.5),
      #       legend.justification = c("right", "bottom"),
      #       legend.box.just = "right",
      #       legend.margin = margin(3, 3, 3, 3),
      #       axis.title.x=element_blank())

#theta
theta1 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
          geom_line(data = CS01Dobspt_h, aes(x= Time/86400, y = theta,  color = '30 s/m'))+
          geom_line(data = CS02Dobspt_h, aes(x=Time/86400, y = theta, color = '100 s/m'))+
          geom_line(data = CS03Dobspt_h, aes(x=Time/86400, y = theta,color = '300 s/m'))+
          labs(x = 'time [days]', y ='water content [-]')+
          scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
          theme_bw()+
          ylim(0.23,0.36)+
          xlim(0,17)+
          #ggtitle('No Rs')+
          theme(legend.position = "none")
          # theme(legend.position = c(0.9, 0.7),
          #       legend.justification = c("right", "bottom"),
          #       legend.box.just = "right",
          #       legend.margin = margin(3, 3, 3, 3))




## CONST. RADIATION ESCENARIO

h2_all <- ggplot(data = CS02S_hpress2) + 
  geom_line(mapping = aes( x = Pressure.Head,
                           y = x.coord,
                           colour = factor(Day)))+
  scale_color_manual(values = cols, name = "Obs. Time")+
  labs(x = 'pressure head [m]', y ='Depth [m]')+
  theme_bw()+
  ggtitle("with Rs")+
  theme(legend.position = "none",
        axis.title.y=element_blank())


theta2_all <- ggplot(data = CS02S_theta) + 
              geom_line(mapping = aes( x = theta,
                           y = x.coord,
                           colour = factor(Day)))+
              scale_color_manual(values = cols, name = "Obs. Time")+
              labs(x = 'water content [-]', y ='Depth [m]')+
              theme_bw()+
              theme(axis.title.y=element_blank(),
                    legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.box = "horizontal")+
              guides(colour = guide_legend(nrow = 1))
              
             

T2_all <- ggplot(data = CS02S_temp2) + 
            geom_line(mapping = aes( x = Temperature,
                           y = x.coord,
                           colour = factor(Day)))+
            scale_color_manual(values = cols, name = "Obs. Time")+
            labs(x = 'temperature [°C]', y ='Depth [m]')+
            theme_bw()+
            theme(legend.position = "none",
                  axis.title.y=element_blank())
            #ggtitle("with Rs")



#Evaporation rate 

e2 <- ggplot(NULL,aes(x=time/86400 , y=E*86400*100, color = Air_resistance)) +
      geom_line(data = SEBS_30, aes(x= time/86400, y = E*86400*100,  color = '30 s/m'))+
      geom_line(data = SEBS_100, aes(x=time/86400, y = E*86400*100, color = '100 s/m'))+
      geom_line(data = SEBS_300, aes(x=time/86400, y = E*86400*100,color = '300 s/m'))+
      labs(x = 'time [days]', y ='evaporation rate [cm/d]')+
      scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
      theme_bw()+
      ylim(0,2)+
      xlim(0,17)+
      ggtitle('with Rs')+
      theme(legend.position = "none",
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
      # theme(legend.position = c(0.9, 0.6),
      #       legend.justification = c("right", "bottom"),
      #       legend.box.just = "right",
      #       legend.margin = margin(3, 3, 3, 3))

#temperature

T2 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
      geom_line(data = CS01Sobspt_T, aes(x= Time/86400, y = Temperature,  color = '30 s/m'))+
      geom_line(data = CS02Sobspt_T, aes(x=Time/86400, y = Temperature, color = '100 s/m'))+
      geom_line(data = CS03Sobspt_T, aes(x=Time/86400, y = Temperature,color = '300 s/m'))+
      geom_line(data = CS03Sobspt_T, aes(x=Time/86400, y = 25, color = 'Air Temperature'))+
      labs(x = 'time [days]', y ='Temperature [°C]')+
      scale_color_manual(values=c(cols[1],cols[5],cols[15], 'black'))+
      theme_bw()+
      ylim(17.5,32)+
      xlim(0,17)+
      theme(legend.position = "none",
            axis.title.y=element_blank())
      # theme(legend.position = c(0.9, 0.01),
      #       legend.justification = c("right", "bottom"),
      #       legend.box.just = "right",
      #       legend.margin = margin(3, 3, 3, 3))

#pressure head

h2 <- ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
      geom_line(data = CS01Sobspt_h, aes(x= Time/86400, y = h,  color = '30 s/m'))+
      geom_line(data = CS02Sobspt_h, aes(x=Time/86400, y = h, color = '100 s/m'))+
      geom_line(data = CS03Sobspt_h, aes(x=Time/86400, y = h,color = '300 s/m'))+
      labs(x = 'time [days]', y ='Pressure Head [m]')+
      scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
      theme_bw()+
      ylim(-1000,0)+
      xlim(0,17)+
      ggtitle('with Rs')+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position = c(0.9, 0.3),
            legend.justification = c("right", "bottom"),
            legend.box.just = "right",
            legend.margin = margin(3, 3, 3, 3))

#theta
theta2 <-ggplot(NULL,aes(x=Time/86400 , y=Temperature, color = Air_resistance)) +
        geom_line(data = CS01Sobspt_h, aes(x= Time/86400, y = theta,  color = '30 s/m'))+
        geom_line(data = CS02Sobspt_h, aes(x=Time/86400, y = theta, color = '100 s/m'))+
        geom_line(data = CS03Sobspt_h, aes(x=Time/86400, y = theta,color = '300 s/m'))+
        labs(x = 'time [days]', y ='water content [-]')+
        scale_color_manual(values=c(cols[1],cols[5],cols[15]))+
        theme_bw()+
        ylim(0.23,0.36)+
        xlim(0,17)+
        #ggtitle('with Rs')+
        theme(legend.position = "none",
              axis.title.y=element_blank())
        # theme(legend.position = c(0.9, 0.7),
        #       legend.justification = c("right", "bottom"),
        #       legend.box.just = "right",
        #       legend.margin = margin(3, 3, 3, 3))


SEB2 <- ggplot(SEBS_100,aes(x=time/86400 , y= G, color = Fluxes)) +
  geom_line(data = SEBS_100, aes(x= time/86400, y = G,  color = 'Soil Heat'))+
  geom_line(data = SEBS_100, aes(x=time/86400, y = Rad, color = 'Net Radiation'))+
  geom_line(data = SEBS_100, aes(x=time/86400, y = Hs ,color = 'Sensible Heat'))+
  geom_line(data = SEBS_100, aes(x=time/86400, y = LE ,color = 'Latent Heat'))+
  labs(x = 'time [days]', y ='Energy flux [W/m2]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[8],cols[15]))+
  theme_bw()+
  ggtitle('with Rs')+
  theme(legend.position = 'none',
        axis.title.y=element_blank(),
        axis.title.x=element_blank())

SEB1 <- ggplot(SEBD_100,aes(x=time/86400 , y= G, color = Fluxes)) +
  geom_line(data = SEBD_100, aes(x= time/86400, y = G,  color = 'Soil Heat'))+
  geom_line(data = SEBD_100, aes(x=time/86400, y = Rad, color = 'Net Radiation'))+
  geom_line(data = SEBD_100, aes(x=time/86400, y = Hs ,color = 'Sensible Heat'))+
  geom_line(data = SEBD_100, aes(x=time/86400, y = LE ,color = 'Latent Heat'))+
  labs(x = 'time [days]', y ='Energy flux [W/m2]')+
  scale_color_manual(values=c(cols[1],cols[5],cols[8],cols[15]))+
  theme_bw()+
  ggtitle('no Rs')+
  theme(legend.position = c(0.9, 0.55),
        legend.justification = c("right", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        axis.title.x=element_blank())




#-------- For exporting the plots-----------------------------------------------------------
ggarrange(e1, e2, T1, T2,
          ncol = 2, nrow = 2)


ggarrange(h1, h2, theta1, theta2,
          ncol = 2, nrow = 2)


ggarrange(h1_all, h2_all, theta1_all, theta2_all,T1_all, T2_all,
          ncol = 2, nrow = 3,
          legend = "right", 
          common.legend = TRUE)

ggarrange(SEB1, SEB2, cume1,cume2,
          ncol = 2, nrow = 2)
  
ggarrange(DEN1, DEN2, RH1,RH2,
          ncol = 2, nrow = 2)
