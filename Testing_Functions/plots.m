% ------------------------------------------------------------------------
% Script name: Functions for evaporation Module in DRUTES
% Purpose of script: Debugging the Evaporation module in Drutes
% Author: Juliana Arbelaez Gaviria
% Date Created: 2020-01-14
% Copyright (c) Juliana Arbelaez Gaviria, 2020
% Email: arbelaez@fzp.czu.cz
% ------------------------------------------------------------------------
% Notes:Debugging the Evaporation Module of Drutes
% ------------------------------------------------------------------------
%%Auxiliar functions of the Model

%Relative Humidity [-]
%temperature[K]
T = linspace(273.15, 373.15);
h = linspace(-100,0);
[T,h] = meshgrid(T,h);
MolWat = 0.018615;
gravity = 9.18;
R_gas = 8.314;
rh_fnc = @(T,h) exp ((h.*MolWat.*gravity)./(R_gas.*T));
rh = rh_fnc(T,h);
mesh(T, h, rh)

%%
%Saturated vapor density [kg/m^3]
%temp [°C]
%temp = linspace(0,100);
temp = 25;
T = temp + 273.15;
rho_sv_fnc = @(T) 1e-3.*(exp(31.3716 - (6014.79./T) - 7.92495e-3.*T))./T;
rho_sv = rho_sv_fnc(T)
plot(temp, rho_sv,'k');

%%
%Derivative Saturated vapor density [kg/m^3 K]
%temp [°C]
temp = linspace(1,100);
T = temp + 273.15;
drho_sv_dT_fnc = @(T) exp(- 7.92495e-3.*T - (6014.79./T)).*((-3.33818e8.*T - 4.2122e10).*T + 2.53357e14).*(1./T.^3);
drho_sv_dT = drho_sv_dT_fnc(T);
plot(temp, drho_sv_dT,'k')
%%
%Water liquid density[kg/m^3]
%temp [°C]
T = linspace(1,100);
rho_l_fnc = @(T) (1000.0 - 7.37e-3*(T - 4.0).^2 + 3.79e-5*(T -4.0).^3);
rho_l = rho_l_fnc(T);
plot(T, rho_l,'k')
%%
%Specific Latent heat  [Jkg^-1]
%temp [°C]
temp = linspace(1,100);
latent_heat_fnc = @(temp) 2.501e-6 - 2369.2*temp;
latent_heat = latent_heat_fnc(temp);
plot(temp, latent_heat, 'k')
%%
%Surface tension [g/s^2]
%temp [°C]
temp = linspace(1,100);
surf_tension_soilwat = 75.6 - 0.1425*temp - 2.38e-4*temp.^2;
plot(temp, surf_tension_soilwat,'k')
%%
%Derivative of Surface tension [g/s^2 ºC]
%temp [°C]
temp = linspace(1,100);
dsurf_tension_soilwat_dT_fnc = @(temp) (- 0.1425 - 4.76e-4*temp);
dsurf_tension_soilwat_dT = dsurf_tension_soilwat_dT_fnc(temp);
plot(temp,dsurf_tension_soilwat_dT,'k')
%%
%thermal conductivity  [Wm^-1 C°^-1]]
%theta_l [-]
b1 = 0.228;
b2 = -2.406;
b3 = 4.909;
theta_l = linspace(0,1);
thermal_conduc  = @(theta_l) b1 + b2*theta_l + b3*theta_l.^0.5;
kappa = thermal_conduc(theta_l);
plot(theta_l, kappa,'k')
%%
%Vapor Difussivity in soil  [m^2/s]
%theta_l [-]
theta_l = linspace(0,1);
temp = linspace(1,100);
T = temp + 273.15;
theta_air = 1 - theta_l;
%theta_sat = 0.5;
tor = tortuosity(theta_air);
vapor_diff_air = vapor_diff_air_fnc(T);
vapor_diff_soil = tor.*theta_air.*vapor_diff_air;
plot(theta_l,vapor_diff_soil,'k')
%%
%Tortuosity factor in gaseous phase [-]
%theta_l [-]
theta_l = linspace(0,1);
theta_air = 1 - theta_l;
theta_sat = 0.5;
tortuosity = @(theta_air) ((theta_air).^(7/3))./ (theta_sat.^2);
tor = tortuosity(theta_air);
plot(theta_l,tor,'k')
%%
%Vapor Difussivity in air  [m^2/s]
%temp [°C]
temp = linspace(1,100);
T = temp + 273.15;
vapor_diff_air_fnc =  @(T) (2.12e-5 * (T./273.15).^2);
vapor_diff_air = vapor_diff_air_fnc(T);
plot(theta_l,vapor_diff_air,'k')
%%
%Enhacement Factor [-]
%theta_l [-]
theta_l = linspace(0,1);
theta_sat = 0.5;
f_c = 0.02;
const = 1 + (2.6/sqrt(f_c));
tmp = exp(-(const * (theta_l./theta_sat)).^4);
enhancement_factor_fnc  = @(theta_l) (9.5 + 3.0*(theta_l./theta_sat) -8.5.*tmp);
enhancement_factor = enhancement_factor_fnc(theta_l);
plot(theta_l,enhancement_factor,'k')
%%
% Water vapor content [-]
theta_l = linspace(0,1);
temp = linspace(0,100);
T = temp + 273.15;
rho_sv = rho_sv_fnc(T) ;
h = linspace(-100,0);
rho_l = rho_l_fnc(temp) ;
rh_soil = rh_fnc(T,h);
theta_vapor = (1 - theta_l).*rho_sv.*rh_soil.*(1.0./rho_l);
plot(temp,theta_vapor,'k')
%%
%Isothermal vapor hydarulic conductivity
temp = linspace(0,100);
T = temp + 273.15;
theta_l = linspace(0,1);
h = linspace(-100,0);
theta_air = 1 - theta_l;
[T,h] = meshgrid(T,h);
rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
rho_l = rho_l_fnc(temp);
vapor_diff_air = vapor_diff_air_fnc(T);
vapor_diff_soil = tor.*theta_air.*vapor_diff_air;
hydraulic_vh = (vapor_diff_soil./rho_l).*rho_sv.*((MolWat.*gravity)./(R_gas.*T)).*rh_soil;
mesh(temp,h,hydraulic_vh)
%%
%Thermal vapor hydarulic conductivity
temp = 25;
T = temp + 273.15;
theta_l = 0.4;
h= -0.15;
theta_air = 1 - theta_l;

rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
rho_l = rho_l_fnc(temp);
factor = enhancement_factor_fnc(theta_l);
drho_sv_dT = drho_sv_dT_fnc(T);
hydraulic_vT = (vapor_diff_soil./rho_l).*factor.*drho_sv_dT.*rh_soil
%mesh(temp,h,hydraulic_vT)
%%
% Nonthermal liquid hydraulic conductivity
h = linspace(-100,0);
n = 2.73; 
m = 0.63;  
a = 1e-1;
k_sat =  7.128;
hydraulic_lh_fnc = @(h) k_sat.*(1 -(-(a.*h)).^(m*n)./(1 + (-(a.*h)).^n).^m).^2./(1 + (-(a.*h)).^n).^(m/2);
hydraulic_lh = hydraulic_lh_fnc(h);
plot(h,hydraulic_lh)
%%
% Thermal liquid hydraulic conductivity
h = linspace(-100,0);
temp = linspace(1,100);
[temp,h] = meshgrid(temp,h);
dsurf_tension_soilwat_dT = dsurf_tension_soilwat_dT_fnc(temp);
GwT = 7;
gamma_0 = 71.89;
hydraulic_lh = hydraulic_lh_fnc(h);
hydraulic_lT = hydraulic_lh.*h.*GwT.*(1/gamma_0)*dsurf_tension_soilwat_dT; 
mesh(temp,h,hydraulic_lT)
%%
%Derivative of Nonthermal liquid hydraulic conductivity
h = linspace(-100,0);
n = 2.73; 
m = 0.63;  
a = 1e-1;
k_sat =  7.128;
dmualem_dh_fnc = @(h) k_sat.*(((a.*(-(a.*h)).^(-1 + n).*(1 + (-(a.*h)).^n).^(-1 - m./2.0).*(1 - (-(a.*h)).^(m.*n)/(1 + ...
(-(a.*h)).^n).^m).^2*m*n)./2.0 + ...
(2.*(1 - (-(a.*h)).^(m*n)./(1 + (-(a.*h)).^n).^m).* ...
(-(a.*(-(a.*h)).^(-1 + n + m.*n).*(1 + (-(a.*h)).^n).^(-1 - m).*m.*n) + ...
(a*(-(a.*h)).^(-1 + m*n)*m*n)/(1 + (-(a.*h)).^n).^m))./ ...
(1 + (-(a.*h)).^n).^(m./2.0)));
dmualem_dh = dmualem_dh_fnc(h);
plot(h,dmualem_dh)
%%
%convection_T 
h = linspace(-100,0);
n = 2.73; 
m = 0.63;  
a = 1e-1;
k_sat =  7.128;
temp = linspace(1,100);
rho_l = rho_l_fnc(temp);
C_liq = 4188;
dmualem_dh = dmualem_dh_fnc(h);
convection_T = C_liq.*rho_l.*temp.*dmualem_dh;
plot(temp, convection_T)
%%
%difussion_hT
h = linspace(-100,0);
temp = linspace(1,100);
T = temp + 273.15;
[temp,h] = meshgrid(temp,h);
dsurf_tension_soilwat_dT = dsurf_tension_soilwat_dT_fnc(temp);
hydraulic_lh = hydraulic_lh_fnc(h);
hydraulic_lT = hydraulic_lh.*h.*GwT.*(1/gamma_0)*dsurf_tension_soilwat_dT;
rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
rho_l = rho_l_fnc(temp);
factor = enhancement_factor_fnc(theta_l);
drho_sv_dT = drho_sv_dT_fnc(T);
hydraulic_vT = (vapor_diff_soil./rho_l).*factor.*drho_sv_dT.*rh_soil;
difussion_hT = hydraulic_lT + hydraulic_vT;
mesh(temp,h,difussion_hT)
%%
%difussion_hh
h = linspace(-100,0);
hydraulic_lh = hydraulic_lh_fnc(h);
theta_air = 1 - theta_l;
temp = linspace(1,100);
T = temp + 273.15;
[temp,h] = meshgrid(temp,h);
rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
rho_l = rho_l_fnc(temp);
vapor_diff_air = vapor_diff_air_fnc(T);
vapor_diff_soil = tor.*theta_air.*vapor_diff_air;
hydraulic_vh = (vapor_diff_soil./rho_l).*rho_sv.*((MolWat.*gravity)./(R_gas.*T)).*rh_soil;
difussion_hh = hydraulic_lh + hydraulic_vh;
mesh(temp,h,difussion_hh)
%%
% capacity_T
C_liq = 4188; 
C_vap = 1800;
C_soil =  1920;
rho_soil = 2650;
h = linspace(-100,0);
temp = linspace(1,100);
T = temp + 273.15;
rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
rho_l = rho_l_fnc(temp);
capacity_T = rho_l.*C_liq + rho_sv.*rh_soil.*C_vap + rho_soil*C_soil;
[temp,h] = meshgrid(temp,h);
mesh(temp,h,capacity_T)
%%
%difussion_TT
matrix = ones(100);
theta_l = linspace(0,1);
kappa = thermal_conduc(theta_l);
C_liq = 4188; 
C_vap = 1800;
h = linspace(-100,0);
temp = linspace(1,100);
T = temp + 273.15;
rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
rho_l = rho_l_fnc(temp);
drho_sv_dT = drho_sv_dT_fnc(T);
vapor_diff_air = vapor_diff_air_fnc(T);
vapor_diff_soil = tor.*theta_air.*vapor_diff_air;
factor = enhancement_factor_fnc(theta_l);
dsurf_tension_soilwat_dT = dsurf_tension_soilwat_dT_fnc(temp);
GwT = 7;
gamma_0 = 71.89;
hydraulic_lh = hydraulic_lh_fnc(h);
hydraulic_lT = hydraulic_lh.*h.*GwT.*(1/gamma_0).*dsurf_tension_soilwat_dT;
Latent = latent_heat_fnc(temp);
hydraulic_vT = (vapor_diff_soil./rho_l).*factor.*drho_sv_dT.*rh_soil;
difussion_TT = kappa.*matrix + C_vap.*rho_sv.*rh_soil.*T.*hydraulic_vT + ...
    C_liq.*rho_l.*T.*hydraulic_lT + Latent.*rho_l.*hydraulic_vT;
[temp,h] = meshgrid(temp,h);
mesh(temp,h,difussion_TT)
%%
%difussion_Th
C_liq = 4188; 
C_vap = 1800;
temp = linspace(1,100);
h = linspace(-100,0);
T = temp + 273.15;
[temp,h] = meshgrid(temp,h);
rho_l = rho_l_fnc(temp);
rh_soil = rh_fnc(T,h);
rho_sv = rho_sv_fnc(T);
Latent = latent_heat_fnc(temp);
vapor_diff_air = vapor_diff_air_fnc(T);
vapor_diff_soil = tor.*theta_air.*vapor_diff_air;
hydraulic_lh = hydraulic_lh_fnc(h);
hydraulic_vh = (vapor_diff_soil./rho_l).*rho_sv.*((MolWat.*gravity)./(R_gas.*T)).*rh_soil;
difussion_Th = rho_l.*C_liq.*T.*hydraulic_lh + ...
rho_sv.*rh_soil.*C_vap.*T.*hydraulic_vh +  hydraulic_vh.*Latent.*rho_l;
% plot(temp,difussion_Th)
mesh(temp,h,difussion_Th)
%%
%Capacity_h
theta_r = 0.0625; 
theta_sat = 0.5; 
h = linspace(-100,0);
n = 2.73; 
m = 0.63;  
a = 1e-1;
Capacity_h = a*m*n*(-theta_r + theta_sat).*(-(a.*h)).^(-1 + n).*(1 + (-(a.*h)).^n).^(-1 - m);
plot(h,Capacity_h)
%% 
%Vapor pressure
temp = linspace(1,100);
e = @(temp) 0.6108.*exp(17.27*temp./(temp + 237.3));
vapor_pressure = e(temp);
plot(temp, vapor_pressure, 'k')