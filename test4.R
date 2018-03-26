library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
library("ggplot2") 
library("dplyr") 
tbl = read_csv("C:/Odincova_124/MathMod/eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl = tbl[-1,] 
tbl 
glimpse(tbl) 
tbl = select(tbl, -(roll)) 
tbl = tbl [ ,c(-6, -7, -9, -10, -12, -13, -15, -16, -18, -19, -21, -22, -77:-130)] 
tbl 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
tbl=tbl[tbl$DOY > 181 & tbl$DOY < 212,] 
tbl 
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric)] 
tbl_numeric 
cor_tbl = cor(tbl_numeric) 
cor_tbl 
cor_tbl = cor(na.omit(tbl_numeric)) 
cor_tbl 
cor_tbl = cor(na.omit(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
cor_tbl 
vars = row.names(cor_tbl)[cor_tbl$co2_flux^2 > .7] %>% na.exclude 
vars 
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep = "")) 
formula 
row_numbers = 1:length(tbl$date) 
teach = sample(row_numbers, floor(length(tbl$date)*.7)) 
test = row_numbers[-teach] 
teaching_tbl_unq = tbl[teach,] 
testing_tbl_unq = tbl[test,] 
mod1 = lm(co2_flux~Tau+H+LE+co2_flux+h2o_flux+H_strg+co2_v_minus_adv+h2o_v_minus_adv +co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+h2o_mole_fraction + h2o_mixing_ratio + h2o_time_lag + sonic_temperature+air_temperature+air_pressure+air_density+air_heat_capacity+air_molar_volume+water_vapor_density+e+es+specific_humidity+RH+VPD+Tdew+u_unrot+v_unrot+w_unrot+u_rot+v_rot+w_rot+wind_speed+max_speed+yaw+pitch+u*+TKE+L+_z_minus_d__div_L+bowen_ratio+x_peak+x_offset+x_10%+x_30%+x_50%+x_70%+x_90+un_Tau, data = tbl) 
summary(mod1) 
predict(mod1) 
resid(mod1) 
coef(mod1) 
anova(mod1) 
plot(mod1) 
mod2 = lm(co2_flux~H+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+air_density+u_unrot+v_unrot+w_unrot+yaw+pitch, data=tbl) 
summary(mod2) 
predict(mod2) 
resid(mod2) 
coef(mod2) 
anova(mod2) 
plot(mod2) 
mod3 = lm(co2_flux~H+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+air_density+w_unrot+yaw+pitch, data=tbl) 
summary(mod3) 
predict(mod3) 
resid(mod3) 
coef(mod3) 
anova(mod3) 
plot(mod3) 
mod4 = lm(co2_flux~H+co2_molar_density+co2_mole_fraction+co2_mixing_ratio+h2o_molar_density+air_density+yaw+pitch, data=tbl) 
summary(mod4) 
predict(mod4) 
resid(mod4) 
coef(mod4) 
anova(mod4) 
plot(mod4)