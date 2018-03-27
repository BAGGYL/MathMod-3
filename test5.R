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
tbl<-tbl[,c(-1,-3,-9,-12,-15,-18,-21,-30,-35,-70,-88:-99)] 
tbl 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
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
mod = lm(formula, data=tbl) 
mod 
coef(mod) 
resid(mod) 
confint(mod) 
summary(mod) 
anova(mod) 
mod1=lm(co2_flux~(DOY+Tau+rand_err_Tau + H+rand_err_H +LE+ rand_err_LE +co2_flux+rand_err_co2_flux+ rand_err_h2o_flux + H_strg + co2_molar_density +h2o_time_lag + sonic_temperature + 
                    + air_temperature+air_density+air_molar_volume+es+RH+VPD+u_star_+TKE+T_star_+un_H+un_Tau+un_LE+un_co2_flux+un_h2o_flux+flowrate)^2,data=tbl)
mod1 
coef(mod1) 
resid(mod1) 
confint(mod1) 
summary(mod1) 
anova(mod1) 
plot(mod1)

