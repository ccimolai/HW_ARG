#TRENDS FOR COLD (MONTH 4-9) AND WARM (MONTH 10-3)
library(tidyverse)
setwd("C:/Heatwave nightmare/")

var<- read.csv("./Ext_varERA5_region/var_region_month/tmin_enso_duration.csv")
var2<- read.csv("./Ext_varERA5_region/var_region_month/tmin_enso_events.csv")
var3<- read.csv("./Ext_varERA5_region/var_region_month/tmin_enso_int_max.csv")
var4<- read.csv("./Ext_varERA5_region/var_region_month/tmin_enso_int_mean.csv")

#******************************************************
#*Calculate cold season (CS)
cal_cs <- function(data, variable) {
  yending <- data %>%
    select(-ENSO) %>%
    filter(month %in% c(4, 5, 6, 7, 8, 9)) %>% 
    group_by(year, area_geografica) %>%
    summarize(mean_d = mean({{variable}})) %>% #promedio regional anual
    group_by(area_geografica) %>%
    do(broom::tidy(zyp.zhang(.$mean_d, .$year)))
  
  trend_sig <- yending %>%
    filter(names %in% c("trend", "sig")) %>%
    pivot_wider(names_from = names, values_from = x) %>%
    mutate(trend = trend * 10)
  
  colnames(trend_sig) <- c("area_geografica", "trend", "sig")
  
  return(trend_sig)
}

#Apply the function to different datasets and variables of interes
d_cs <- cal_cs(var, duracion) %>%
  rename(cs_duration= estimate)
e_cs <- cal_cs(var2, evento)%>%
  rename(cs_events= estimate)
imx_cs <- cal_cs(var3, int_maxima)%>%
  rename(cs_int_mx= estimate)
imn_cs <- cal_cs(var4, int_media)%>%
  rename(cs_int_mn= estimate)

#******************************************************
#*calculate warm season (WS)
cal_ws <- function(data, variable) {
  yending <- data %>%
    select(-ENSO) %>%
    filter(month %in% c(10,11,12,1,2,3)) %>%
    group_by(year, area_geografica) %>%
    summarize(mean_d = mean({{ variable }})) %>%
    group_by(area_geografica) %>%
    do(broom::tidy(zyp.zhang(.$mean_d, .$year)))
  
  trend_sig <- yending %>%
    filter(names %in% c("trend", "sig")) %>%
    pivot_wider(names_from = names, values_from = x) %>%
    mutate(trend = trend * 10)
  
  # Renombrar las columnas
  colnames(trend_sig) <- c("area_geografica", "trend", "sig")
  
  return(trend_sig)
}

#Apply the function to different datasets and variables of interes
d_ws <- cal_ws(var, duracion) %>%
  rename(ws_duration= estimate)
e_ws <- cal_ws(var2, evento)%>%
  rename(ws_events= estimate)
imx_ws <- cal_ws(var3, int_maxima)%>%
  rename(ws_int_mx= estimate)
imn_ws <- cal_ws(var4, int_media)%>%
  rename(ws_int_mn= estimate)%>%
  mutate(significant = ifelse(p.value <= 0.05, "si", "no"))

# Complete period (Y)

cal_y <- function(data, variable) {
  yending <- data %>%
    select(-ENSO) %>%
    group_by(year, area_geografica) %>%
    summarize(mean_d = mean({{ variable }})) %>%
    group_by(area_geografica) %>%
    do(broom::tidy(zyp.zhang(.$mean_d, .$year)))
  
  trend_sig <- yending %>%
    filter(names %in% c("trend", "sig")) %>%
    pivot_wider(names_from = names, values_from = x) %>%
    mutate(trend = trend * 10)
  
  # Renombrar las columnas
  colnames(trend_sig) <- c("area_geografica", "trend", "sig")
  
  return(trend_sig)
}

#Apply the function to different datasets and variables of interes
d_y <- cal_y(var, duration) 
e_y <- cal_y(var2, evento)
imx_y <- cal_y(var3, int_maxima)
imn_y <- cal_y(var4, int_media)
