
#*************************************************************************************************
# ANALIZING HEATWAVES CONSIDERING ENSO PHASES
#*************************************************************************************************

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(highcharter)
library(htmltools)

setwd("C:/Heatwave nightmare/")

#*************************************************************************************************
#Charge HW data (by month)

dir_path <- "./Ext_varERA5_region/var_region_month/"

var<- read.csv("./Ext_varERA5_region/var_region_month/tmax_month_duration.csv")
var2<- read.csv("./Ext_varERA5_region/var_region_month/tmax_month_events.csv")
var3<- read.csv("./Ext_varERA5_region/var_region_month/tmax_month_int_max.csv")
var4<- read.csv("./Ext_varERA5_region/var_region_month/tmax_month_int_mean.csv")

#rm(var, var2, var3, var4, result_d, result_e, result_imn, result_imx)
# Ordenar los data frames por las columnas relevantes
var <- var[order(var$year, var$month, var$area_geografica), ]
var2 <- var2[order(var2$year, var2$month, var2$area_geografica), ]
var3 <- var3[order(var3$year, var3$month, var3$area_geografica), ]
var4 <- var4[order(var4$year, var4$month, var4$area_geografica), ]

#Charge ENSO data (by month)
enso <- read_excel("ENSO_years.xlsx") %>%
  select(YR, MON, ENSO)

#*************************************************************************************************
#Join ENSO and HW data

result_d <- var %>%
  left_join(enso, by = c("year" = "YR", "month" = "MON")) %>%
  select(year, month, duracion, area_geografica, ENSO) %>%
  mutate(duration= as.numeric(duracion))

#write.csv(result_d, file = file.path(dir_path, "tmin_enso_duration.csv"), row.names = FALSE)

result_e <- var2 %>%
  left_join(enso, by = c("year" = "YR", "month" = "MON")) %>%
  select(year, month, evento, area_geografica, ENSO) %>%
  mutate(evento= as.numeric(evento))

#write.csv(result_e, file = file.path(dir_path, "tmin_enso_events.csv"), row.names = FALSE)

result_imx <- var3 %>%
  left_join(enso, by = c("year" = "YR", "month" = "MON")) %>%
  select(year, month, int_maxima, area_geografica, ENSO) %>%
  rename(int_maxima= int_maxima) 

#write.csv(result_imx, file = file.path(dir_path, "tmin_enso_int_max.csv"), row.names = FALSE)

result_imn <- var4 %>%
  left_join(enso, by = c("year" = "YR", "month" = "MON")) %>%
  select(year, month, int_media, area_geografica, ENSO) %>%
  mutate(int_mean= as.numeric(int_media))

#write.csv(result_imn, file = file.path(dir_path, "tmin_enso_int_mean.csv"), row.names = FALSE)


#***********************************************************************************************
#*Analizing differences between HW events/regions/ENSO Phases
#*************************************************************************************************
# Función para calcular las diferencias
calcular_diferencias <- function(tabla, variable) {
  # Filtrar las categorías de interés
  filtered_table <- tabla %>%
    filter(ENSO %in% c("NIÑO", "NIÑA")) %>%
    group_by(area_geografica, ENSO) %>%
    summarise(avg_variable = mean({{ variable }}))
  
  # Calcular la diferencia entre NIÑO y NIÑA para cada región
  diff_table <- filtered_table %>%
    spread(key = ENSO, value = avg_variable) %>%
    mutate(Dif = NIÑO - NIÑA) %>%
    select(area_geografica, Dif)
  
  return(diff_table)
}

# Calcular diferencias para cada tabla
diff_result_d_duration <- calcular_diferencias(result_d, duration) %>%
  rename(Dif_d= Dif)
diff_result_e_evento <- calcular_diferencias(result_e, evento)%>%
  rename(Dif_e= Dif)
diff_result_imn_int_media <- calcular_diferencias(result_imn, int_media)%>%
  rename(Dif_imn= Dif)
diff_result_imx_int_maxima <- calcular_diferencias(result_imx, int_maxima)%>%
  rename(Dif_imx= Dif)

# Combinar todas las diferencias
diff_combined <- full_join(diff_result_d_duration, diff_result_e_evento, by = "area_geografica") %>%
  full_join(diff_result_imn_int_media, by = "area_geografica") %>%
  full_join(diff_result_imx_int_maxima, by = "area_geografica")

# Guardar en un archivo CSV
write.csv(diff_combined, file = "tmax_dif_reg_enso.csv", row.names = FALSE)

#***********************************************************************************************
#*Significance - no parametric
#*************************************************************************************************
#*
#*perform_kruskal_wilcoxon <- function(data, duration_col, enso_col, area_col, output_prefix) {
# Realizar la prueba de Kruskal-Wallis para ENSO
kruskal_result_enso <- kruskal.test(data[[duration_col]] ~ data[[enso_col]], data = data)
print(kruskal_result_enso)

# Realizar la prueba de Kruskal-Wallis para área geográfica
kruskal_result_area <- kruskal.test(data[[duration_col]] ~ data[[area_col]], data = data)
print(kruskal_result_area)

# Realizar comparaciones múltiples no paramétricas con la prueba de Wilcoxon (ajustar p-values con el método de Holm)
pairwise_result <- pairwise.wilcox.test(data[[duration_col]], interaction(data[[enso_col]], data[[area_col]]), p.adjust.method = "holm")
print(pairwise_result)

# Guardar los resultados en archivos Excel
kruskal_resultados <- data.frame(
  Test = character(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Añadir resultados de kruskal_result_enso
kruskal_resultados <- rbind(kruskal_resultados, c(paste("Kruskal-Wallis (", enso_col, ")", sep = ""), kruskal_result_enso$p.value))

# Añadir resultados de kruskal_result_area
kruskal_resultados <- rbind(kruskal_resultados, c(paste("Kruskal-Wallis (", area_col, ")", sep = ""), kruskal_result_area$p.value))

# Renombrar las columnas
colnames(kruskal_resultados) <- c("Test", "p_value")

write_xlsx(kruskal_resultados, paste0(output_prefix, "_kruskal_tn_", duration_col, ".xlsx"))

# Convertir el resultado de las comparaciones múltiples a un formato tabular
tidy_wilcoxon <- tidy(pairwise_result)

# Guardar el resultado en un archivo Excel
write_xlsx(list(Wilcoxon = tidy_wilcoxon), paste0(output_prefix, "_wilcoxon_tn_", duration_col, ".xlsx"))

perform_kruskal_wilcoxon(result_d, "duration", "ENSO", "area_geografica", "result_d")
perform_kruskal_wilcoxon(result_e, "evento", "ENSO", "area_geografica", "result_e")
perform_kruskal_wilcoxon(result_imn, "int_media", "ENSO", "area_geografica", "result_imn")
perform_kruskal_wilcoxon(result_imx, "int_maxima", "ENSO", "area_geografica", "result_imx")

