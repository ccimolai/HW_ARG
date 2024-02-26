#MAKE RASTERS OF EACH VARIABLE, FOR THE COMPLETE AREA
#do this for each variable (tx and tn)

setwd("/home/public/Documentos/Heatwave_nightmare")
prueba<- readRDS("./4_extract_var_month/tmax_month_duration.RDS")
tgq<-prueba %>% dplyr::select(lon = lon, lat = lat, YEAR = YEAR, int_maxima = int_maxima) %>% pivot_wider(names_from = YEAR, values_from = int_maxima) %>% rasterFromXYZ()
ordenado<-order(names(tgq))
tg<- tgq[[ordenado]]
  
writeRaster(tg, filename = file.path("./5_rastervar", "tmax_int_max.tif"), overwrite=TRUE)

#******************************************************************************************
#TRENDS
#Function: calculate trends and significance 
  
audrey_2 <- function(tg, open = FALSE) {
  ### Toma un raster, calcula medias anuales y devuelve un stack con trend (OLS) y valor p
  ## tg es un raster con valores anuales
  if (open) {
    tg <- brick(tg)
  }
  time <- as.numeric(substring(names(tg), 2, 5))
  
  # Definir función para aplicar a cada píxel
  lm_fun <- function(x) {
    if (all(is.na(x))) {
      trend <- NA  # Si todas las capas son NA, devolver NA
    } else {
      x <- ifelse(is.na(x), 0, x)  # Considerar NA como cero
      m <- zyp.zhang(x, time)
      trend <- m["trend"]* 10 #per decade
      
    }
    return(trend)
  } # slope
  
  # Aplicar la función a cada píxel
  regressed <- calc(tg, fun = lm_fun) 
  
  # Definir función para calcular p-values
  lm_fun2 <- function(x) {
    if (all(is.na(x))) {
      p_value <- NA  # Si todas las capas son NA, devolver NA
    } else {
      x <- ifelse(is.na(x), 0, x)  # Considerar NA como cero
      m <- zyp.zhang(x, time)
      p_value <- m["sig"]
    }
    return(p_value)
  } # pval
  
  # Aplicar la función a cada píxel
  pvalues <- calc(tg, fun = lm_fun2)
  
  # Apilar resultados
  trends <- stack(regressed, pvalues)
  
  return(trends)
}

trends<- audrey_2(tg)

#**************************************************************************************************************
#* Create a raster displaying the values of significant trends.

val_sig_Z<- overlay(trends$layer.1, trends$layer.2, fun = function(x,y) {ifelse(y <= 0.05, x, NA)})
writeRaster(val_sig_Z, filename = ".....", overwrite=TRUE)


  
