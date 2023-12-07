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
  
audrey_2<-function(tg,open=FALSE){
    ### takes a raster, computes anual means and returns a stack with trend (OLS) and pvalue  
    ## tg is a raster with annual values
    if(open){tg<-brick(tg)}  
    time<-as.numeric(substring(names(tg),2,5))
    lm_fun = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }} # slope
    regressed<-calc(tg,lm_fun)*10 # times 10 = per decade
    lm_fun2 = function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[8] }} # pval
    pvalues<-calc(tg,lm_fun2)
    trends<-stack(regressed,pvalues)
    return(trends)
  }
trends<- audrey_2(tg)

#******************************************************************************************
# argentina´s limits if needed
  area <- readOGR("./limites_Arg/Argentina_limites.shp") # abrimos el fichero para la mascara
  crs(area) <- "EPSG:4326" # definimos el sistema de coordenadas
  a_crs_object_epsg <-CRS("+proj=longlat +datum=WGS84 +no_defs +init=epsg:4326")
  area <- spTransform(area, a_crs_object_epsg)
  
#******************************************************************************************  
##CUT RASTER (IF NEEDED)
projection(tg) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
projection(trends) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#var_duration = raster::crop(tg, area) %>% raster::mask(area) 
#var_trend = raster::crop(trends, area) %>% raster::mask(area) 
m1<- raster::mean(var_duration, na.rm= TRUE)
  
writeRaster(m1, filename = file.path("./6_stats", "tmax_duracion_mean.tif"), overwrite=TRUE)

#******************************************************************************************  
# Create a new layer only with significant trends(p_value<=0.05)
ttt<- var_trend[[1]]
ppp<- var_trend[[2]]
val_sig <- overlay(ttt, ppp, fun = function(x,y) {ifelse(y <= 0.05, x, NA)})
writeRaster(val_sig, filename = file.path("./6_stats", "tmax_duracion_trend.tif"), overwrite=TRUE)
  
