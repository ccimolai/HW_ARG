#******************************************************************************************
#*#******************************************************************************************
#*CREAR UN RASTER QUE CONTENGA TODA LA DIMENSION ESPACIAL Y TEMPORAL CON LOS DATOS ERA5-LAND
#una vez que tenga todos los archivos descargados de COPERNICUS (ver script en: downloads_copernicus_KrigR.R)= en cada archivo tengo toda la dimensión espacial pero por cada año por separado
# los junto a todos los netcdf en 1 que contenga toda la dim espacial y temporal (1950-2022)

#CREATE A RASTER CONTAINING THE ENTIRE SPATIAL AND TEMPORAL DIMENSION WITH ERA5-LAND DATA
#Once you have downloaded all the files from COPERNICUS (see script at: downloads_copernicus_KrigR.R) 
#each file contains the entire spatial dimension but for each year separately 
#merge all the NetCDF files into one that encompasses the entire spatial and temporal dimension (1950-2022).
#******************************************************************************************
caterinator<-function(fyear=1950,lyear=2022){
  for (i in fyear:lyear){
    filena<-paste0('./ERA5/2m_temperature_',i,'-01-01_',i,'-12-31_day.nc')
    print(filena)
    
    if(i==fyear){ladrillito<-raster::brick(filena)}
    else{
      nyu<-raster::brick(filena)
      ladrillito<-raster::stack(ladrillito,nyu)}
  }
  writeRaster(ladrillito, "./Stacks/ARG_tmax.tiff") #ubicacion del raster completo
}

ladrillon<- raster::brick("./Stacks/ARG_tmax.tiff")
df_tmax <- rasterToPoints(ladrillon)


#******************************************************************************************
#******************************************************************************************
#PREPARAR LOS DATOS PARA CALCULAR LOS EVENTOS DE OLAS DE CALOR
#separar el data frame entero, por cada pixel. Es decir, hay que generar una tabla que corresponda paracada pixel que tenga 
#dos columnas, una seria el tiempo y la otra la temperatura
#cada una de esta tabla deberia guardarse con el nombre del pixel para no perder esa informacion. que va a ser necesaria cuando quiera juntar todo de nuevo. 

# PREPARE THE DATA FOR CALCULATING HEATWAVE EVENTS
# Split the entire data frame for each pixel= generate a table for each pixel that has two columns: 
# one for time and the other for temperature.
# Each of these tables should be saved with the pixel name to retain that information, 
#which will be necessary when merging everything back together.

#******************************************************************************************
crear_eventos<-function(){

pixel<- df_tmax[,1:2]
pixel<- round(pixel, digits=2)

s <- as_date(dmy("01-01-1950"))
f <- as_date(dmy("31-12-2022"))
fechas<- as_date(seq(from=s, to=f, by=1))

for(i in 1:nrow(df_tmax)){
  print(i);
  mate<-t((df_tmax[i,3:ncol(df_tmax)]))
      mate<-data.frame(t=fechas,temp = round(mate-273.15,1))
      gardel<-ts2clm(mate,climatologyPeriod = c('1961-01-01','1990-12-31'))
      tango<-detect_event(gardel)

      nombre<- paste0('./eventos/', 'tmax-',pixel[i,1],'-',pixel[i,2], '.Rds')
      saveRDS(tango, nombre)
      
}
}


#******************************************************************************************
#******************************************************************************************
#ANALIZAR LOS EVENTOS DE OLAS DE CALOR EN PARALELELO
#ANALYZE HEATWAVES EVENTS IN PARALLEL
#******************************************************************************************

ncores <- detectCores()

# Registrar núcleos a utilizar
registerDoParallel(cores = ncores)

# Definir ruta de los archivos a procesar
ruta <- "./eventos/"

lista_archivos <- list.files(path = ruta, pattern = "*.Rds", full.names = TRUE)

procesar_archivo <- function(archivo) {
  
  eventos <- read.table(archivo)
  
  coord1 <- substring(head(archivo), 16, 20)
  coord2 <- substring(head(archivo), 22, 26)
  xmin <- data.frame(columna = rep(coord1, 173))
  xmax <- data.frame(columna = rep(coord1, 173))
  ymin <- data.frame(columna = rep(coord2, 173))
  ymax <- data.frame(columna = rep(coord2, 173))
  
  clima<- eventos$climatology
  eventos<- eventos$event
  hot <- cbind(xmin, xmax, ymin, ymax, eventos$date_start, eventos$duration, eventos$intensity_max_abs)
  names(hot) <- c("xmin", "xmax", "ymin", "ymax", "Date_start", "Duration", "Event_intensity_max")
  
  return(hot)
}

superhot <- foreach(archivo = lista_archivos, .combine = rbind) %dopar% procesar_archivo(archivo)
saveRDS(superhot, file = "arg_eventos.rds")
stopImplicitCluster()
superhot

#******************************************************************************************
