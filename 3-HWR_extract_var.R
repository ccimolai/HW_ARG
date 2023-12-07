#EXTRACT VARIABLES FROM HWR EVENTS
#Apply this for TX AND TN

#******************************************************************************************
list.of.packages <- c(
  "foreach",
  "doParallel",
  "ranger",
  "palmerpenguins",
  "tidyverse",
  "dplyr",
  "raster",
  "ncdf4",
  "kableExtra"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
    )
  )
}

#******************************************************************************************
#******************************************************************************************

#k$event %>%mutate(YEAR = as.numeric(substring(date_start,1,4))) %>% group_by(YEAR) %>%summarise(duracion =sum(duration,na.rm=TRUE)) %>% mutate(lat =lat,lon = lon)

### FUNCTION: extract coordinates from each file
coordinator<-function(filename){
  largo<-nchar(filename)-4
  peaso<-unlist(strsplit(substring(filename,1,largo),('--')))
  peaso<-list(lon<-as.numeric(peaso[2])*-1,lat=as.numeric(peaso[3])*-1)
  return(peaso)
}

#******************************************************************************************
#******************************************************************************************
#Extract "num of events"
lista<-list.files('./3_eventos_tmin',full=TRUE)
ene<-length(lista)

cl <- makeCluster(30)  # Cambia el número de núcleos según tus necesidades
registerDoParallel(cl)
clusterEvalQ(cl, library(dplyr))

asado <- foreach(i = 1:ene, .combine = rbind) %dopar% {
  k <- readRDS(lista[i])
  coords <- coordinator(lista[i])
  lon <- as.numeric(coords[1])
  lat <- as.numeric(coords[2])
  k$event %>%
    mutate(year = as.numeric(substring(date_start, 1, 4)),
           month = as.numeric(substring(date_start, 6, 7))) %>%
    group_by(year, month) %>%
    summarise(evento = n()) %>%
    mutate(lat = lat, lon = lon)
}

saveRDS(asado,'./4_extract_var_month/tmax_month_events.RDS')

#******************************************************************************************
#Extract "duration"- sum number of days with events during the year
lista<-list.files('./3_eventos',full=TRUE)
ene<-length(lista)
asado<-foreach(i = 1:ene, .combine = rbind) %dopar% {
  k<-readRDS(lista[i])
  coords<-coordinator(lista[i])
  lon<-as.numeric(coords[1])
  lat<-as.numeric(coords[2])
  k$event %>%
    mutate(year = as.numeric(substring(date_start, 1, 4)),
           month = as.numeric(substring(date_start, 6, 7))) %>%
    group_by(year, month) %>%
    summarise(duracion =sum(duration,na.rm=TRUE)) %>% mutate(lat =lat,lon = lon)
}

saveRDS(asado,'./4_extract_var_month/tmax_month_duration.RDS')


#******************************************************************************************
#Extract "max intensity"
lista<-list.files('./3_eventos',full=TRUE)
ene<-length(lista)
asado<-foreach(i = 1:ene, .combine = rbind) %dopar% {
  k<-readRDS(lista[i])
  coords<-coordinator(lista[i])
  lon<-as.numeric(coords[1])
  lat<-as.numeric(coords[2])
  k$event %>%
    mutate(year = as.numeric(substring(date_start, 1, 4)),
           month = as.numeric(substring(date_start, 6, 7))) %>%
    group_by(year, month) %>%
    summarise(int_max =mean(intensity_max,na.rm=TRUE)) %>% mutate(lat =lat,lon = lon)
}

saveRDS(asado,'./4_extract_var_month/tmax_month_int_max.RDS')

#******************************************************************************************
