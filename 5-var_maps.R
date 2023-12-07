#This script is a modification of an original script by Dr. Oleg Skrynyk-
setwd("C:/Heatwave nightmare/")
wd<-getwd()

#*************************************************************************************************
#*************************************************************************************************
library("ggplot2")
theme_set(theme_bw()) #............We are going to use the classic dark-on-light theme for ggplot2
library("sf") #...............................Contains the function 'geom_sf()' used to build maps
library("rnaturalearth") #.........................Provides a map of countries of the entire world
library("rnaturalearthdata")
library("maps")
library("ggpubr") #.......This package contains 'ggarrange()' to combine several plots in one file
library("raster") #.........This package contains crs() function to define a coordinate projection
library("colorspace") #.....................................................Contains color pallets
library("reproj")
library(dplyr)
library(cowplot) #para el arreglo de mapas

colorpallet <- colorRampPalette(c("#59c3e5", "#ecf49f", "#ffff56", "#ffd700","#feb125","#fe8634","#ff4000","#c70000", "#800000"))

colores_eventos <- c(a = "#f78283", A = "#a20a20", b = "#a7e3f1", B = "#111065")

#Define map projection
LCC <- crs("+proj=lcc +lat_1=-27.00 +lat_2=-47.00 +lat_0=-37.00 +lon_0=-64.00 +x_0=0 +y_0=0 +ellps=intl +units=km +no_defs")
LCC.text <- "+proj=lcc +lat_1=-27.00 +lat_2=-47.00 +lat_0=-37.00 +lon_0=-64.00 +x_0=0 +y_0=0 +ellps=intl +units=km +no_defs"
#Get boundary of Argentina
ar <- st_read("./Mapas_graficos/mapas_oleg/ssa_limits.shp")
ar.LCC <- st_transform(ar, LCC) #Transform country boundaries to the LCC projection

#Define how maps will look like
theme <- theme(
  plot.title = element_text(color="blue", size=10, face="bold", hjust = 0.5),
  plot.subtitle = element_text(color="black", size=8, hjust = 0.5),
  axis.title.x = element_text(color="black", size=8, face="bold"),
  axis.title.y = element_text(color="black", size=8, face="bold"),
  axis.text = element_text(color="black", size=8, face="bold"),
  legend.title = element_text(color="black", size=8),
  legend.text = element_text(color = "black",size=8),
  #legend.background = element_rect(fill = "lightgray"),
  legend.background = element_rect(fill = "white"),
  #legend.position = c(1, 0),
  legend.key.height= unit(1.0, 'cm'), #Define a size of a color scale
  legend.key.width= unit(0.3, 'cm'), #Define a size of a color scale
  legend.justification = c("right", "center"))
#legend.justification = c("right", "bottom"))

#*************************************************************************************************
# Read data
#*************************************************************************************************
#Define the path where data are stored and where the results will be saved
path2data <- "./Mapas_graficos/mapas_oleg/"
path2out <- "./Mapas_graficos/mapas_oleg/"
dat1 <- read.csv(file=paste0(path2data,"rasterera5_tx_event.csv"),header=TRUE)
dat3 <- read.csv(file=paste0(path2data,"rasterera5_tx_duration.csv"),header=TRUE) %>%
  select(-1)
dat4 <- read.csv(file=paste0(path2data,"rasterera5_tx_int_mean.csv"),header=TRUE) %>%
  select(-1)
dat5 <- read.csv(file=paste0(path2data,"rasterera5_tx_int_max.csv"),header=TRUE)%>%
  select(-1)

colnames(dat1) <- c("lon","lat","ev")
colnames(dat3) <- c("lon","lat","du")
colnames(dat4) <- c("lon","lat","im")
colnames(dat5) <- c("lon","lat","ix")

dat2 <- read.csv2(file=paste0(path2data,"WS_tx_era5_stations_anualtrend.csv"),header=TRUE)
#*************************************************************************************************
# Prepare trend data -event
#*************************************************************************************************
dfr <- rasterFromXYZ(dat1,res=c(0.1,0.1), crs=4326)
dfr.LCC <- projectRaster(dfr, crs = LCC, res = 10.0)
df.LCC <- as.data.frame(dfr.LCC, xy = TRUE)
df.LCC <- df.LCC[!is.na(df.LCC$ev),]
#*************************************************************************************************
# Prepare trend data -duration
#*************************************************************************************************
dfr3 <- rasterFromXYZ(dat3,res=c(0.1,0.1), crs=4326)
dfr3.LCC <- projectRaster(dfr3, crs = LCC, res = 10.0)
df3.LCC <- as.data.frame(dfr3.LCC, xy = TRUE)
df3.LCC <- df3.LCC[!is.na(df3.LCC$du),]
#*************************************************************************************************
#* Prepare trend data -intensity mean
#*************************************************************************************************
dfr4 <- rasterFromXYZ(dat4,res=c(0.1,0.1), crs=4326)
dfr4.LCC <- projectRaster(dfr4, crs = LCC, res = 10.0)
df4.LCC <- as.data.frame(dfr4.LCC, xy = TRUE)
df4.LCC <- df4.LCC[!is.na(df4.LCC$im),]
#*************************************************************************************************
#* Prepare trend data -intensity max
#*************************************************************************************************
dfr5 <- rasterFromXYZ(dat5,res=c(0.1,0.1), crs=4326)
dfr5.LCC <- projectRaster(dfr5, crs = LCC, res = 10.0)
df5.LCC <- as.data.frame(dfr5.LCC, xy = TRUE)
df5.LCC <- df5.LCC[!is.na(df5.LCC$ix),]

#*************************************************************************************************
# Prepare station data to build in LCC projection
#*************************************************************************************************
##dat2.LCC <- as.data.frame(reproj(cbind(as.numeric(dat2$lon),as.numeric(dat2$lat)),target=LCC.text,source = getOption("reproj.default.longlat"))) 
##dat2.LCC <- dat2.LCC[,-3]
##dat2.LCC <- cbind(dat2.LCC,sign=c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,4,4,4,4))

dat2_sf <- st_as_sf(dat2, coords = c("lon", "lat"), crs = 4326) # 4326 es el sistema de coordenadas WGS84
dat2_LCC <- st_transform(dat2_sf, crs = LCC)
#dat2_LCC$sign <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4)

#*************************************************************************************************
# Build map- event
#*************************************************************************************************
#colorpallet <- colorRampPalette(c("#59c3e5", "#ecf49f", "#ffff56", "#ffd700","#feb125","#fe8634","#ff4000","#c70000", "#800000"))
limit <- c(-0.4,1.8)
b <- c(-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6, 1.8)
titletext <- "Number of events"
subtitletext <- "Significative trend"
scaletitle <- "Nº events/10 years"

g01 <- ggplot() +
  geom_raster(data = df.LCC,aes(x = x, y = y, fill = ev)) +
  scale_fill_gradientn(scaletitle,limits = limit,breaks=b,colours=colorpallet_02(9))+
  geom_sf(data = ar.LCC, color = "black", size = 0.15, fill = NA) +
  ggtitle(titletext, subtitle = subtitletext) +
  xlab("Longitude") + ylab("Latitude") +
  xlim(c(-1000.00, 1000.00)) +
  ylim(c(-2100.00, 2100.00)) +
  theme

#g01 <- g01 +
  geom_point(data = dat2_LCC, aes(x = st_coordinates(dat2_LCC)[, "X"], y = st_coordinates(dat2_LCC)[, "Y"], colour = factor(events)), show.legend = FALSE) +
  scale_colour_manual(values = colores_eventos)
#*************************************************************************************************
# Build map -duration
#*************************************************************************************************
  colorpallet_02 <- colorRampPalette(c("#59c3e5","#acf2d8","#ecf49f", "#ffff56", "#ffd700","#feb125","#fe8634","#ff4000","#c70000", "#800000"))
  limit_2 <- c(-3.0,19.5)
  b_2 <- c(-3.0,-1.5, 0, 1.5, 3.0, 4.5, 6.0, 7.5, 9.0, 10.5, 12.0, 13.5, 15.0, 16.5, 18.0, 19.5)

titletext_2 <- "Duration"
subtitletext_2 <- "Significative trend"
scaletitle_2 <- "Nº of days/10 years"

g02 <- ggplot() +
  geom_raster(data =  df3.LCC,aes(x = x, y = y, fill = du)) +
  scale_fill_gradientn(scaletitle_2,limits = limit_2,breaks=b_2,colours=colorpallet_02(9)) +
  geom_sf(data = ar.LCC, color = "black", size = 0.15, fill = NA) +
  ggtitle(titletext_2,subtitle = subtitletext_2) +
  xlab("Longitude") + ylab("Latitude") +
  xlim(c(-1000.00, 1000.00)) +
  ylim(c(-2100.00, 2100.00)) +
  theme

g02 <- g02 +
  geom_point(data = dat2_LCC, aes(x = st_coordinates(dat2_LCC)[, "X"], y = st_coordinates(dat2_LCC)[, "Y"], colour = factor(duration)), show.legend = FALSE) +
  scale_colour_manual(values = colores_eventos)
#*************************************************************************************************
# Build map -intensity mean
#*************************************************************************************************

colorpallet_2 <- colorRampPalette(c("#59c3e5","#a0f0cd","#c0e3c1","#ecf49f", "#ffd700","#fe8634","#ff4000","#c70000", "#800000"))
#limit_3 <- c(-0.6,1.5)
#b_3 <- c(-0.6,-0.4,-0.2,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5)

limit_3 <- c(-0.9,1.6)
b_3 <- c(-0.9,-0.7,-0.5,-0.3,0,0.3, 0.6, 0.9, 1.2, 1.6)

titletext_3 <- "Mean intensity"

subtitletext_3 <- "Significative trend"

scaletitle_3 <- "ºC/10 years"

g03 <- ggplot() +
  geom_raster(data =  df4.LCC,aes(x = x, y = y, fill = im)) +
  scale_fill_gradientn(scaletitle_3,limits = limit_3,breaks=b_3,colours=colorpallet_2(9)) +
  geom_sf(data = ar.LCC, color = "black", size = 0.15, fill = NA) +
  ggtitle(titletext_3,subtitle = subtitletext_3) +
  xlab("Longitude") + ylab("Latitude") +
  xlim(c(-1000.00, 1000.00)) +
  ylim(c(-2100.00, 2100.00)) +
  theme

g03 <- g03 +
  geom_point(data = dat2_LCC, aes(x = st_coordinates(dat2_LCC)[, "X"], y = st_coordinates(dat2_LCC)[, "Y"], colour = factor(intensity_mean)), show.legend = FALSE) +
  scale_colour_manual(values = colores_eventos)
#*************************************************************************************************
# Build map -intensity max
#*************************************************************************************************
limit_4 <- c(-0.9,1.6)
b_4 <- c(-0.9,-0.7,-0.5,-0.3,0,0.3, 0.6, 0.9, 1.2, 1.6)

titletext_4 <- "Maximum intensity"
subtitletext_4 <- "Significative trend"
scaletitle_4 <- "ºC/10 years"

g04 <- ggplot() +
  geom_raster(data =  df5.LCC,aes(x = x, y = y, fill = ix)) +
  scale_fill_gradientn(scaletitle_4,limits = limit_4,breaks=b_4,colours=colorpallet_2(9)) +
  geom_sf(data = ar.LCC, color = "black", size = 0.25, fill = NA) +
  ggtitle(titletext_4,subtitle = subtitletext_4) +
  xlab("Longitude") + ylab("Latitude") +
  xlim(c(-1000.00, 1000.00)) +
  ylim(c(-2100.00, 2100.00)) +
  theme

g04 <- g04 +
  geom_point(data = dat2_LCC, aes(x = st_coordinates(dat2_LCC)[, "X"], y = st_coordinates(dat2_LCC)[, "Y"], colour = factor(intensity_max)), show.legend = FALSE) +
  scale_colour_manual(values = colores_eventos)
#*************************************************************************************************
# Build ARREGLO DE MAPAS
#*************************************************************************************************
g <- ggarrange(g01,g02, g03, g04, ncol=2,nrow=2)

#*************************************************************************************************

#titulo <- ggdraw() +
  #draw_label("Trends of heatwaves in Argentina", fontface = 'bold', size = 12)
#g_g <- plot_grid(titulo, g, ncol = 1, rel_heights = c(0.1, 0.9), hjust = -0.5)

#png(paste0(path2out,"Rez.png"),width=12, height=20, units="cm",bg="white",pointsize=2, res=300)
png(paste0(path2out,"Rez.png"),width=7, height=10, units="cm",bg="white",pointsize=2, res=300)
print(g_g)
dev.off()
