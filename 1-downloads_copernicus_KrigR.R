
## ERA5 downloads

#starter<-function(){ 
  API_User <<- "143020"
  API_Key<<-"c818a65c-1997-4533-9173-58a64ba74079"
  
  library(KrigR)
  install.load.package <- function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, repos = "http://cran.us.r-project.org")
    }
    require(x, character.only = TRUE)
  }
mydownload<-function(fyear,lyear)
  
  # set a key to the keychain
  wf_set_key(user = "143020",
             key = "c818a65c-1997-4533-9173-58a64ba74079",
             service = "cds")
  
## download an era5 or era land variable
fyear <- 2018
lyear <- 2022
  
for(i in fyear:lyear){
  primu = paste0(i,"-01-01")
  buho = paste0(i,"-12-31")
  print(i)
  KrigR::download_ERA(
    Variable = "2m_temperature",
    PrecipFix = FALSE,
    Type = "reanalysis",
    DataSet = "era5-land",
    DateStart = primu,
    DateStop = buho, 
    TResolution = "day",
    TStep = 1,
    FUN = "max", #also download min
    Extent = extent(-75, -50, -60, -20),
    Buffer = 0.5,
    ID = "ID",
    Dir = './ERA5',
    FileName = NULL,
    API_User = API_User,
    API_Key = API_Key,
    TryDown = 10,
    verbose = TRUE,
    Cores = 1,
    TimeOut = 36000,
    SingularDL = FALSE
  )
}

#******************************************************************************************
### read era5
mydataframe<-function(fyear,lyear){
rm(slp2)
k=1
for(i in 1950:2022){
  print(i)
 
  name<-paste0('./era5/2mt_',i,'-01-01_',i,'-12-31_day.nc')

  nyu<-ReadNetCDF(file = name,vars = "2m_temperature",out = "data.frame")
  primu = paste0(i,"-01-01")
  buho = paste0(i,"-12-31")
  fechas<-seq(as.Date(primu),as.Date(buho),by='days')
  unicos<-unique(nyu$z)
  kaka<-data.frame(z=unicos,fechas=fechas)
  nyi<-inner_join(kaka,nyu,by=c('z'))
  nyi<-nyi[,-1]
  names(nyi)<-c("time","lat","lon","value")
  slp<-nyi %>% filter(lat >= 25  & lat <= 70 & lon >=-30 & lon <=30)
  if(k == 1){slp2<-slp}else{slp2<-bind_rows(slp2,slp)}
  k=k+1 
  rm(slp);rm(nyi)  
}
return(slp2)
}
