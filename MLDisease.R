library(rgdal)
library(raster)
library(Matrix)
library(xgboost)

ConsR <- function(RH,tC) {
  # argument : tRHTC is a concathenated vector of hourly RH for the 7 previous days
  # followed by tC hourly for the 7 previous days
  # RH  =  Relative Humidity
  # tC  =  temperature *C
  # value : list, [[1]]=tcons and [[2]] consmc.
  # - tcons is the number of hours of the periods of the day with 
  # consecutive hours with humidity above 90%, 
  # starting fom noon to noon the next day
  # - consmc is the mean temperature for each periods
  # Usage : To get each assign out1 = ConsR(c(tRH, tC)) then out1$tcons and out1$consmc
  # This outputs tcons and consmc as a list.
  # To get each assign out1 = ConsR(tRH, tC) then out1$tcons and out1$consmc
  consmc <- c()
  first <- TRUE
  tcons <- 0
  cons_index <- 1
  tttemp <- (-99)
  tcons[[1]] = 0
  lenvec <- length(RH)
  
  for (j in (1:lenvec)) {
    if (RH[j] >= 90) {
      tcons[cons_index] <- tcons[cons_index] + 1
      if (first) {
        tttemp <- tC[j]
      }
      
      else{
        tttemp <- c(tttemp, tC[j])
      }
      first <- FALSE
      
      if ((RH[j + 1] < 90 & j < lenvec) | j  ==  lenvec) {
        consmc[cons_index] <- mean(tttemp)
        cons_index <- cons_index + 1
        tcons[cons_index] = 0
        tttemp <- (-99)
        first <- TRUE
      }
    }
  }
  cons_out <- list(tcons=tcons, consmc=consmc)
  cons_out
}

weatherVars7days <- function(Date=Sys.Date(), coords, removeClimateData=TRUE){
  # Returns the amount of favorable periods, its average duration and its average temperature 
  # Argument coords is a dataframe of the coordinates for which the temperature and humidity will be downloaded
  URL <- "http://bart.ideam.gov.co/wrfideam/new_modelo/WRF00COLOMBIA/tif/"
  
  Date<-Date-7
  
  files = lapply(c(TEMP="TEMP1H_",RH="RH1H_"),function(x){
    list(Day1 =c(paste(x,rep(as.character(format(Date-1, "%d%m%Y")),11),"_fcst_DIA1",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA1",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),
         Day2 =c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA1",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA2",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),
         Day3 =c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA2",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA3",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),
         Day4 =c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA3",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA4",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),
         Day5 =c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA4",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA5",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),
         Day6 =c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA5",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA6",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),
         Day7 =c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA6",13:23,"HLC.tif",sep=""),
                 paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA7",c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")))})
  fileInDir = lapply (files,function(x) lapply(x,function(y) paste(getwd(),"/Data/maps/",y,sep="")) )
  
  zipfile = list(TempDayBefore = paste("geoTIFFtemphorario",as.character(format(Date-1, "%d%m%Y")),"00Z.zip",sep=""),
                 TempCurrentDay = paste("geoTIFFtemphorario",as.character(format(Date, "%d%m%Y")),"00Z.zip",sep=""),
                 RHDayBefore = paste("geoTIFFhumedadhorario",as.character(format(Date-1, "%d%m%Y")),"00Z.zip",sep=""),
                 RHCurrentDay = paste("geoTIFFhumedadhorario",as.character(format(Date, "%d%m%Y")),"00Z.zip",sep=""))
  zipfileInURL = lapply (zipfile, function(x) paste(URL,x,sep="") )
  zipfileInDir = lapply (zipfile, function(x) paste(getwd(),"/Data/maps/",x,sep="") )
  if (!all(file.exists(unlist(fileInDir)))) {
    for (typeOfData in c("TempDayBefore","TempCurrentDay","RHDayBefore","RHCurrentDay")){      
      if (!file.exists(zipfileInDir[[typeOfData]])) repeat {
        if (try(download.file(url = zipfileInURL[[typeOfData]], destfile = zipfileInDir[[typeOfData]], method="auto"))==0) break
      }
    }
    unzip(zipfile = zipfileInDir[["TempDayBefore"]],files = files[["TEMP"]][["Day1"]][1:11],exdir = "./Data/maps")
    unzip(zipfile = zipfileInDir[["RHDayBefore"]],files = files[["RH"]][["Day1"]][1:11],exdir = "./Data/maps")
    unzip(zipfile = zipfileInDir[["TempCurrentDay"]],exdir = "./Data/maps")
    unzip(zipfile = zipfileInDir[["RHCurrentDay"]],exdir = "./Data/maps")
  }
  file.rename(
    from=paste("Data/maps/",unlist(lapply(c(TEMP="TEMP1H_",RH="RH1H_"),function(x){c(paste(x,rep(as.character(format(Date-1, "%d%m%Y")),11),"_fcst_DIA1",13:23,"HLC.tif",sep=""))})),sep=""),
    to=paste("Data/maps/",unlist(lapply(c(TEMP="TEMP1H_",RH="RH1H_"),function(x){c(paste(x,rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA0",13:23,"HLC.tif",sep=""))})),sep=""))
  #warning(paste("couldn't get RH and mean T for ",Date," and ",Date-1,", trying ",Date-1," and ",Date-2," instead...",sep="") )
  #Date=Date-1
  
  tCarray<-matrix(nrow=length(coords[,1]))
  RHarray<-matrix(nrow=length(coords[,1]))
  for (Day in 1:7){
    TempFiles = paste("./Data/maps/",c(paste("TEMP1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",(Day-1),13:23,"HLC.tif",sep=""),
                                       paste("TEMP1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",Day,c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),sep="")
    RHFiles = paste("./Data/maps/",c(paste("RH1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",(Day-1),13:23,"HLC.tif",sep=""),
                                     paste("RH1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",Day,c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep="")),sep="")
    tCstack <- stack(TempFiles)
    RHstack <- stack(RHFiles)
    tCarray <- cbind(tCarray,extract(x=tCstack,y=places))
    RHarray <- cbind(RHarray,extract(x=RHstack,y=places))
  }
  
  tempPeriodLen<-vector()
  tempPeriods<-vector()
  tempTemp<-vector()
  
  favperTemp <- apply(RHarray[,-1],MARGIN = 1,FUN=ConsR,tC=tCarray[,-1])
  for(i in favperTemp) {
    tempPeriodLen <- c(tempPeriodLen,mean(i$tcons[i$tcons != 0]))
    tempPeriods <- c(tempPeriods,sum(i$tcons != 0))
    tempTemp <- c(tempTemp,mean(i$consmc))
  }
  
  favper=data.frame(tempPeriodLen,tempPeriods,tempTemp)
  
  if (removeClimateData) {
    for (Day in 0:7){
      file.remove(paste("./Data/maps/","TEMP1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",Day,(13:23),"HLC.tif",sep=""))
      file.remove(paste("./Data/maps/","TEMP1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",Day,c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep=""))
      file.remove(paste("./Data/maps/","RH1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",Day,13:23,"HLC.tif",sep=""))
      file.remove(paste("./Data/maps/","RH1H_",rep(as.character(format(Date, "%d%m%Y")),11),"_fcst_DIA",Day,c("00","01","02","03","04","05","06","07","08","09","10","11","12"),"HLC.tif",sep=""))
    }
  }
  if (removeClimateData){
    file.remove(paste("./Data/maps/","geoTIFFtemphorario",as.character(format(Date-1, "%d%m%Y")),"00Z.zip",sep=""))
    file.remove(paste("./Data/maps/","geoTIFFtemphorario",as.character(format(Date, "%d%m%Y")),"00Z.zip",sep=""))
    file.remove(paste("./Data/maps/","geoTIFFhumedadhorario",as.character(format(Date-1, "%d%m%Y")),"00Z.zip",sep=""))
    file.remove(paste("./Data/maps/","geoTIFFhumedadhorario",as.character(format(Date, "%d%m%Y")),"00Z.zip",sep=""))
  }
  return(favper)
}
