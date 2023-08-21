### HOUNGNIBO MANDELA- SONGOTI HENRI- SEYDOU TRAORE 10/2020 #####

rm(list=ls())
packages <- c("lubridate", "stringr","foreach","doParallel","rgdal","raster","ncdf4","curl","R.utils")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#library
require(lubridate)
require(stringr)
library(foreach)
library(doParallel)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(raster)
library(ncdf4)
library(curl)
library(R.utils)



#lien de telechargement des donnees
#lnk.url<-"ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CCS/daily"
lnk.url="ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CCS/6hrly"

#definir les options de telechargement
#curl::handle_reset(h)
h <- curl::new_handle()
curl::handle_setopt(h, low_speed_limit=5,low_speed_time=180)

#repertoire de sauvegarde
dir_save_nc <-"/data_cdt/SATELLITE_DATA/model_data/persiann_ccs/hourly06/nc"
if(!dir.exists(dir_save_nc)){dir.create(dir_save_nc,recursive=TRUE)}
dir_save_bin <-"/data_cdt/SATELLITE_DATA/model_data/persiann_ccs/hourly06/bin"
if(!dir.exists(dir_save_bin)){dir.create(dir_save_bin,recursive=TRUE)}
# dir_save_tiff <-"F:/2020/data/chirp/tiff/Afrique"
# if(!dir.exists(dir_save_tiff)){dir.create(dir_save_tiff,recursive=TRUE)}
dir_save_daily_nc <-"/data_cdt/SATELLITE_DATA/model_data/persiann_ccs/daily/nc"
if(!dir.exists(dir_save_daily_nc)){dir.create(dir_save_daily_nc,recursive=TRUE)}
dir_save_tiff_cilss <-"/data_cdt/SATELLITE_DATA/model_data/persiann_ccs/daily/tif"
if(!dir.exists(dir_save_tiff_cilss)){dir.create(dir_save_tiff_cilss,recursive=TRUE)}


#periode des donnees
start_date <- as.POSIXct("2003-01-01 06:00:00")
#todays <- ymd("2003-01-30")
todays <- Sys.time()

#pour un eventuel decoupage
zone.cilss <- as(extent(-20,30,0,30),"SpatialPolygons")
zone.afrique <- as(extent(-20.05,55.05,-40.05,40.05),"SpatialPolygons")
crs(zone.afrique)<-"+proj=longlat +datum=WGS84 +no_defs"


dd.tmp1<- do.call("rbind", strsplit(as.character(seq(start_date, todays, by= "6 hour")), "-"))

jday=yday(seq(start_date, todays, by= "6 hour"))
heur=hour(seq(start_date, todays, by= "6 hour"))

dd.tmp2<- paste0(substr(year(seq(start_date, todays, by= "6 hour")),3,4),
                 sprintf("%03d",jday),sprintf("%02d",heur))

all_link <-paste(lnk.url,
                 paste("rgccs6h", dd.tmp2, ".bin.gz",paste(dd.tmp1[, 1],
                 dd.tmp1[, 2], substr(dd.tmp1[,3],1,2), paste0(substr(dd.tmp1[,3],4,5),".nc"),sep="_"),sep = ""),sep = "/")

already.down <- list.files(dir_save_nc)

to.down<- all_link[!(gsub("_","",substr(basename(all_link),22,34))%in%gsub("_","",substr(already.down,13,nchar(already.down)-3)))]


for(i in seq(to.down)){
  
  t1=try(curl::curl_download(substr(to.down[[i]],1,76),
                             destfile = paste(dir_save_bin,paste0(substr(basename(to.down[[i]]),1,21)),sep = "/"),handle = h),silent = T)
  on.exit(close(con))
  if(!inherits(t1, "try-error")){
    path=gunzip(paste(dir_save_bin,paste0(substr(basename(to.down[[i]]),1,21)),sep = "/"))
    con <- file(path, open = "rb")
    data <- try(readBin(con, numeric(), 3000 * 9000, 4, endian = "big"), silent = TRUE)
    

    if(!inherits(data, "try-error")){
      lon <- seq(0.02, 359.98, 0.04)
      lon <- ((lon + 180) %% 360) - 180
      lat <- seq(59.98, -59.98, -0.04)
      
      ox <- order(lon)
      oy <- order(lat)
      lon <- lon[ox]
      lat <- lat[oy]
      
      #ix <- lon >= zone.cilss@bbox[1,1] & lon <= zone.cilss@bbox[1,2]
      #iy <- lat >= zone.cilss@bbox[2,1] & lon <= zone.cilss@bbox[2,2]
      #lon <- lon[ix]
      #lat <- lat[iy]
      
      data[data == -9999] <- NA
      data[data == -9999.9] <- NA
      data[is.infinite(data)] <- NA
      data[data <0 ] <- NA
      data <- matrix(data, 9000, 3000)
      data <- data[ox, oy]
      #data <- data[ix, iy]
      data=data/100
      data=round(data,1)
      
      
      dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
      dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
      missval <- -99
      longname <- "Precipitation Estimation from Remotely Sensed Information using Artificial Neural Networks CCS"
      ncgrd <- ncdf4::ncvar_def("rfe", "mm", list(dx, dy), missval, longname, "float", compression = 9)
      data[is.na(data)] <- missval
      data=round(data,1)
      nc <- ncdf4::nc_create(paste(dir_save_nc,paste0("Persian_ccs_",substr(basename(to.down[[i]]),22,37)),sep = "/"), ncgrd)
      ncdf4::ncvar_put(nc, ncgrd, data)
      ncdf4::nc_close(nc)

    }
  }
}

#daily data computation
nb_cores <- detectCores() - 1
cl1 <- makeCluster(nb_cores)
registerDoParallel(cl1)

jdd=unique(substr(basename(to.down),22,32))

ret <- foreach(j = seq(1:length(jdd)), .combine = "c", .packages = c("ncdf4","raster")) %dopar% {

 #for(i in unique(substr(basename(to.down),22,32))){
    
    if(all(file.exists(paste(dir_save_nc,
                             paste0("Persian_ccs_",jdd[j],c("06","12","18"),".nc"),sep = "/"),
                       paste(dir_save_nc,paste0("Persian_ccs_",jdd[j+1],"00",".nc"),
                             sep = "/")))==T){
      
      pathh=c(paste(dir_save_nc,
                  paste0("Persian_ccs_",jdd[j],c("06","12","18"),".nc"),sep = "/"),
      paste(dir_save_nc,paste0("Persian_ccs_",jdd[j+1],"00",".nc"),
            sep = "/"))
      
      allrasters <- calc(stack(pathh,varname="rfe"),sum)
      crs(allrasters)="+proj=longlat +datum=WGS84 +no_defs"
      #crs(zone.afrique) <- crs(allrasters)
      x=raster::crop(allrasters,
                     zone.afrique,filename=paste(dir_save_tiff_cilss,
                                                 paste0("Persian_ccs_",substr(jdd[j],1,10),".tif"),sep = "/"),overwrite=T)
      
    
      #ecriture du fichier netcdf
      lon_lat=xyFromCell(x,cellsFromExtent(x,extent(x)))
      lon=unique(round(lon_lat[,1],4))
      lat=unique(round(lon_lat[,2],4))
      value=raster::as.matrix(x)
      mis_val=NAvalue(x)
      if(is.infinite(mis_val)==T){value[is.infinite(value)] <- NA}
      value[value == -99] <- NA
      value[value < 0] <- NA
      ox <- order(lon)
      oy <- order(lat)
      lon <- lon[ox]
      lat <- lat[oy]
      value=t(value)
      value <- value[ox, oy]
      
      
      dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
      dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
      missval <- -99
      longname <- "Precipitation Estimation from Remotely Sensed Information using Artificial Neural Networks CCS"
      ncgrd <- ncdf4::ncvar_def("rfe", "mm", list(dx, dy), missval, longname, "float", compression = 9)
      value[is.na(value)] <- missval
      nc <- ncdf4::nc_create(paste(dir_save_daily_nc,paste0("Persian_ccs_",substr(jdd[j],1,10),".nc"),sep = "/"), ncgrd)
      ncdf4::ncvar_put(nc, ncgrd, value)
      ncdf4::nc_close(nc)
      
      }
    
} 
stopCluster(cl1)
    
    
  #}



