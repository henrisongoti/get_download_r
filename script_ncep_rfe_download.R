### HOUNGNIBO MANDELA- SONGOTI HENRI- SEYDOU TRAORE 10/2020 #####
rm(list=ls())
packages <- c("lubridate", "stringr","rgdal","raster","ncdf4","curl","R.utils")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#library
require(lubridate)
require(stringr)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(raster)
library(ncdf4)
library(curl)
library(R.utils)

#lien de telechargement des donnees
lnk.url<-"ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/rfe2/geotiff"
#link.url<-"https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/africa/daily/rfe/downloads/daily"

#definir les options de telechargement
#curl::handle_reset(handle)
h <- curl::new_handle()
curl::handle_setopt(h, low_speed_limit=30,low_speed_time=180)

#repertoire de sauvegarde
dir_save_nc <-"/data_cdt/SATELLITE_DATA/model_data/rfe_ncep/nc"
if(!dir.exists(dir_save_nc)){dir.create(dir_save_nc,recursive=TRUE)}

dir_save_tiff_afrik <-"/data_cdt/SATELLITE_DATA/model_data/rfe_ncep/tiff/Afrique"
if(!dir.exists(dir_save_tiff_afrik)){dir.create(dir_save_tiff_afrik,recursive=TRUE)}
# dir_save_tiff <-"F:/2020/data/chirp/tiff/Afrique"
# if(!dir.exists(dir_save_tiff)){dir.create(dir_save_tiff,recursive=TRUE)}
dir_save_tiff_cilss <-"/data_cdt/SATELLITE_DATA/model_data/rfe_ncep/tiff/Cilss"
if(!dir.exists(dir_save_tiff_cilss)){dir.create(dir_save_tiff_cilss,recursive=TRUE)}

#pour un eventuel decoupage
zone.cilss <- as(extent(-20,30,0,30),"SpatialPolygons")


#periode des donnees
start_date <- ymd("2001-01-01")
#todays <- ymd("1983-01-31")
todays <- today(tzone = "")

dd.tmp1<- do.call("rbind", strsplit(as.character(seq(start_date, todays, "day")), "-"))

all_link <-paste(lnk.url,
                 paste("africa_rfe.", dd.tmp1[, 1], dd.tmp1[, 2], dd.tmp1[, 3], ".tif.zip",sep = ""),sep = "/")

already.down <- list.files(dir_save_tiff_afrik)

#Verification des fichiers avant telechargement 

to.down<- all_link[!(unlist(str_extract_all(unlist(all_link),
                                            paste("africa_rfe.", dd.tmp1[, 1], dd.tmp1[, 2],  dd.tmp1[, 3], ".tif.zip",sep = "")))%in%already.down)]

for(i in seq(to.down)){ 
  
  dest=paste0(
    gsub("\\.","_",substr(basename(to.down[[i]]),1,11)),
    substr(basename(to.down[[i]]),12,15),"_",
    substr(basename(to.down[[i]]),16,17),"_",
    substr(basename(to.down[[i]]),18,19),
    substr(basename(to.down[[i]]),20,nchar(basename(to.down[[i]])))
  )
  
  t1=try(curl::curl_download(to.down[[i]],destfile = paste(dir_save_tiff_afrik,dest,sep = "/"),handle = h),silent = T)
  
  if(!inherits(t1, "try-error")){
    path=unzip(paste(dir_save_tiff_afrik,dest,sep = "/"),exdir = dir_save_tiff_afrik)
    nc.read.rfe=raster::raster(path)
    NAvalue(nc.read.rfe)<--9999
    crs(nc.read.rfe)="+proj=longlat +datum=WGS84 +no_defs"
    crs(zone.cilss) <- crs(nc.read.rfe)
    raster::crop(nc.read.rfe,zone.cilss,filename=paste(dir_save_tiff_cilss,gsub("[.]zip$", "",dest),sep = "/"),overwrite=TRUE)
 
    #ecriture du fichier netcdf
    lon_lat=xyFromCell(nc.read.rfe,cellsFromExtent(nc.read.rfe,extent(nc.read.rfe)))
    lon=unique(round(lon_lat[,1],1))
    lat=unique(round(lon_lat[,2],1))
    value=raster::as.matrix(nc.read.rfe)
    mis_val=NAvalue(nc.read.rfe)
    if(is.infinite(mis_val)==T){value[is.infinite(value)] <- NA}
    value[value == -9999] <- NA
    ox <- order(lon)
    oy <- order(lat)
    lon <- lon[ox]
    lat <- lat[oy]
    value=t(value)
    value <- value[ox, oy]
    
    
    dx <- ncdf4::ncdim_def("lon", "degreeE", lon, longname = "Longitude")
    dy <- ncdf4::ncdim_def("lat", "degreeN", lat, longname = "Latitude")
    missval <- -99
    longname <- "Precipitation Estimation rfe ncep"
    ncgrd <- ncdf4::ncvar_def("rfe", "mm", list(dx, dy), missval, longname, "float", compression = 9)
    value[is.na(value)] <- missval
    nc <- ncdf4::nc_create(paste(dir_save_nc,paste0(gsub("[.]tif.zip$", "",dest),".nc"),sep = "/"), ncgrd)
    ncdf4::ncvar_put(nc, ncgrd, value)
    ncdf4::nc_close(nc)
    
     }
}


actual.now=list.files(dir_save_tiff_afrik,pattern ="[.]tif.zip$",full.names = T )
delete.ca=actual.now[which(file.size(actual.now)<20)]
unlink(delete.ca)
unlink(list.files(dir_save_tiff_afrik,pattern ="[.]tif$",full.names = T ))
