### Script Pour le telechargement des donnees TAMSAT Version 3.1 ####
### Auteur: HOUNGNIBO Mandela, SONGOTI Henri, TRAORE Seydou ########

#packrat::init("F:/2020/Project_R/down_rfe_rea_sarrao")
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

#lien de telechargement des donnees
lnk.url<-"http://gws-access.jasmin.ac.uk/public/tamsat/rfe/data/v3.1/daily"

#definir les options de telechargement
#curl::handle_reset(handle)
h <- curl::new_handle()
curl::handle_setopt(h, low_speed_limit=30,low_speed_time=80)

#repertoire de sauvegarde
dir_save_nc <-"/data_cdt/SATELLITE_DATA/model_data/tamsat3.1/nc"
if(!dir.exists(dir_save_nc)){dir.create(dir_save_nc,recursive=TRUE)}
dir_save_tiff <-"/data_cdt/SATELLITE_DATA/model_data/tamsat3.1/tiff/Afrique"
if(!dir.exists(dir_save_tiff)){dir.create(dir_save_tiff,recursive=TRUE)}
dir_save_tiff_cilss <-"/data_cdt/SATELLITE_DATA/model_data/tamsat3.1/tiff/Cilss"
if(!dir.exists(dir_save_tiff_cilss)){dir.create(dir_save_tiff_cilss,recursive=TRUE)}

#pour un eventuel decoupage
zone.cilss <- as(extent(-20,30,0,30),"SpatialPolygons") 


#periode des donnees
start_date <- ymd("1983-01-01")
#todays <- ymd("1983-01-31")
todays <- today(tzone = "")-3

dd.tmp1<- do.call("rbind", strsplit(as.character(seq(start_date, todays, "day")), "-"))

all_link <-paste(lnk.url,dd.tmp1[, 1],dd.tmp1[, 2],
                 paste("rfe", dd.tmp1[, 1], "_", dd.tmp1[, 2], "_", dd.tmp1[, 3], ".v3.1.nc",sep = ""),sep = "/")

already.down <- list.files(dir_save_nc)

#Verification des fichiers avant telechargement 
to.down<- all_link[!(unlist(str_extract_all(unlist(all_link),
                                                     paste("rfe", dd.tmp1[, 1], "_", dd.tmp1[, 2], "_", dd.tmp1[, 3], ".v3.1.nc",sep = "")))%in%already.down)]

#telecharge fichiers
for(i in seq(to.down)){ 
t1=try(curl::curl_download(to.down[[i]],destfile = paste(dir_save_nc,basename(to.down[[i]]),sep = "/"),handle = h),silent = T)

if(!inherits(t1, "try-error")){
  #lecture et verification des rfe et rfe_filled (voir documentation tamsat)    
  nc.read.rfe=raster::raster(paste(dir_save_nc,basename(to.down[[i]]),sep = "/"),varname="rfe")
  nc.read_rfe.fill=raster::raster(paste(dir_save_nc,basename(to.down[[i]]),sep = "/"),varname="rfe_filled")
  
  #Ecriture des geotiffs
  if(any(!is.na(getValues(nc.read.rfe)))){ 
    raster::writeRaster(nc.read.rfe,paste(dir_save_tiff,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"),overwrite=TRUE)
    print("bonjour")
    print(paste(dir_save_tiff,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"))
    crs(zone.cilss) <- crs(nc.read.rfe)
    raster::crop(nc.read.rfe,zone.cilss,filename=paste(dir_save_tiff_cilss,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"),overwrite=TRUE)
    #raster::writeRaster(nc.read.rfe,paste(dir_save_tiff_cilss,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"),overwrite=TRUE)
    }else{
      raster::writeRaster(nc.read_rfe.fill,paste(dir_save_tiff,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"),overwrite=TRUE)
      print(paste(dir_save_tiff,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"))
      crs(zone.cilss) <- crs(nc.read_rfe.fill)
      raster::crop(nc.read_rfe.fill,zone.cilss,filename=paste(dir_save_tiff_cilss,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"),overwrite=TRUE)
      #raster::writeRaster(nc.read.rfe,paste(dir_save_tiff_cilss,stringr::str_replace(basename(to.down[[i]]),".v3.1.nc",".tif"),sep = "/"),overwrite=TRUE)
    }
  }
 }


