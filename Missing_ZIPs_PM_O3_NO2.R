#######################################################################################
# Project:  Assign PM, O3, NO2 levels for zip codes that are missing in ESRI files    #
# Author: Yaguang Wei                                                                 #
#######################################################################################

############## 0. Setup load packages ###############
rm(list=ls())
gc()

library(gdalUtils)
library(devtools)
library(magrittr)
library(dplyr)
library(stringr)
library(raster)
library(data.table)
library(nabor)
library(lubridate)
library(zip)

dir_shp <- '/media/qnap4/Yaguang/ZIPCODE_INFO/polygon/'
dir_pobox <- '/media/qnap4/Yaguang/ZIPCODE_INFO/pobox_csv/'
dir_missingness <- '/media/qnap4/Yaguang/ZIPCODE_INFO/missing_zip_info/'
dir_save <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/'
dir_save_tmp <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/tmp/'
dir_pm_grid_daily <- '/media/qnap3/Exposure modeling/3 National scale/USA/4 PM2.5 v2.2000_16/7 Final predictions 1 km/Daily/'
dir_o3_grid_daily <- '/media/qnap3/Exposure modeling/3 National scale/USA/6 O3 vx.2000_16/7 Final predictions 1 km/Daily/'
dir_pm_zip_daily <- '/media/qnap3/Exposure modeling/3 National scale/USA/4 PM2.5 v2.2000_16/9 Zipcode average/Daily/'
dir_o3_zip_daily <- '/media/qnap4/Zipcode_average_exposures/O3/Daily/'

years_char <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16')



############## 1. missing zip code info ###############
zip_missing <- read.csv(paste0(dir_missingness,'Valid ZIP without Exposure.csv'))
zip_missing$ZIP <- as.character(zip_missing$ZIP)
zip_missing$ZIP <- str_pad(zip_missing$ZIP, width=5, side="left", pad="0")
zip_missing_coord <- readRDS(paste0(dir_missingness,'exposure_missing_zip_info_1221.rds'))

zip_missing <- left_join(zip_missing,zip_missing_coord,by=c('ZIP'='zip'))
saveRDS(zip_missing, file = paste0(dir_save,'zip_missing_info.rds'))



############## 2.1 update daily zip code-level PM2.5 ###############
zip_missing <- readRDS(paste0(dir_save,'zip_missing_info.rds'))

### link nearest grid cell id
sitecode_pm <- readRDS(paste0(dir_pm_grid_daily,'USGridSite.rds'))
link_pm <- nabor::knn(sitecode_pm[,c("Lon","Lat")],zip_missing[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
link_pm <- cbind.data.frame(link_pm$nn.idx,link_pm$nn.dists)
names(link_pm) <- c("grid_id_pm","dis")
zip_missing_pm <- cbind.data.frame(zip_missing,link_pm)
zip_missing_pm <- zip_missing_pm[,c("ZIP","year","grid_id_pm")]
zip_missing_pm <- zip_missing_pm[complete.cases(zip_missing_pm),]
zip_missing_pm <- zip_missing_pm[zip_missing_pm$grid_id_pm!=0,]

### update daily zip code files
pm_grid_daily_files <- list.files(path=dir_pm_grid_daily,pattern = "^PredictionStep2_PM25_USGrid_20(.*)rds$")
pm_zip_daily_files <- list.files(path=dir_pm_zip_daily,pattern = "^20(.*)rds$")
for (i in 1:length(pm_grid_daily_files)) {
  pm_grid_daily_tmp <- readRDS(paste0(dir_pm_grid_daily,pm_grid_daily_files[i]))
  pm_zip_daily_tmp <- readRDS(paste0(dir_pm_zip_daily,pm_zip_daily_files[i]))
  
  year_tmp <- as.numeric(substr(pm_zip_daily_files[i],start=1,stop=4))
  zip_missing_pm_tmp <- zip_missing_pm[zip_missing_pm$year==year_tmp,]
  
  zip_missing_pm_to_add_tmp <- setdiff(zip_missing_pm_tmp$ZIP, pm_zip_daily_tmp$ZIP)
  zip_missing_info_pm_to_add_tmp <- zip_missing_pm_tmp[zip_missing_pm_tmp$ZIP%in%zip_missing_pm_to_add_tmp,]
  
  pm_zip_to_add_tmp <- data.frame(ZIP=zip_missing_info_pm_to_add_tmp$ZIP,
                                  pm25=pm_grid_daily_tmp[1,zip_missing_info_pm_to_add_tmp$grid_id_pm])
  pm_zip_daily_tmp <- rbind(pm_zip_daily_tmp,pm_zip_to_add_tmp)
  pm_zip_daily_tmp <- pm_zip_daily_tmp[order(pm_zip_daily_tmp$ZIP),]
  pm_zip_daily_tmp <- pm_zip_daily_tmp[complete.cases(pm_zip_daily_tmp),]
  saveRDS(pm_zip_daily_tmp,file=paste0(dir_save_tmp,pm_zip_daily_files[i]))
  
  rm(pm_grid_daily_tmp,pm_zip_daily_tmp,year_tmp,zip_missing_pm_tmp,zip_missing_pm_to_add_tmp,
     zip_missing_info_pm_to_add_tmp,pm_zip_to_add_tmp)
  gc()
  
  if (i%%10 == 0) cat(paste0('Processed ', i, ' of ', length(pm_grid_daily_files),'\n'))
}

### validate: missingness and duplicates
pm_files <- list.files(path=dir_save_tmp)
n_zip_missing <- data.frame(matrix(NA, nrow=length(pm_files), ncol=4))
names(n_zip_missing) <- c('date','n_zip','n_missing','n_duplicate')
for (i in 1:length(pm_files)) {
  dat_tmp <- readRDS(paste0(dir_save_tmp,pm_files[i]))
  n_zip_missing[i,1] <- ymd(gsub('([0-9]+).*$','\\1',pm_files[i]))
  n_zip_missing[i,2] <- dim(dat_tmp)[1]
  n_zip_missing[i,3] <- sum(is.na(dat_tmp$pm25))
  n_zip_missing[i,4] <- dim(dat_tmp)[1]-length(unique(dat_tmp$ZIP))
  rm(dat_tmp)
  gc()
  
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
n_zip_missing$date <- as.Date(n_zip_missing$date,origin = "1970-01-01")
summary(n_zip_missing)
#     date                n_zip         n_missing  n_duplicate
# Min.   :2000-01-01   Min.   :40259   Min.   :0   Min.   :0   
# 1st Qu.:2004-04-01   1st Qu.:41131   1st Qu.:0   1st Qu.:0   
# Median :2008-07-01   Median :41252   Median :0   Median :0   
# Mean   :2008-07-01   Mean   :41260   Mean   :0   Mean   :0   
# 3rd Qu.:2012-09-30   3rd Qu.:41373   3rd Qu.:0   3rd Qu.:0   
# Max.   :2016-12-31   Max.   :41548   Max.   :0   Max.   :0  

### compress daily files by year
setwd(dir_save_tmp)
for (i in 1:length(years_char)) {
  pm_daily_files_tmp <- list.files(path=dir_save_tmp,pattern = paste0("^20",years_char[i],"(.*)rds$"))
  zip::zipr(zipfile = paste0(dir_save_tmp,"PM25_US_ZipcodeAverage_daily_20",years_char[i],".zip"), files = pm_daily_files_tmp)
  rm(pm_daily_files_tmp)
  gc()
}



############## 2.2 update annual zip code-level PM2.5 ###############
for (i in 1:length(years_char)) {
  pm_files_tmp <- Sys.glob(paste0(dir_save_tmp,"20",years_char[i],"*.rds"))
  pm_files_ls_tmp <- lapply(pm_files_tmp,readRDS)
  pm_daily_merged_tmp <- rbindlist(pm_files_ls_tmp)
  pm_annual_tmp <- pm_daily_merged_tmp %>% dplyr::group_by(ZIP) %>% dplyr::summarize(pm25=mean(pm25,na.rm=TRUE))
  saveRDS(pm_annual_tmp,file = paste0(dir_save_tmp,'20',years_char[i],'.rds'))
  rm(pm_files_tmp,pm_files_ls_tmp,pm_daily_merged_tmp,pm_annual_tmp)
  gc()
}

### validate: missingness and duplicates
pm_files <- list.files(path=dir_save_tmp)
n_zip_missing <- data.frame(matrix(NA, nrow=length(pm_files), ncol=4))
names(n_zip_missing) <- c('date','n_zip','n_missing','n_duplicate')
for (i in 1:length(pm_files)) {
  dat_tmp <- readRDS(paste0(dir_save_tmp,pm_files[i]))
  n_zip_missing[i,1] <- gsub('([0-9]+).*$','\\1',pm_files[i])
  n_zip_missing[i,2] <- dim(dat_tmp)[1]
  n_zip_missing[i,3] <- sum(is.na(dat_tmp$pm25))
  n_zip_missing[i,4] <- dim(dat_tmp)[1]-length(unique(dat_tmp$ZIP))
  rm(dat_tmp)
  gc()
}
n_zip_missing$date <- as.numeric(n_zip_missing$date)
summary(n_zip_missing)
#      date          n_zip         n_missing  n_duplicate
# Min.   :2000   Min.   :40977   Min.   :0   Min.   :0   
# 1st Qu.:2004   1st Qu.:41131   1st Qu.:0   1st Qu.:0   
# Median :2008   Median :41252   Median :0   Median :0   
# Mean   :2008   Mean   :41260   Mean   :0   Mean   :0   
# 3rd Qu.:2012   3rd Qu.:41373   3rd Qu.:0   3rd Qu.:0   
# Max.   :2016   Max.   :41548   Max.   :0   Max.   :0   



############## 3.1 update daily zip code-level O3 ###############
zip_missing <- readRDS(paste0(dir_save,'zip_missing_info.rds'))

### link nearest grid cell id
sitecode_o3 <- readRDS(paste0(dir_o3_grid_daily,'USGridSite_O3.rds'))
link_o3 <- nabor::knn(sitecode_o3[,c("Lon","Lat")],zip_missing[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
link_o3 <- cbind.data.frame(link_o3$nn.idx,link_o3$nn.dists)
names(link_o3) <- c("grid_id_o3","dis")
zip_missing_o3 <- cbind.data.frame(zip_missing,link_o3)
zip_missing_o3 <- zip_missing_o3[,c("ZIP","year","grid_id_o3")]
zip_missing_o3 <- zip_missing_o3[complete.cases(zip_missing_o3),]
zip_missing_o3 <- zip_missing_o3[zip_missing_o3$grid_id_o3!=0,]

### update daily zip code files
o3_grid_daily_files <- list.files(path=dir_o3_grid_daily,pattern = "^Ensemble_predictions_O3_USGrid_20(.*)rds$")
o3_zip_daily_files <- list.files(path=dir_o3_zip_daily,pattern = "^20(.*)rds$")
for (i in 1:length(o3_grid_daily_files)) {
  o3_grid_daily_tmp <- readRDS(paste0(dir_o3_grid_daily,o3_grid_daily_files[i]))
  o3_zip_daily_tmp <- readRDS(paste0(dir_o3_zip_daily,o3_zip_daily_files[i]))
  
  year_tmp <- as.numeric(substr(o3_zip_daily_files[i],start=1,stop=4))
  zip_missing_o3_tmp <- zip_missing_o3[zip_missing_o3$year==year_tmp,]
  
  zip_missing_o3_to_add_tmp <- setdiff(zip_missing_o3_tmp$ZIP, o3_zip_daily_tmp$ZIP)
  zip_missing_info_o3_to_add_tmp <- zip_missing_o3_tmp[zip_missing_o3_tmp$ZIP%in%zip_missing_o3_to_add_tmp,]
  
  o3_zip_to_add_tmp <- data.frame(ZIP=zip_missing_info_o3_to_add_tmp$ZIP,
                                  ozone=o3_grid_daily_tmp[zip_missing_info_o3_to_add_tmp$grid_id_o3])
  o3_zip_daily_tmp <- rbind(o3_zip_daily_tmp,o3_zip_to_add_tmp)
  o3_zip_daily_tmp <- o3_zip_daily_tmp[order(o3_zip_daily_tmp$ZIP),]
  o3_zip_daily_tmp <- o3_zip_daily_tmp[complete.cases(o3_zip_daily_tmp),]
  saveRDS(o3_zip_daily_tmp,file=paste0(dir_save_tmp,o3_zip_daily_files[i]))
  
  rm(o3_grid_daily_tmp,o3_zip_daily_tmp,year_tmp,zip_missing_o3_tmp,zip_missing_o3_to_add_tmp,
     zip_missing_info_o3_to_add_tmp,o3_zip_to_add_tmp)
  gc()
  
  if (i%%10 == 0) cat(paste0('Processed ', i, ' of ', length(o3_grid_daily_files),'\n'))
}

### validate: missingness and duplicates
o3_files <- list.files(path=dir_save_tmp)
n_zip_missing <- data.frame(matrix(NA, nrow=length(o3_files), ncol=4))
names(n_zip_missing) <- c('date','n_zip','n_missing','n_duplicate')
for (i in 1:length(o3_files)) {
  dat_tmp <- readRDS(paste0(dir_save_tmp,o3_files[i]))
  n_zip_missing[i,1] <- ymd(gsub('([0-9]+).*$','\\1',o3_files[i]))
  n_zip_missing[i,2] <- dim(dat_tmp)[1]
  n_zip_missing[i,3] <- sum(is.na(dat_tmp$ozone))
  n_zip_missing[i,4] <- dim(dat_tmp)[1]-length(unique(dat_tmp$ZIP))
  rm(dat_tmp)
  gc()
  
  if( i %% 100 == 0 ) cat(paste("iteration", i, "complete\n"))
}
n_zip_missing$date <- as.Date(n_zip_missing$date,origin = "1970-01-01")
summary(n_zip_missing)
#      date                n_zip         n_missing  n_duplicate
# Min.   :2000-01-01   Min.   :40977   Min.   :0   Min.   :0   
# 1st Qu.:2004-04-01   1st Qu.:41131   1st Qu.:0   1st Qu.:0   
# Median :2008-07-01   Median :41252   Median :0   Median :0   
# Mean   :2008-07-01   Mean   :41260   Mean   :0   Mean   :0   
# 3rd Qu.:2012-09-30   3rd Qu.:41373   3rd Qu.:0   3rd Qu.:0   
# Max.   :2016-12-31   Max.   :41548   Max.   :0   Max.   :0   

### compress daily files by year
setwd(dir_save_tmp)
for (i in 1:length(years_char)) {
  o3_daily_files_tmp <- list.files(path=dir_save_tmp,pattern = paste0("^20",years_char[i],"(.*)rds$"))
  zip::zipr(zipfile = paste0(dir_save_tmp,"O3_US_ZipcodeAverage_daily_20",years_char[i],".zip"), files = o3_daily_files_tmp)
  rm(o3_daily_files_tmp)
  gc()
}



############## 3.2 update annual zip code-level PM2.5 ###############
for (i in 1:length(years_char)) {
  o3_files_tmp <- Sys.glob(paste0(dir_save_tmp,"20",years_char[i],"*.rds"))
  o3_files_ls_tmp <- lapply(o3_files_tmp,readRDS)
  o3_daily_merged_tmp <- rbindlist(o3_files_ls_tmp)
  o3_annual_tmp <- o3_daily_merged_tmp %>% dplyr::group_by(ZIP) %>% dplyr::summarize(ozone=mean(ozone,na.rm=TRUE))
  saveRDS(o3_annual_tmp,file = paste0(dir_save_tmp,'20',years_char[i],'.rds'))
  rm(o3_files_tmp,o3_files_ls_tmp,o3_daily_merged_tmp,o3_annual_tmp)
  gc()
}

### validate: missingness and duplicates
o3_files <- list.files(path=dir_save_tmp)
n_zip_missing <- data.frame(matrix(NA, nrow=length(o3_files), ncol=4))
names(n_zip_missing) <- c('date','n_zip','n_missing','n_duplicate')
for (i in 1:length(o3_files)) {
  dat_tmp <- readRDS(paste0(dir_save_tmp,o3_files[i]))
  n_zip_missing[i,1] <- gsub('([0-9]+).*$','\\1',o3_files[i])
  n_zip_missing[i,2] <- dim(dat_tmp)[1]
  n_zip_missing[i,3] <- sum(is.na(dat_tmp$ozone))
  n_zip_missing[i,4] <- dim(dat_tmp)[1]-length(unique(dat_tmp$ZIP))
  rm(dat_tmp)
  gc()
}
n_zip_missing$date <- as.numeric(n_zip_missing$date)
summary(n_zip_missing)
#      date          n_zip         n_missing  n_duplicate
# Min.   :2000   Min.   :40977   Min.   :0   Min.   :0   
# 1st Qu.:2004   1st Qu.:41131   1st Qu.:0   1st Qu.:0   
# Median :2008   Median :41252   Median :0   Median :0   
# Mean   :2008   Mean   :41260   Mean   :0   Mean   :0   
# 3rd Qu.:2012   3rd Qu.:41373   3rd Qu.:0   3rd Qu.:0   
# Max.   :2016   Max.   :41548   Max.   :0   Max.   :0   