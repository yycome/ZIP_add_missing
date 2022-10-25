########################################################################################
# Project:  Assign PM component levels for zip codes that are missing in ESRI files    #
# Code: Step 2. Validate and compile                                                   #
# Author: Yaguang Wei                                                                  #
########################################################################################

############## 0. Setup load packages ###############
rm(list=ls())
gc()

library(magrittr)
library(dplyr)
library(stringr)
library(data.table)
library(lubridate)
library(zip)

dir_missingness <- '/media/qnap4/Yaguang/ZIPCODE_INFO/missing_zip_info/'
dir_save <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/'
dir_save_tmp <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/tmp/'
dir_save_tmp_processed <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/tmp_processed/'
dir_zip <- '/media/qnap4/heresh-predictions-combined/Zipcode_average/'

components <- c('br','ca','cu','ec','fe','k','nh4','ni','no3','oc','pb','si','so4','v','z')
years <- 2000:2019
regions <- c('NE','MA','NEC','WNC','SA','ESC','WSC','M','P')



############################# 1. Validate missing zipcodes ##############################
### 1.1 how many files?
zip_missing_files <- list.files(path=dir_save_tmp)
# length(zip_missing_files)
# [1] 2700


### 1.2 how many missing zipcodes?
zip_missing_validate <- data.frame(matrix(NA, nrow=0, ncol=6))
names(zip_missing_validate) <- c('component','year','region','n_zip','n_zip_unique','n_zip_missing')
for (component in components) {
  for (year in years) {
    for (region in regions) {
      zip_missing_file <- readRDS(paste0(dir_save_tmp,component,'_',year,'_',region,'.rds'))
      n_zip <- nrow(zip_missing_file)
      n_zip_unique <- length(unique(zip_missing_file$ZIP))
      n_zip_missing <- sum(is.na(zip_missing_file$br))
      zip_missing_validate[nrow(zip_missing_validate)+1,] = c(component,year,region,n_zip,n_zip_unique,n_zip_missing)
      rm(zip_missing_file,n_zip,n_zip_unique,n_zip_missing)
      gc()
    }
  }
}
zip_missing_validate$n_zip <- as.numeric(zip_missing_validate$n_zip)
zip_missing_validate$n_zip_unique <- as.numeric(zip_missing_validate$n_zip_unique)
zip_missing_validate$n_zip_missing <- as.numeric(zip_missing_validate$n_zip_missing)
summary(zip_missing_validate)
#  component             year              region              n_zip         n_zip_unique    n_zip_missing
# Length:2700        Length:2700        Length:2700        Min.   :  0.00   Min.   :  0.00   Min.   :0    
# Class :character   Class :character   Class :character   1st Qu.: 19.75   1st Qu.: 19.75   1st Qu.:0    
# Mode  :character   Mode  :character   Mode  :character   Median : 28.00   Median : 28.00   Median :0    
#                                                          Mean   : 32.67   Mean   : 32.67   Mean   :0    
#                                                          3rd Qu.: 40.00   3rd Qu.: 40.00   3rd Qu.:0    
#                                                          Max.   :161.00   Max.   :161.00   Max.   :0  


### 1.3 row-bind files for same component in same year
zip_missing_validate <- data.frame(matrix(NA, nrow=0, ncol=5))
names(zip_missing_validate) <- c('component','year','n_zip','n_zip_unique','n_zip_missing')
for (component in components) {
  for (year in years) {
    zip_missing_files <- list.files(path=dir_save_tmp,pattern = paste0("^",component,'_',year,"(.*).rds$"))
    zip_missing_file <- readRDS(paste0(dir_save_tmp,zip_missing_files[1])) 
    for (k in 2:length(zip_missing_files)) {
      zip_missing_file_tmp <- readRDS(paste0(dir_save_tmp,zip_missing_files[k])) 
      zip_missing_file <- rbind(zip_missing_file,zip_missing_file_tmp)
      rm(zip_missing_file_tmp)
    }
    n_zip <- nrow(zip_missing_file)
    n_zip_unique <- length(unique(zip_missing_file$ZIP))
    n_zip_missing <- sum(is.na(zip_missing_file$br))
    zip_missing_validate[nrow(zip_missing_validate)+1,] = c(component,year,n_zip,n_zip_unique,n_zip_missing)
    rm(zip_missing_file,n_zip,n_zip_unique,n_zip_missing)
    gc()
  }
}
zip_missing_validate$n_zip <- as.numeric(zip_missing_validate$n_zip)
zip_missing_validate$n_zip_unique <- as.numeric(zip_missing_validate$n_zip_unique)
zip_missing_validate$n_zip_missing <- as.numeric(zip_missing_validate$n_zip_missing)
summary(zip_missing_validate)
#  component             year               n_zip        n_zip_unique   n_zip_missing
# Length:300         Length:300         Min.   :  0.0   Min.   :  0.0   Min.   :0    
# Class :character   Class :character   1st Qu.:278.8   1st Qu.:272.2   1st Qu.:0    
# Mode  :character   Mode  :character   Median :301.5   Median :296.5   Median :0    
#                                       Mean   :294.0   Mean   :286.6   Mean   :0    
#                                       3rd Qu.:314.5   3rd Qu.:305.8   3rd Qu.:0    
#                                       Max.   :526.0   Max.   :487.0   Max.   :0    



############################# 2. Compile missing zipcodes ##############################
### 2.1 bind columns by year and region
for (year in years) {
  for (region in regions) {
    file_list_tmp <- list.files(path=dir_save_tmp,pattern = paste0("^(.*)_",year,"_",region,".rds"))
    dat_tmp_merged <- readRDS(paste0(dir_save_tmp,file_list_tmp[1])) 
    dat_tmp_merged <- dat_tmp_merged[!duplicated(dat_tmp_merged$ZIP),]
    for (k in 2:length(file_list_tmp)) {
      dat_tmp <- readRDS(paste0(dir_save_tmp,file_list_tmp[k])) 
      dat_tmp <- dat_tmp[!duplicated(dat_tmp$ZIP),]
      dat_tmp_merged <- left_join(dat_tmp_merged,dat_tmp,by='ZIP')
      rm(dat_tmp)
    }
    saveRDS(dat_tmp_merged,file=paste0(dir_save_tmp_processed,year,"_",region,".rds"))
    rm(file_list_tmp,dat_tmp_merged)
    gc()
  }
  cat(paste0("year ", year, " complete \n"))
}

### 2.2 bind rows by year 
for (year in years) {
  file_list_tmp <- list.files(path=dir_save_tmp_processed,pattern = paste0("^",year,"(.*).rds"))
  dat_tmp_merged <- readRDS(paste0(dir_save_tmp_processed,file_list_tmp[1]))
  dat_tmp_merged <- dat_tmp_merged[!duplicated(dat_tmp_merged$ZIP),]
  for (k in 2:length(file_list_tmp)) {
    dat_tmp <- readRDS(paste0(dir_save_tmp_processed,file_list_tmp[k])) 
    dat_tmp <- dat_tmp[!duplicated(dat_tmp$ZIP),]
    dat_tmp_merged <- bind_rows(dat_tmp_merged,dat_tmp)
    rm(dat_tmp)
  }
  dat_tmp_merged <- dat_tmp_merged[!duplicated(dat_tmp_merged$ZIP),]
  saveRDS(dat_tmp_merged,file=paste0(dir_save_tmp_processed,year,".rds"))
  rm(file_list_tmp,dat_tmp_merged)
  gc()
  cat(paste0("year ", year, " complete \n"))
}



############################# 3. Add to existing zipcodes ##############################
for (year in years) {
  zip_existing <- readRDS(paste0(dir_zip,year,'.rds'))
  zip_to_add <- readRDS(paste0(dir_save_tmp_processed,year,'.rds'))
  zip_combined <- rbind(zip_existing,zip_to_add)
  zip_combined <- zip_combined[!duplicated(zip_combined$ZIP),]
  zip_combined <- zip_combined[order(zip_combined$ZIP),]
  saveRDS(zip_combined,file=paste0(dir_save_tmp_processed,year,".rds"))
  rm(zip_existing,zip_to_add,zip_combined)
  gc()
}



############################# 4. How many zipcodes in each year ##############################
for (year in years) {
  zip_combined <- readRDS(paste0(dir_save_tmp_processed,year,'.rds'))
  print(nrow(zip_combined))
  rm(zip_combined)
  gc()
}
# [1] 41470
# [1] 41511
# [1] 41320
# [1] 41315
# [1] 41345
# [1] 41259
# [1] 41234
# [1] 41203
# [1] 41038
# [1] 41055
# [1] 40877
# [1] 41133
# [1] 41113
# [1] 40999
# [1] 40964
# [1] 40935
# [1] 40860
# [1] 40828
# [1] 40752
# [1] 40413