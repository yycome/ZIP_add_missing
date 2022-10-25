########################################################################################
# Project:  Assign PM component levels for zip codes that are missing in ESRI files    #
# Code: Step 1. Identify missing zipcodes in Mid Atlantic                              #
# Author: Yaguang Wei                                                                  #
########################################################################################

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

dir_missingness <- '/media/qnap4/Yaguang/ZIPCODE_INFO/missing_zip_info/'
dir_save <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/'
dir_save_tmp <- '/media/qnap4/Yaguang/Pollutants_Missing_ZIPCODES/tmp/'
dir_grid_urban <- '/media/qnap4/heresh-predictions-combined/2 Mid-Atlantic/Urban areas at 50m spatial resolution/'
dir_grid_nonurban <- '/media/qnap4/heresh-predictions-combined/2 Mid-Atlantic/Non-urban areas at 1km spatial resolution/'
dir_zip <- '/media/qnap4/heresh-predictions-combined/Zipcode_average/'

years_char <- c('00','01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19')

zip_missing <- readRDS(paste0(dir_save,'zip_missing_info.rds'))



############################# 1. br ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'br_20',i,'_MA.rds'))){
    ### aggregated file
    br_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    br_zip_tmp <- br_zip_tmp[,c('ZIP','br')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, br_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    br_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.br.rds'))
    br_grid_urban <- br_grid_urban[,c("lon","lat","final.predicted.br")]
    br_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.br.rds'))
    br_grid_nonurban <- br_grid_nonurban[br_grid_nonurban$year==paste0('20',i),]
    br_grid_nonurban <- br_grid_nonurban[,c("lon","lat","final.predicted.br")]
    br_grid <- rbind(br_grid_urban,br_grid_nonurban)
    
    ### aggregate for po box
    link_br <- nabor::knn(br_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_br <- cbind.data.frame(link_br$nn.idx,link_br$nn.dists)
    names(link_br) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_br)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### save
    br_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,br=br_grid[zip_missing_tmp$id,'final.predicted.br'])
    saveRDS(br_zip_to_add_tmp,file=paste0(dir_save_tmp,'br_20',i,'_MA.rds'))
    
    rm(br_zip_tmp,zip_missing_tmp,br_grid_urban,br_grid,br_grid_nonurban,link_br,br_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 2. ca ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'ca_20',i,'_MA.rds'))){
    ### aggregated file
    ca_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    ca_zip_tmp <- ca_zip_tmp[,c('ZIP','ca')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, ca_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    ca_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.ca.rds'))
    ca_grid_urban <- ca_grid_urban[,c("lon","lat","final.predicted.ca")]
    ca_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.ca.rds'))
    ca_grid_nonurban <- ca_grid_nonurban[ca_grid_nonurban$year==paste0('20',i),]
    ca_grid_nonurban <- ca_grid_nonurban[,c("lon","lat","final.predicted.ca")]
    ca_grid <- rbind(ca_grid_urban,ca_grid_nonurban)
    
    ### aggregate for po box
    link_ca <- nabor::knn(ca_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_ca <- cbind.data.frame(link_ca$nn.idx,link_ca$nn.dists)
    names(link_ca) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_ca)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    ca_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,ca=ca_grid[zip_missing_tmp$id,'final.predicted.ca'])
    saveRDS(ca_zip_to_add_tmp,file=paste0(dir_save_tmp,'ca_20',i,'_MA.rds'))
    
    rm(ca_zip_tmp,zip_missing_tmp,ca_grid_urban,ca_grid,ca_grid_nonurban,link_ca,ca_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 3. cu ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'cu_20',i,'_MA.rds'))){
    ### aggregated file
    cu_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    cu_zip_tmp <- cu_zip_tmp[,c('ZIP','cu')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, cu_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    cu_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.cu.rds'))
    cu_grid_urban <- cu_grid_urban[,c("lon","lat","final.predicted.cu")]
    cu_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.cu.rds'))
    cu_grid_nonurban <- cu_grid_nonurban[cu_grid_nonurban$year==paste0('20',i),]
    cu_grid_nonurban <- cu_grid_nonurban[,c("lon","lat","final.predicted.cu")]
    cu_grid <- rbind(cu_grid_urban,cu_grid_nonurban)
    
    ### aggregate for po box
    link_cu <- nabor::knn(cu_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_cu <- cbind.data.frame(link_cu$nn.idx,link_cu$nn.dists)
    names(link_cu) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_cu)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    cu_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,cu=cu_grid[zip_missing_tmp$id,'final.predicted.cu'])
    saveRDS(cu_zip_to_add_tmp,file=paste0(dir_save_tmp,'cu_20',i,'_MA.rds'))
    
    rm(cu_zip_tmp,zip_missing_tmp,cu_grid_urban,cu_grid,cu_grid_nonurban,link_cu,cu_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 4. ec ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'ec_20',i,'_MA.rds'))){
    ### aggregated file
    ec_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    ec_zip_tmp <- ec_zip_tmp[,c('ZIP','ec')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, ec_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    ec_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.ec.rds'))
    ec_grid_urban <- ec_grid_urban[,c("lon","lat","final.predicted.ec")]
    ec_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.ec.rds'))
    ec_grid_nonurban <- ec_grid_nonurban[ec_grid_nonurban$year==paste0('20',i),]
    ec_grid_nonurban <- ec_grid_nonurban[,c("lon","lat","final.predicted.ec")]
    ec_grid <- rbind(ec_grid_urban,ec_grid_nonurban)
    
    ### aggregate for po box
    link_ec <- nabor::knn(ec_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_ec <- cbind.data.frame(link_ec$nn.idx,link_ec$nn.dists)
    names(link_ec) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_ec)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    ec_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,ec=ec_grid[zip_missing_tmp$id,'final.predicted.ec'])
    saveRDS(ec_zip_to_add_tmp,file=paste0(dir_save_tmp,'ec_20',i,'_MA.rds'))
    
    rm(ec_zip_tmp,zip_missing_tmp,ec_grid_urban,ec_grid,ec_grid_nonurban,link_ec,ec_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 5. fe ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'fe_20',i,'_MA.rds'))){
    ### aggregated file
    fe_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    fe_zip_tmp <- fe_zip_tmp[,c('ZIP','fe')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, fe_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    fe_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.fe.rds'))
    fe_grid_urban <- fe_grid_urban[,c("lon","lat","final.predicted.fe")]
    fe_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.fe.rds'))
    fe_grid_nonurban <- fe_grid_nonurban[fe_grid_nonurban$year==paste0('20',i),]
    fe_grid_nonurban <- fe_grid_nonurban[,c("lon","lat","final.predicted.fe")]
    fe_grid <- rbind(fe_grid_urban,fe_grid_nonurban)
    
    ### aggregate for po box
    link_fe <- nabor::knn(fe_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_fe <- cbind.data.frame(link_fe$nn.idx,link_fe$nn.dists)
    names(link_fe) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_fe)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    fe_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,fe=fe_grid[zip_missing_tmp$id,'final.predicted.fe'])
    saveRDS(fe_zip_to_add_tmp,file=paste0(dir_save_tmp,'fe_20',i,'_MA.rds'))
    
    rm(fe_zip_tmp,zip_missing_tmp,fe_grid_urban,fe_grid,fe_grid_nonurban,link_fe,fe_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 6. k ##############################
## for years 01-04 and 06-19
years_char_k <- c('00','01','02','03','04','06','07','08','09','10','11','12','13','14','15','16','17','18','19')

for (i in years_char_k) {
  if (!file.exists(paste0(dir_save_tmp,'k_20',i,'_MA.rds'))){
    ### aggregated file
    k_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    k_zip_tmp <- k_zip_tmp[,c('ZIP','k')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, k_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    k_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.k.rds'))
    k_grid_urban <- k_grid_urban[,c("lon","lat","final.predicted.k")]
    k_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.k.rds'))
    k_grid_nonurban <- k_grid_nonurban[k_grid_nonurban$year==paste0('20',i),]
    k_grid_nonurban <- k_grid_nonurban[,c("lon","lat","final.predicted.k")]
    k_grid <- rbind(k_grid_urban,k_grid_nonurban)
    
    ### aggregate for po box
    link_k <- nabor::knn(k_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_k <- cbind.data.frame(link_k$nn.idx,link_k$nn.dists)
    names(link_k) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_k)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    k_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,k=k_grid[zip_missing_tmp$id,'final.predicted.k'])
    saveRDS(k_zip_to_add_tmp,file=paste0(dir_save_tmp,'k_20',i,'_MA.rds'))
    
    rm(k_zip_tmp,zip_missing_tmp,k_grid_urban,k_grid,k_grid_nonurban,link_k,k_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}


## for year 05
year_char_k <- c('05')

for (i in year_char_k) {
  i='05'
  if (!file.exists(paste0(dir_save_tmp,'k_20',i,'_MA.rds'))){
    ### aggregated file
    k_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    k_zip_tmp <- k_zip_tmp[,c('ZIP','k')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, k_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    k_grid_urban_files <- list.files(path=dir_grid_urban,pattern = paste0("^20",i,".final.predictions.urban.MA2.50m.k.(.*)rds$"))
    k_grid_urban <- readRDS(paste0(dir_grid_urban,k_grid_urban_files[1]))
    for (j in 2:length(k_grid_urban_files)) {
      k_grid_urban_tmp <- readRDS(paste0(dir_grid_urban,k_grid_urban_files[j]))
      k_grid_urban <- bind_rows(k_grid_urban,k_grid_urban_tmp)
      rm(k_grid_urban_tmp)
      gc()
    }
    k_grid_urban <- k_grid_urban[,c("lon","lat","final.predicted.k")]
    k_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.k.rds'))
    k_grid_nonurban <- k_grid_nonurban[k_grid_nonurban$year==paste0('20',i),]
    k_grid_nonurban <- k_grid_nonurban[,c("lon","lat","final.predicted.k")]
    k_grid <- rbind(k_grid_urban,k_grid_nonurban)
    
    ### aggregate for po box
    link_k <- nabor::knn(k_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_k <- cbind.data.frame(link_k$nn.idx,link_k$nn.dists)
    names(link_k) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_k)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    k_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,k=k_grid[zip_missing_tmp$id,'final.predicted.k'])
    saveRDS(k_zip_to_add_tmp,file=paste0(dir_save_tmp,'k_20',i,'_MA.rds'))
    
    rm(k_zip_tmp,zip_missing_tmp,k_grid_urban,k_grid,k_grid_nonurban,link_k,k_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 7. nh4 ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'nh4_20',i,'_MA.rds'))){
    ### aggregated file
    nh4_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    nh4_zip_tmp <- nh4_zip_tmp[,c('ZIP','nh4')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, nh4_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    nh4_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.nh4.rds'))
    nh4_grid_urban <- nh4_grid_urban[,c("lon","lat","final.predicted.nh4")]
    nh4_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.nh4.rds'))
    nh4_grid_nonurban <- nh4_grid_nonurban[nh4_grid_nonurban$year==paste0('20',i),]
    nh4_grid_nonurban <- nh4_grid_nonurban[,c("lon","lat","final.predicted.nh4")]
    nh4_grid <- rbind(nh4_grid_urban,nh4_grid_nonurban)
    
    ### aggregate for po box
    link_nh4 <- nabor::knn(nh4_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_nh4 <- cbind.data.frame(link_nh4$nn.idx,link_nh4$nn.dists)
    names(link_nh4) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_nh4)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    nh4_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,nh4=nh4_grid[zip_missing_tmp$id,'final.predicted.nh4'])
    saveRDS(nh4_zip_to_add_tmp,file=paste0(dir_save_tmp,'nh4_20',i,'_MA.rds'))
    
    rm(nh4_zip_tmp,zip_missing_tmp,nh4_grid_urban,nh4_grid,nh4_grid_nonurban,link_nh4,nh4_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 8. ni ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'ni_20',i,'_MA.rds'))){
    ### aggregated file
    ni_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    ni_zip_tmp <- ni_zip_tmp[,c('ZIP','ni')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, ni_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    ni_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.ni.rds'))
    ni_grid_urban <- ni_grid_urban[,c("lon","lat","final.predicted.ni")]
    ni_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.ni.rds'))
    ni_grid_nonurban <- ni_grid_nonurban[ni_grid_nonurban$year==paste0('20',i),]
    ni_grid_nonurban <- ni_grid_nonurban[,c("lon","lat","final.predicted.ni")]
    ni_grid <- rbind(ni_grid_urban,ni_grid_nonurban)
    
    ### aggregate for po box
    link_ni <- nabor::knn(ni_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_ni <- cbind.data.frame(link_ni$nn.idx,link_ni$nn.dists)
    names(link_ni) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_ni)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    ni_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,ni=ni_grid[zip_missing_tmp$id,'final.predicted.ni'])
    saveRDS(ni_zip_to_add_tmp,file=paste0(dir_save_tmp,'ni_20',i,'_MA.rds'))
    
    rm(ni_zip_tmp,zip_missing_tmp,ni_grid_urban,ni_grid,ni_grid_nonurban,link_ni,ni_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 9. no3 ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'no3_20',i,'_MA.rds'))){
    ### aggregated file
    no3_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    no3_zip_tmp <- no3_zip_tmp[,c('ZIP','no3')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, no3_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    no3_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.no3.rds'))
    no3_grid_urban <- no3_grid_urban[,c("lon","lat","final.predicted.no3")]
    no3_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.no3.rds'))
    no3_grid_nonurban <- no3_grid_nonurban[no3_grid_nonurban$year==paste0('20',i),]
    no3_grid_nonurban <- no3_grid_nonurban[,c("lon","lat","final.predicted.no3")]
    no3_grid <- rbind(no3_grid_urban,no3_grid_nonurban)
    
    ### aggregate for po box
    link_no3 <- nabor::knn(no3_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_no3 <- cbind.data.frame(link_no3$nn.idx,link_no3$nn.dists)
    names(link_no3) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_no3)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    no3_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,no3=no3_grid[zip_missing_tmp$id,'final.predicted.no3'])
    saveRDS(no3_zip_to_add_tmp,file=paste0(dir_save_tmp,'no3_20',i,'_MA.rds'))
    
    rm(no3_zip_tmp,zip_missing_tmp,no3_grid_urban,no3_grid,no3_grid_nonurban,link_no3,no3_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 10. oc ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'oc_20',i,'_MA.rds'))){
    ### aggregated file
    oc_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    oc_zip_tmp <- oc_zip_tmp[,c('ZIP','oc')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, oc_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    oc_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.oc.rds'))
    oc_grid_urban <- oc_grid_urban[,c("lon","lat","final.predicted.oc")]
    oc_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.oc.rds'))
    oc_grid_nonurban <- oc_grid_nonurban[oc_grid_nonurban$year==paste0('20',i),]
    oc_grid_nonurban <- oc_grid_nonurban[,c("lon","lat","final.predicted.oc")]
    oc_grid <- rbind(oc_grid_urban,oc_grid_nonurban)
    
    ### aggregate for po box
    link_oc <- nabor::knn(oc_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_oc <- cbind.data.frame(link_oc$nn.idx,link_oc$nn.dists)
    names(link_oc) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_oc)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    oc_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,oc=oc_grid[zip_missing_tmp$id,'final.predicted.oc'])
    saveRDS(oc_zip_to_add_tmp,file=paste0(dir_save_tmp,'oc_20',i,'_MA.rds'))
    
    rm(oc_zip_tmp,zip_missing_tmp,oc_grid_urban,oc_grid,oc_grid_nonurban,link_oc,oc_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 11. pb ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'pb_20',i,'_MA.rds'))){
    ### aggregated file
    pb_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    pb_zip_tmp <- pb_zip_tmp[,c('ZIP','pb')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, pb_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    pb_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.pb.rds'))
    pb_grid_urban <- pb_grid_urban[,c("lon","lat","final.predicted.pb")]
    pb_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.pb.rds'))
    pb_grid_nonurban <- pb_grid_nonurban[pb_grid_nonurban$year==paste0('20',i),]
    pb_grid_nonurban <- pb_grid_nonurban[,c("lon","lat","final.predicted.pb")]
    pb_grid <- rbind(pb_grid_urban,pb_grid_nonurban)
    
    ### aggregate for po box
    link_pb <- nabor::knn(pb_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_pb <- cbind.data.frame(link_pb$nn.idx,link_pb$nn.dists)
    names(link_pb) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_pb)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    pb_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,pb=pb_grid[zip_missing_tmp$id,'final.predicted.pb'])
    saveRDS(pb_zip_to_add_tmp,file=paste0(dir_save_tmp,'pb_20',i,'_MA.rds'))
    
    rm(pb_zip_tmp,zip_missing_tmp,pb_grid_urban,pb_grid,pb_grid_nonurban,link_pb,pb_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 12. si ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'si_20',i,'_MA.rds'))){
    ### aggregated file
    si_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    si_zip_tmp <- si_zip_tmp[,c('ZIP','si')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, si_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    si_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.si.rds'))
    si_grid_urban <- si_grid_urban[,c("lon","lat","final.predicted.si")]
    si_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.si.rds'))
    si_grid_nonurban <- si_grid_nonurban[si_grid_nonurban$year==paste0('20',i),]
    si_grid_nonurban <- si_grid_nonurban[,c("lon","lat","final.predicted.si")]
    si_grid <- rbind(si_grid_urban,si_grid_nonurban)
    
    ### aggregate for po box
    link_si <- nabor::knn(si_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_si <- cbind.data.frame(link_si$nn.idx,link_si$nn.dists)
    names(link_si) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_si)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    si_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,si=si_grid[zip_missing_tmp$id,'final.predicted.si'])
    saveRDS(si_zip_to_add_tmp,file=paste0(dir_save_tmp,'si_20',i,'_MA.rds'))
    
    rm(si_zip_tmp,zip_missing_tmp,si_grid_urban,si_grid,si_grid_nonurban,link_si,si_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 13. so4 ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'so4_20',i,'_MA.rds'))){
    ### aggregated file
    so4_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    so4_zip_tmp <- so4_zip_tmp[,c('ZIP','so4')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, so4_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    so4_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.so4.rds'))
    so4_grid_urban <- so4_grid_urban[,c("lon","lat","final.predicted.so4")]
    so4_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.so4.rds'))
    so4_grid_nonurban <- so4_grid_nonurban[so4_grid_nonurban$year==paste0('20',i),]
    so4_grid_nonurban <- so4_grid_nonurban[,c("lon","lat","final.predicted.so4")]
    so4_grid <- rbind(so4_grid_urban,so4_grid_nonurban)
    
    ### aggregate for po box
    link_so4 <- nabor::knn(so4_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_so4 <- cbind.data.frame(link_so4$nn.idx,link_so4$nn.dists)
    names(link_so4) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_so4)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    so4_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,so4=so4_grid[zip_missing_tmp$id,'final.predicted.so4'])
    saveRDS(so4_zip_to_add_tmp,file=paste0(dir_save_tmp,'so4_20',i,'_MA.rds'))
    
    rm(so4_zip_tmp,zip_missing_tmp,so4_grid_urban,so4_grid,so4_grid_nonurban,link_so4,so4_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 14. v ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'v_20',i,'_MA.rds'))){
    ### aggregated file
    v_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    v_zip_tmp <- v_zip_tmp[,c('ZIP','v')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, v_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    v_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.v.rds'))
    v_grid_urban <- v_grid_urban[,c("lon","lat","final.predicted.v")]
    v_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.v.rds'))
    v_grid_nonurban <- v_grid_nonurban[v_grid_nonurban$year==paste0('20',i),]
    v_grid_nonurban <- v_grid_nonurban[,c("lon","lat","final.predicted.v")]
    v_grid <- rbind(v_grid_urban,v_grid_nonurban)
    
    ### aggregate for po box
    link_v <- nabor::knn(v_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_v <- cbind.data.frame(link_v$nn.idx,link_v$nn.dists)
    names(link_v) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_v)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    v_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,v=v_grid[zip_missing_tmp$id,'final.predicted.v'])
    saveRDS(v_zip_to_add_tmp,file=paste0(dir_save_tmp,'v_20',i,'_MA.rds'))
    
    rm(v_zip_tmp,zip_missing_tmp,v_grid_urban,v_grid,v_grid_nonurban,link_v,v_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}



############################# 15. z ##############################
for (i in years_char) {
  if (!file.exists(paste0(dir_save_tmp,'z_20',i,'_MA.rds'))){
    ### aggregated file
    z_zip_tmp <- readRDS(paste0(dir_zip,'20',i,'.rds'))
    z_zip_tmp <- z_zip_tmp[,c('ZIP','z')]
    
    ### zipcodes not included
    zip_missing_tmp <- zip_missing[zip_missing$year==as.numeric(paste0('20',i)),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$ZIP%in%setdiff(zip_missing_tmp$ZIP, z_zip_tmp$ZIP),]
    
    ### combine urban and nonurban grids
    z_grid_urban <- readRDS(paste0(dir_grid_urban,'20',i,'.final.predictions.urban.MA2.50m.z.rds'))
    z_grid_urban <- z_grid_urban[,c("lon","lat","final.predicted.z")]
    z_grid_nonurban <- readRDS(paste0(dir_grid_nonurban,'final.predictions.nonurban.MA2.z.rds'))
    z_grid_nonurban <- z_grid_nonurban[z_grid_nonurban$year==paste0('20',i),]
    z_grid_nonurban <- z_grid_nonurban[,c("lon","lat","final.predicted.z")]
    z_grid <- rbind(z_grid_urban,z_grid_nonurban)
    
    ### aggregate for po box
    link_z <- nabor::knn(z_grid[,c("lon","lat")],zip_missing_tmp[,c("longitude","latitude")],k=1,radius=2*sqrt(2)/10*0.1)
    link_z <- cbind.data.frame(link_z$nn.idx,link_z$nn.dists)
    names(link_z) <- c("id","dis")
    zip_missing_tmp <- cbind.data.frame(zip_missing_tmp,link_z)
    zip_missing_tmp <- zip_missing_tmp[,c("ZIP","year","id")]
    zip_missing_tmp <- zip_missing_tmp[complete.cases(zip_missing_tmp),]
    zip_missing_tmp <- zip_missing_tmp[zip_missing_tmp$id!=0,]
    
    ### merge and save
    z_zip_to_add_tmp <- data.frame(ZIP=zip_missing_tmp$ZIP,z=z_grid[zip_missing_tmp$id,'final.predicted.z'])
    saveRDS(z_zip_to_add_tmp,file=paste0(dir_save_tmp,'z_20',i,'_MA.rds'))
    
    rm(z_zip_tmp,zip_missing_tmp,z_grid_urban,z_grid,z_grid_nonurban,link_z,z_zip_to_add_tmp)
    gc()
  }
  cat(i,'\n')
}
