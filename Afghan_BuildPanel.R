

#------------------------------------------
# USAID Afghanistan Office of Agriculture
# GIE of On Farm Water Management Project
# Outcome: 30m NDVI from Landsat 7
#------------------------------------------

library(sf)
library(readxl)
library(stringdist)
library(plyr)
library(devtools)
library(maptools)
library(rgdal)
library(forecast)
library(tidyr)
library(SDMTools)


# !CHANGE THIS TO YOUR OWN DIRECTORY!
#set the working directory to where the files are stored 
setwd("/Users/rbtrichler/Box Sync/afghanistan_gie")

# --------------
# Read in Data
# --------------

#-----
## Read in points data

#point id, and associated project id and end date
#from geo-referencing done by students to digitize pdf maps from USAID Afghanistan, uses cells that fill command areas
#cells represented as points because they are so small and easier for GeoQuery extract

#use rgdal line to read in as a shapefile
#afcells <- rgdal::readOGR("/Users/rbtrichler/Box Sync/afghanistan_gie/inputData/canal_point_grid.geojson","OGRGeoJSON")
#read in as a dataframe
afcells<-"inputData/canal_point_grid.geojson"
afcells<-st_read(afcells)
#id field is weird, so assign reu_id that is unique, continuous, and numeric: 1 to 221,985
# the field "unique" also provides unique and numeric id, but is too long 
afcells$reu_id<-as.numeric(afcells$id)

# DO THIS AGAIN IF CELL LOCATION DATA CHANGES
# ## Subset cell level dataset so that we have shapefile of one point with coordinates for each project
# # will use this to join in province that each project takes place in
# 
# afproj <- do.call(rbind, by(afcells, list(afcells$project_id),
#                             FUN=function(x) head(x,1)))
# 
# st_write(afproj, "afproj_geojson",layer="afproj", driver="GeoJSON")
# #then use file above to manually join one point per project with admin1 province information in QGIS

#extract geometry from full datset and convert geometry dataset to dataframe
afcells_geo<-st_geometry(afcells)
st_geometry(afcells)<-NULL
afcells_geo <- st_set_geometry(as.data.frame(afcells[,c(4,6)]),afcells_geo)

#-----
## Merge in district level data
# In QGIS, manually joined afproj (one point per project) with district (and province) shapefile
# This gives district and province for each project location 
# will help to evaluate growing seasons by province, allow for district-level clustering

#read in shapefile with province information created with manual join in QGIS
afproj_district <- "ProcessedSpatialData/afproj_district.shp"
afproj_district <- st_read(afproj_district)

# remove geometry
afproj_district_geo <-st_geometry(afproj_district)
st_geometry(afproj_district)<-NULL
afproj_district_geo<- st_set_geometry(as.data.frame(afproj_district$reu_id),afproj_district_geo)
# remove unnecessary columns, just keeping project level vars and province name
afproj_district<-afproj_district[,c(2:3,10,13)]

# merge with afcells
afcells1<- merge(afcells, afproj_district, by="project_id")
afcells<-afcells1
#rename district/province name columns
names(afcells)[names(afcells) == "NAME_1"] = "prov_name"
names(afcells)[names(afcells) == "NAME_2"] = "district_name"
#do a manual/visual check of project end dates, then drop "actual end"
afcells<-afcells[,-(7)]
#create numeric var for province and district
afcells$prov_id<-as.factor(afcells$prov_name)
afcells$prov_id <- unclass(afcells$prov_id)

afcells$district_id<-as.factor(afcells$district_name)
afcells$district_id<-unclass(afcells$district_id)

# # double check creation of province id
# table(afcells$prov_id)
# table(afcells$prov_name)

#------
#Read in geoquery extract
afdist<-read.csv("inputData/merge_canal_point_grid4.csv")

#Drop
#monthly viirs
afdist <- afdist[,-grep("viirs_ntl_monthly",colnames(afdist))]
#drop repeat project vars
afdist <- afdist[,-grep("project_",colnames(afdist))]
afdist <- afdist[,-grep("end_date",colnames(afdist))]

#Rename ndvi vars
colnames(afdist) <- gsub("landsat_","",colnames(afdist),fixed=TRUE)
colnames(afdist) <- gsub(".mean","",colnames(afdist),fixed=TRUE)
colnames(afdist) <- sub("winter","1",colnames(afdist))
colnames(afdist) <- sub("spring","2",colnames(afdist))
colnames(afdist) <- sub("summer","3",colnames(afdist))
colnames(afdist) <- sub("fall","4",colnames(afdist))

#add yearly quarter (1,2,3,4) to end of variable after year value (e.g. 20121, 20122,)

years<-c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
for (i in years)
{
  colnames(afdist) <- sub(paste0("1.",i),paste0("",i,"1"),colnames(afdist))
  colnames(afdist) <- sub(paste0("2.",i),paste0("",i,"2"),colnames(afdist))
  colnames(afdist) <- sub(paste0("3.",i),paste0("",i,"3"),colnames(afdist))
  colnames(afdist) <- sub(paste0("4.",i),paste0("",i,"4"),colnames(afdist))
}

#rename ntl
colnames(afdist) <- sub("v4composites_calibrated_201709.","dmsp_",colnames(afdist))
colnames(afdist) <- sub("viirs_ntl_yearly.","viirs_",colnames(afdist))

#rename
colnames(afdist) <- sub("srtm_slope_500m.na","slope",colnames(afdist))
colnames(afdist) <- sub("srtm_elevation_500m.na","elevation",colnames(afdist))
colnames(afdist) <- sub("dist_to_water.na","rivdist",colnames(afdist))
colnames(afdist) <- sub("dist_to_groads.na","roaddist",colnames(afdist))
colnames(afdist) <- sub("accessibility_map.na","urbtravtime",colnames(afdist))
colnames(afdist) <- sub("distance_to_canal.na.max","dist_canal",colnames(afdist))
colnames(afdist) <- sub("distance_to_starts.na.max","dist_start",colnames(afdist))

#create data subsets with modis temp, cru temp, cru precip
aftemp_pre<-afdist[,c(75:224,491)]
crutemp_pre<-afdist[,c(357:488,491)]
cruprecip_pre<-afdist[,c(225:356,491)]

#drop out temp and precip so can merge it back in later
afdist1<-afdist[,c(1:74,489:491)]
afdist<-afdist1

#merge afcells and afdist
afwide<-merge(afcells,afdist,by="unique")

#-------
#Create seasonal vars from monthly temp and precip vars


#AFTEMP#

aftemp<-aftemp_pre
colnames(cruprecip) <- gsub("modis_lst_day_monthly.","temp_",colnames(aftemp),fixed=TRUE)
aftemp <- aftemp[,-grep("2017",colnames(aftemp))]
aftemp <- aftemp[,-grep("2018",colnames(aftemp))]

#rename december temp values
# e.g. temp_201212 becomes temp_200700 to make it easier to create quarterly vars from monthly

for (i in 2006:2015)
{
  colnames(aftemp)<-sub(paste0("temp_",i,"12"),(paste0("temp_",i+1,"00")),colnames(aftemp))
  
}

#do 2006 winter quarter separately
max06<-c("temp_200601","temp_200602")
aftemp$maxtemp_20061<-apply(aftemp[max06],1,FUN=max)
aftemp$meantemp_20061<-apply(aftemp[max06],1,FUN=mean)
aftemp$mintemp_20061<-apply(aftemp[max06],1,FUN=min)

#Winter vars, 2007-2016
for (i in 2007:2016)
{
  winter<-c((paste0("temp_",i,"00")),(paste0("temp_",i,"01")),(paste0("temp_",i,"02")))
  aftemp[paste0("maxtemp_",i,"1")]<-apply(aftemp[winter],1,FUN=max)
  aftemp[paste0("meantemp_",i,"1")]<-apply(aftemp[winter],1,FUN=mean)
  aftemp[paste0("mintemp_",i,"1")]<-apply(aftemp[winter],1,FUN=min)
  
}

#loop to create spring vars for max temp
for (i in 2006:2016)
{
  spring<-c((paste0("temp_",i,"03")),(paste0("temp_",i,"04")),(paste0("temp_",i,"05")))
  aftemp[paste0("maxtemp_",i,"2")]<-apply(aftemp[spring],1,FUN=max)
  aftemp[paste0("meantemp_",i,"2")]<-apply(aftemp[spring],1,FUN=mean)
  aftemp[paste0("mintemp_",i,"2")]<-apply(aftemp[spring],1,FUN=min)
}  

#loop to create summer vars
for (i in 2006:2016)
{
  summer<-c((paste0("temp_",i,"06")),(paste0("temp_",i,"07")),(paste0("temp_",i,"08")))
  aftemp[paste0("maxtemp_",i,"3")]<-apply(aftemp[spring],1,FUN=max)
  aftemp[paste0("meantemp_",i,"3")]<-apply(aftemp[spring],1,FUN=mean)
  aftemp[paste0("mintemp_",i,"3")]<-apply(aftemp[spring],1,FUN=min)
  
} 

#loop to create fall vars
for (i in 2006:2016)
{
  fall<-c((paste0("temp_",i,"09")),(paste0("temp_",i,"10")),(paste0("temp_",i,"11")))
  aftemp[paste0("maxtemp_",i,"4")]<-apply(aftemp[fall],1,FUN=max)
  aftemp[paste0("meantemp_",i,"4")]<-apply(aftemp[fall],1,FUN=mean)
  aftemp[paste0("mintemp_",i,"4")]<-apply(aftemp[fall],1,FUN=min)
  
} 

#CHECK TEMP CREATION
aftempsub<-aftemp[c(1:400,170000:171000),]

#check 
table(aftempsub$temp_200602, aftempsub$maxtemp_20061)
table(aftempsub$temp_200705, aftempsub$maxtemp_20072)
table(aftempsub$temp_201607, aftempsub$meantemp_20163)
table(aftempsub$temp_200611, aftempsub$mintemp_20064)

#merge
aftempslim<-aftemp[,133:265]
afwide1<-merge(afwide, aftempslim, by="unique")
afwide<-afwide1

# CRUPRECIP #
cruprecip<-cruprecip_pre
colnames(cruprecip) <- gsub("cru_precipitation_monthly.","cruprecip_",colnames(cruprecip),fixed=TRUE)

#rename december temp values
# e.g. temp_201212 becomes temp_200700 to make it easier to create quarterly vars from monthly

for (i in 2006:2015)
{
  colnames(cruprecip)<-sub(paste0("cruprecip_",i,"12"),(paste0("cruprecip_",i+1,"00")),colnames(cruprecip))
  
}

#do 2006 winter quarter separately
max06<-c("cruprecip_200601","cruprecip_200602")
cruprecip$maxcrup_20061<-apply(cruprecip[max06],1,FUN=max)
cruprecip$meancrup_20061<-apply(cruprecip[max06],1,FUN=mean)
cruprecip$mincrup_20061<-apply(cruprecip[max06],1,FUN=min)

#Winter vars, 2007-2016
for (i in 2007:2016)
{
  winter<-c((paste0("cruprecip_",i,"00")),(paste0("cruprecip_",i,"01")),(paste0("cruprecip_",i,"02")))
  cruprecip[paste0("maxcrup_",i,"1")]<-apply(cruprecip[winter],1,FUN=max)
  cruprecip[paste0("meancrup_",i,"1")]<-apply(cruprecip[winter],1,FUN=mean)
  cruprecip[paste0("mincrup_",i,"1")]<-apply(cruprecip[winter],1,FUN=min)
  
}

#loop to create spring vars for max temp
for (i in 2006:2016)
{
  spring<-c((paste0("cruprecip_",i,"03")),(paste0("cruprecip_",i,"04")),(paste0("cruprecip_",i,"05")))
  cruprecip[paste0("maxcrup_",i,"2")]<-apply(cruprecip[spring],1,FUN=max)
  cruprecip[paste0("meancrup_",i,"2")]<-apply(cruprecip[spring],1,FUN=mean)
  cruprecip[paste0("mincrup_",i,"2")]<-apply(cruprecip[spring],1,FUN=min)
}  

#loop to create summer vars
for (i in 2006:2016)
{
  summer<-c((paste0("cruprecip_",i,"06")),(paste0("cruprecip_",i,"07")),(paste0("cruprecip_",i,"08")))
  cruprecip[paste0("maxcrup_",i,"3")]<-apply(cruprecip[spring],1,FUN=max)
  cruprecip[paste0("meancrup_",i,"3")]<-apply(cruprecip[spring],1,FUN=mean)
  cruprecip[paste0("mincrup_",i,"3")]<-apply(cruprecip[spring],1,FUN=min)
  
} 

#loop to create fall vars
for (i in 2006:2016)
{
  fall<-c((paste0("cruprecip_",i,"09")),(paste0("cruprecip_",i,"10")),(paste0("cruprecip_",i,"11")))
  cruprecip[paste0("maxcrup_",i,"4")]<-apply(cruprecip[fall],1,FUN=max)
  cruprecip[paste0("meancrup_",i,"4")]<-apply(cruprecip[fall],1,FUN=mean)
  cruprecip[paste0("mincrup_",i,"4")]<-apply(cruprecip[fall],1,FUN=min)
  
} 

#Check CRU Precip
cruprecipsub<-cruprecip[c(1150:1250,210000:211000),]

#check values
table(cruprecipsub$cruprecip_200601, cruprecipsub$maxcrup_20061)
table(cruprecipsub$cruprecip_200704, cruprecipsub$mincrup_20072)
table(cruprecipsub$cruprecip_200709, cruprecipsub$meancrup_20074)

# MERGE cruprecip into afwide
#cut down cruprecip to relevant variables
crupslim <- cruprecip[,133:265]

afwide2<-merge(afwide, crupslim, by="unique")
afwide<-afwide2



