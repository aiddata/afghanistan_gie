

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
#setwd("/Users/rbtrichler/Box Sync/afghanistan_gie")

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
#afcells<-"inputData/canal_point_grid.geojson"
afcells<-st_read("/Users/christianbaehr/Downloads/canal_point_grid.geojson")
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
#afproj_district <- "ProcessedSpatialData/afproj_district.shp"
afproj_district <- st_read("/Users/christianbaehr/Downloads/ProcessedSpatialData/afproj_district.shp")

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
#afdist<-read.csv("inputData/merge_canal_point_grid4.csv")
afdist<-read.csv("/Users/christianbaehr/Downloads/merge_canal_point_grid4.csv")

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
colnames(aftemp) <- gsub("modis_lst_day_monthly.","temp_",colnames(aftemp),fixed=TRUE)
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

###################

stats.function <- function(variable="aftemp", years=2007:2016, season="winter") {
  temp <- get(variable)
  temp.cols <- ncol(temp)
  var.short <- gsub("af","",variable,fixed=T)
  var.short <- paste0(gsub("cru","",var.short,fixed=T),"_")
  if(season=="winter") {
    values <- c("00", "01", "02")
    x <- "1"
  } else if(season=="spring") {
    values <- c("03", "04", "05")
    x <- "2"
  } else if(season=="summer") {
    values <- c("06", "07", "08")
    x <- "3"
  } else {
    values <- c("09", "10", "11")
    x <- "4"
  }
  for(i in years) {
    temp.season <- c((paste0(var.short,i,values[1])),(paste0(var.short,i,values[2])),(paste0(var.short,i,values[3])))
    temp[paste0("max",var.short,i,x)] <- apply(temp[temp.season],1,FUN = max)
    temp[paste0("mean",var.short,i,x)] <- apply(temp[temp.season],1,FUN = mean)
    temp[paste0("min",var.short,i,x)] <- apply(temp[temp.season],1,FUN = min)
  }
  return(temp[,(temp.cols+1):ncol(temp)])
}

###################

#Winter vars, 2007-2016

aftemp <- cbind(aftemp, stats.function(variable = "aftemp", years = 2007:2016, season = "winter"))

# for (i in 2007:2016)
# {
#   winter<-c((paste0("temp_",i,"00")),(paste0("temp_",i,"01")),(paste0("temp_",i,"02")))
#   aftemp[paste0("maxtemp_",i,"1")]<-apply(aftemp[winter],1,FUN=max)
#   aftemp[paste0("meantemp_",i,"1")]<-apply(aftemp[winter],1,FUN=mean)
#   aftemp[paste0("mintemp_",i,"1")]<-apply(aftemp[winter],1,FUN=min)
# 
# }

#loop to create spring vars for max temp

aftemp <- cbind(aftemp, stats.function(variable = "aftemp", years = 2006:2016, season = "spring"))

# for (i in 2006:2016)
# {
#   spring<-c((paste0("temp_",i,"03")),(paste0("temp_",i,"04")),(paste0("temp_",i,"05")))
#   aftemp[paste0("maxtemp_",i,"2")]<-apply(aftemp[spring],1,FUN=max)
#   aftemp[paste0("meantemp_",i,"2")]<-apply(aftemp[spring],1,FUN=mean)
#   aftemp[paste0("mintemp_",i,"2")]<-apply(aftemp[spring],1,FUN=min)
# }

#loop to create summer vars

aftemp <- cbind(aftemp, stats.function(variable = "aftemp", years = 2006:2016, season = "summer"))

# for (i in 2006:2016)
# {
#   summer<-c((paste0("temp_",i,"06")),(paste0("temp_",i,"07")),(paste0("temp_",i,"08")))
#   aftemp[paste0("maxtemp_",i,"3")]<-apply(aftemp[summer],1,FUN=max)
#   aftemp[paste0("meantemp_",i,"3")]<-apply(aftemp[summer],1,FUN=mean)
#   aftemp[paste0("mintemp_",i,"3")]<-apply(aftemp[summer],1,FUN=min)
# 
# }

#loop to create fall vars

aftemp <- cbind(aftemp, stats.function(variable = "aftemp", years = 2006:2016, season = "fall"))

# for (i in 2006:2016)
# {
#   fall<-c((paste0("temp_",i,"09")),(paste0("temp_",i,"10")),(paste0("temp_",i,"11")))
#   aftemp[paste0("maxtemp_",i,"4")]<-apply(aftemp[fall],1,FUN=max)
#   aftemp[paste0("meantemp_",i,"4")]<-apply(aftemp[fall],1,FUN=mean)
#   aftemp[paste0("mintemp_",i,"4")]<-apply(aftemp[fall],1,FUN=min)
# 
# }

# #CHECK TEMP CREATION
aftempsub<-aftemp[c(1:400,170000:171000),]

#check
table(aftempsub$temp_200602, aftempsub$maxtemp_20061)
table(aftempsub$temp_200705, aftempsub$maxtemp_20072)
table(aftempsub$temp_201607, aftempsub$meantemp_20163)
table(aftempsub$temp_200611, aftempsub$mintemp_20064)

###################

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
  cruprecip[paste0("maxcrup_",i,"3")]<-apply(cruprecip[summer],1,FUN=max)
  cruprecip[paste0("meancrup_",i,"3")]<-apply(cruprecip[summer],1,FUN=mean)
  cruprecip[paste0("mincrup_",i,"3")]<-apply(cruprecip[summer],1,FUN=min)
  
} 

#loop to create fall vars
for (i in 2006:2016)
{
  fall<-c((paste0("cruprecip_",i,"09")),(paste0("cruprecip_",i,"10")),(paste0("cruprecip_",i,"11")))
  cruprecip[paste0("maxcrup_",i,"4")]<-apply(cruprecip[fall],1,FUN=max)
  cruprecip[paste0("meancrup_",i,"4")]<-apply(cruprecip[fall],1,FUN=mean)
  cruprecip[paste0("mincrup_",i,"4")]<-apply(cruprecip[fall],1,FUN=min)
  
} 

# #Check CRU Precip
# cruprecipsub<-cruprecip[c(1150:1250,210000:211000),]
# 
# #check values
# table(cruprecipsub$cruprecip_200601, cruprecipsub$maxcrup_20061)
# table(cruprecipsub$cruprecip_200705, cruprecipsub$mincrup_20072)
# table(cruprecipsub$cruprecip_200709, cruprecipsub$meancrup_20074)

# MERGE cruprecip into afwide
#cut down cruprecip to relevant variables
crupslim <- cruprecip[,133:265]

afwide2<-merge(afwide, crupslim, by="unique")
afwide<-afwide2

# CRU TEMP #
crutemp<-crutemp_pre
colnames(crutemp) <- gsub("cru_tmp_monthly.","crutemp_",colnames(crutemp),fixed=TRUE)

#rename december temp values
# e.g. temp_201212 becomes temp_200700 to make it easier to create quarterly vars from monthly

for (i in 2006:2015)
{
  colnames(crutemp)<-sub(paste0("crutemp_",i,"12"),(paste0("crutemp_",i+1,"00")),colnames(crutemp))
  
}

#do 2006 winter quarter separately
max06<-c("crutemp_200601","crutemp_200602")
crutemp$maxcrut_20061<-apply(crutemp[max06],1,FUN=max)
crutemp$meancrut_20061<-apply(crutemp[max06],1,FUN=mean)
crutemp$mincrut_20061<-apply(crutemp[max06],1,FUN=min)

#Winter vars, 2007-2016
for (i in 2007:2016)
{
  winter<-c((paste0("crutemp_",i,"00")),(paste0("crutemp_",i,"01")),(paste0("crutemp_",i,"02")))
  crutemp[paste0("maxcrut_",i,"1")]<-apply(crutemp[winter],1,FUN=max)
  crutemp[paste0("meancrut_",i,"1")]<-apply(crutemp[winter],1,FUN=mean)
  crutemp[paste0("mincrut_",i,"1")]<-apply(crutemp[winter],1,FUN=min)
  
}

#loop to create spring vars for max temp
for (i in 2006:2016)
{
  spring<-c((paste0("crutemp_",i,"03")),(paste0("crutemp_",i,"04")),(paste0("crutemp_",i,"05")))
  crutemp[paste0("maxcrut_",i,"2")]<-apply(crutemp[spring],1,FUN=max)
  crutemp[paste0("meancrut_",i,"2")]<-apply(crutemp[spring],1,FUN=mean)
  crutemp[paste0("mincrut_",i,"2")]<-apply(crutemp[spring],1,FUN=min)
}  

#loop to create summer vars
for (i in 2006:2016)
{
  summer<-c((paste0("crutemp_",i,"06")),(paste0("crutemp_",i,"07")),(paste0("crutemp_",i,"08")))
  crutemp[paste0("maxcrut_",i,"3")]<-apply(crutemp[summer],1,FUN=max)
  crutemp[paste0("meancrut_",i,"3")]<-apply(crutemp[summer],1,FUN=mean)
  crutemp[paste0("mincrut_",i,"3")]<-apply(crutemp[summer],1,FUN=min)
  
} 

#loop to create fall vars
for (i in 2006:2016)
{
  fall<-c((paste0("crutemp_",i,"09")),(paste0("crutemp_",i,"10")),(paste0("crutemp_",i,"11")))
  crutemp[paste0("maxcrut_",i,"4")]<-apply(crutemp[fall],1,FUN=max)
  crutemp[paste0("meancrut_",i,"4")]<-apply(crutemp[fall],1,FUN=mean)
  crutemp[paste0("mincrut_",i,"4")]<-apply(crutemp[fall],1,FUN=min)
  
} 

# #CHECK TEMP CREATION
# crutempsub<-crutemp[c(7550:8550,180000:180100),]
# 
# #check values
# table(crutempsub$crutemp_200602, crutempsub$maxcrut_20061)
# table(crutempsub$crutemp_201607, crutempsub$meancrut_20163)
# table(crutempsub$crutemp_201306, crutempsub$mincrut_20133)

#merge
crutslim <- crutemp[,133:265]

afwide3<-merge(afwide, crutslim, by="unique")
afwide<-afwide3


#---------
## Prep Full Panel Build 
# ---------

##change actual_end_date_iso to separate day, month, year columns, then create quarter from months

afwide$end_year<-as.numeric(format(afwide$actual_end_date_iso, format="%Y"))
afwide$end_month<-as.numeric(format(afwide$actual_end_date_iso, format="%m"))
afwide$end_day<-as.numeric(format(afwide$actual_end_date_iso, format="%d"))

afwide$end_quarter<- 1
afwide$end_quarter[afwide$end_month>2 & afwide$end_month<6]<-2
afwide$end_quarter[afwide$end_month>5 & afwide$end_month<9]<-3
afwide$end_quarter[afwide$end_month>8 & afwide$end_month<12]<-4

#create year + quarter variable
#create year var that is end_year plus 1 if end_month=12
#because 20141 is dec 2013, jan 2014, feb 2014
afwide$end_yearadj <- as.numeric(afwide$end_year)
afwide$yearadj<-0
afwide$yearadj[afwide$end_month==12]<-1
afwide$end_yearadj<-afwide$end_year + afwide$yearadj
afwide$end_yearqtr <- as.numeric(paste(afwide$end_yearadj,afwide$end_quarter,sep=""))
table(afwide$end_year)
table(afwide$end_yearadj)

# # check
# dec<-afwide[afwide$end_month==12,]
# #end_quarter should only have value=1
# table(dec$end_quarter)
# #should all be month 12
# table(dec$actual_end_date_iso)
# #should be quarter 1 and 1 year beyond end date
# table(dec$end_yearqtr)

## create peak growing season identifier
table(afwide$prov_name,afwide$prov_id)
afwide$peakqtr<-2
afwide$peakqtr[afwide$prov_id==3|afwide$prov_id==11]<-3
table(afwide$prov_id,afwide$peakqtr)

#write.csv (afwide,"ProcessedData/afwide.csv")

# ----------
## Build Panel 
# ----------

#Order variables by name/time to allow reshape to work properly
af_reshape<-afwide[,order(names(afwide))]

#Identify variables where values will change yearly in panel dataset
ndvi<-grep("ndvi_",names(af_reshape))
mint<-grep("mintemp_",names(af_reshape))
meant<-grep("meantemp_",names(af_reshape))
maxt<-grep("maxtemp_",names(af_reshape))
mincrup<-grep("mincrup",names(af_reshape))
meancrup<-grep("meancrup",names(af_reshape))
maxcrup<-grep("maxcrup",names(af_reshape))
mincrut<-grep("mincrut",names(af_reshape))
meancrut<-grep("meancrut",names(af_reshape))
maxcrut<-grep("maxcrut",names(af_reshape))

all_reshape <- c(ndvi,mint, meant, maxt, mincrup, meancrup, maxcrup,mincrut,meancrut,maxcrut)
af_panel <- reshape(af_reshape, varying=all_reshape, direction="long",idvar="unique",sep="_",timevar="qtr")

#write.csv(af_panel,"/Users/rbtrichler/Desktop/af_panel.csv")

#--------------------
# Check Panel with Input Data
#--------------------

# summary stats for all three sources should match

# info from input data, afdist dataset
afdist_sub<- afdist[which(afdist$dist_canal<1),]
summary(afdist_sub$ndvi_20091)
# number of obs should be 2338

#info from cross-section dataset, afwide
# number of obs and summary info should match
afwide_sub<-afwide[which(afwide$dist_canal<1),]
summary(afwide_sub$ndvi_20091)

# info from ndvi_pre_panel
af_panel_sub<-af_panel[which(af_panel$dist_canal<1 & af_panel$qtr=="20091"),]
summary(af_panel_sub$ndvi)

# ----------------
## Add Variables to Panel
# ----------------

## Create weight by canal size

#create weight in af_panel
canal_cells <- count(af_panel$reu_id, c('af_panel$project_id','af_panel$qtr'))
canal_cells<-canal_cells[canal_cells$af_panel.qtr=="20061",]
canal_cells<-canal_cells[,c(1,3)]
af_panel1<-merge(af_panel, canal_cells, by.x="project_id",by.y="af_panel.project_id")
af_panel1$canal_weight<-1/af_panel1$freq

## Divide ndvi values by 10,000
af_panel1$ndvi<-af_panel1$ndvi/10000

## Convert Kelvin Temp to Celcius
af_panel1$meantemp<-af_panel1$meantemp-273.15
af_panel1$mintemp<-af_panel1$mintemp-273.15
af_panel1$maxtemp<-af_panel1$maxtemp-273.15

af_panel<-af_panel1

## CREATE TREATMENT VAR

af_panel$trt<-NA
af_panel$trt[which(af_panel$qtr<af_panel$end_yearqtr)]<-0
af_panel$trt[which(af_panel$qtr>=af_panel$end_yearqtr)]<-1
summary(af_panel$trt)

# manual check with smaller dataset
dec_panel<-af_panel[af_panel$end_year==2016,]

## CREATE PEAK GROWING SEASON VAR
#=1 when peak growing season quarter, =0 otherwise
#create variable that indicates quarter of observation only (without year)
af_panel<-separate(af_panel,qtr,into=c("yearonly","qtronly"),sep=4,remove=FALSE,convert=TRUE)
table(af_panel$yearonly)
table(af_panel$qtronly)

# check
af_panel$yqtrcheck <- as.numeric(paste(af_panel$yearonly,af_panel$qtronly,sep=""))
af_panel$qtrcheck <- af_panel$qtr-af_panel$yqtrcheck
#should be all 0's
table(af_panel$qtrcheck)

#create peak growing season identifier var
af_panel$peakqtr_id<-NA
af_panel$peakqtr_id[which(af_panel$qtronly==af_panel$peakqtr)]<-1
af_panel$peakqtr_id[which(af_panel$qtronly!=af_panel$peakqtr)]<-0
#check
table(af_panel$peakqtr_id)
table(af_panel$peakqtr,af_panel$peakqtr_id)
#manual check
dec_panel<-af_panel[af_panel$end_year==2016,]


#write.dta(af_panel, "ProcessedData/af_panel.dta")

#write.csv(af_panel,"ProcessedData/af_panel.csv")




## -------
#STATS TABLE, weighted
#---------
# for draft report, used code to output summary stats table
# but then had to manually add (to html or in Excel where formatted) the weighted mean and sd 
# for below, only the var names and min/max values are correct if want to weight by community size
# will need to get weighted baseline ndvi values from STATA
stargazer(af_panel, type="html",
          keep=c("ndvi","meantemp","maxtemp","mintemp","meancrup","maxcrup","mincrup","dist_canal",
                 "dist_start"),
          # covariate.labels=c("NDVI","Slope (degree)","Distance to Road (m)","Distance to River (m)","Elevation (m)",
          #                    "Area (hectares)","Population Density","Mean Temperature","Mean Precipitation",
          #                    "Min Temperature","Min Precipitation","Max Temperature","Max Precipitation",
          #                    "NDVI Pre Trend","Predicted NDVI Pre Trend"),
          omit.summary.stat=c("n"))

## Manually produce weighted mean and sd 

#manually change the var as needed
wt.mean(af_panel$maxcrup,af_panel$canal_weight)
wt.sd(af_panel$maxcrup,af_panel$canal_weight)

af_panel2012<-af_panel[af_panel$yearonly==2012,]
wt.mean(af_panel2012$ndvi,af_panel2012$canal_weight)
wt.sd(af_panel2012$ndvi,af_panel2012$canal_weight)


