
#USAID Afghanistan

library(sf)
library(readxl)
library(stringdist)
library(plyr)
library(devtools)
library(maptools)
library(rgdal)


#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
setwd("/Users/rbtrichler/Box Sync/afghanistan_gie")

# --------------
# Read in Data
# --------------

## Read in points, outcome, and covariate data

#point id, and associated project id and end date

#use rgdal line to read in as a shapefile
#afcells <- rgdal::readOGR("/Users/rbtrichler/Box Sync/afghanistan_gie/inputData/canal_point_grid.geojson","OGRGeoJSON")
#read in as a dataframe
afcells<-"inputData/canal_point_grid.geojson"
afcells<-st_read(afcells)
afcells$reu_id<-as.numeric(afcells$id)
#extract geometry and convert geometry dataset to dataframe
afcells_geo<-st_geometry(afcells)
st_geometry(afcells)<-NULL
afcells_geo <- st_set_geometry(as.data.frame(afcells$reu_id),afcells_geo)

## Read in and rename ndvi outcome data file

#ndvi data, join to points by field "unique"
afmerge<-read.csv("inputData/merge_canal_point_grid.csv")

#Rename ndvi vars
colnames(afmerge) <- gsub("landsat_","",colnames(afmerge),fixed=TRUE)
colnames(afmerge) <- gsub(".max","",colnames(afmerge),fixed=TRUE)
colnames(afmerge) <- sub("winter","1",colnames(afmerge))
colnames(afmerge) <- sub("spring","2",colnames(afmerge))
colnames(afmerge) <- sub("summer","3",colnames(afmerge))
colnames(afmerge) <- sub("fall","4",colnames(afmerge))

#add yearly quarter (1,2,3,4) to end of variable after year value

years<-c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
for (i in years)
{
  colnames(afmerge) <- sub(paste0("1.",i),paste0("",i,"1"),colnames(afmerge))
  colnames(afmerge) <- sub(paste0("2.",i),paste0("",i,"2"),colnames(afmerge))
  colnames(afmerge) <- sub(paste0("3.",i),paste0("",i,"3"),colnames(afmerge))
  colnames(afmerge) <- sub(paste0("4.",i),paste0("",i,"4"),colnames(afmerge))
}
#drop repeat vars
afmerge <- afmerge[,-grep("project_",colnames(afmerge))]
afmerge <- afmerge[,-grep("end_date",colnames(afmerge))]

## Read in covariate data
afcovar<-read.csv("inputData/merge_canal_point_grid_other.csv")
afcovar <- afcovar[,-grep("project_",colnames(afcovar))]
afcovar <- afcovar[,-grep("end_date",colnames(afcovar))]

#join files
afmerge1<-merge(afmerge, afcells, by="unique")
afmerge2<-merge(afmerge1, afcovar, by="unique")

afmerge <- afmerge2

## Test creation of pre-trend
#subset data to ndvi, id, end date, distance to canal
ndvi_pre <- afmerge[,(1:51)]
#create panel dataset
#order by name/date to allow reshape to work properly
ndvi_pre_reshape <- ndvi_pre[,order(names(ndvi_pre))]
#cut out years 2013-2016 for ndvi
ndvi_pre_reshape<-ndvi_pre_reshape[,-(32:47)]
# cut out most rows to test (delete when done testing)
ndvi_pre_reshape<-ndvi_pre_reshape[which(ndvi_pre_reshape$reu_id<11),]
#Identify variables where values will change yearly in panel dataset
ndvi_pre<-grep("ndvi_",names(ndvi_pre_reshape))
pre_reshape_vars<-c(ndvi_pre)
ndvi_pre_panel<-reshape(ndvi_pre_reshape, varying=pre_reshape_vars,direction="long",idvar="unique",sep="_",timevar="qtr")

#test creation of pre-trend
# use id instead of reu_id because it already exists as a factor
ndvi_pre_model <- lm (ndvi ~ id*qtr, data=ndvi_pre_panel)

  
#---------
## Prep Panel Build 
# ---------



# ----------
## Build Panel 
# ----------

#Order variables by name/time to allow reshape to work properly
af_reshape<-afmerge[,order(names(afmerge))]

#Identify variables where values will change yearly in panel dataset
ndvi<-grep("ndvi_",names(af_reshape))

all_reshape <- c(ndvi)
af_panel <- reshape(af_reshape, varying=all_reshape, direction="long",idvar="unique",sep="_",timevar="qtr")
