

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

