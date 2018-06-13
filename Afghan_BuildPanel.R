
#USAID Afghanistan

library(sf)
library(readxl)
library(stringdist)
library(plyr)
library(devtools)
library(maptools)
library(rgdal)
library(forecast)


#set the working directory to where the files are stored - !CHANGE THIS TO YOUR OWN DIRECTORY!
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
afcells_geo <- st_set_geometry(as.data.frame(afcells$reu_id),afcells_geo)



#-----
## Merge in province level data
# In QGIS, manually joined afproj (one point per project) with province shapefil
# This gives province for each project location which will help to evaluate growing seasons by province, etc

#read in shapefile with province information created with manual join in QGIS
afproj_province <- "ProcessedSpatialData/afproj_province.shp"
afproj_province <- st_read(afproj_province)

# remove geometry
afproj_province_geo <-st_geometry(afproj_province)
st_geometry(afproj_province)<-NULL
afproj_province_geo<- st_set_geometry(as.data.frame(afproj_province$reu_id),afproj_province_geo)
# remove unnecessary columns, just keeping project level vars and province name
afproj_province<-afproj_province[,c(2:3,10)]

# merge with afcells
afcells1<- merge(afcells, afproj_province, by="project_id")
afcells<-afcells1
#rename province name columns
names(afcells)[names(afcells) == "NAME_1"] = "prov_name"
#manual check of project end dates, then drop "actual end"
afcells<-afcells[,-(7)]
#create numeric var for province name
afcells$prov_id<-as.factor(afcells$prov_name)
afcells$prov_id <- unclass(afcells$prov_id)
# # double check creation of province id
# table(afcells$prov_id)
# table(afcells$prov_name)

#-------
## Read in and rename ndvi outcome data file, merge with cells

#ndvi data extract from GeoQuery, join to points by field "unique"
afmerge<-read.csv("inputData/merge_canal_point_grid.csv")

#Rename ndvi vars
colnames(afmerge) <- gsub("landsat_","",colnames(afmerge),fixed=TRUE)
colnames(afmerge) <- gsub(".max","",colnames(afmerge),fixed=TRUE)
colnames(afmerge) <- sub("winter","1",colnames(afmerge))
colnames(afmerge) <- sub("spring","2",colnames(afmerge))
colnames(afmerge) <- sub("summer","3",colnames(afmerge))
colnames(afmerge) <- sub("fall","4",colnames(afmerge))

#add yearly quarter (1,2,3,4) to end of variable after year value (e.g. 20121, 20122,)

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

#merge with afcells
afwide<-merge(afmerge, afcells, by="unique")

#-----
## Read in covariate data
afcovar<-read.csv("inputData/merge_canal_point_grid_other.csv")
afcovar <- afcovar[,-grep("project_",colnames(afcovar))]
afcovar <- afcovar[,-grep("end_date",colnames(afcovar))]

#merge covar data

afwide1<-merge(afwide, afcovar, by="unique")
afwide<-afwide1

##-----------------------
# Create NDVI pre-panel
## ----------------------

## Create panel dataset for 2006-2012 only
#subset data to ndvi, id, end date, distance to canal
ndvi_pre <- afwide[,(1:53)]
#write.csv(ndvi_pre,"ndvi_pre.csv")
#create panel dataset
#order by name/date to allow reshape to work properly
ndvi_pre_reshape <- ndvi_pre[,order(names(ndvi_pre))]
#cut out years 2013-2016 for ndvi
ndvi_pre_reshape<-ndvi_pre_reshape[,-(32:47)]

# create subset for testing purposes
#ndvi_pre_reshape<-ndvi_pre_reshape[which(ndvi_pre_reshape$reu_id<5000),]

#Identify variables where values will change yearly in panel dataset
ndvi_pre<-grep("ndvi_",names(ndvi_pre_reshape))
pre_reshape_vars<-c(ndvi_pre)
ndvi_pre_panel<-reshape(ndvi_pre_reshape, varying=pre_reshape_vars,direction="long",idvar="reu_id",sep="_",timevar="qtr")


# -------------
## Check that variables match between input, afwide, and panel dataset
# summary stats for all three sources should match

# info from input data, afmerge dataset
afmerge_sub<- afmerge[which(afmerge$distance_to_canal.na<1),]
summary(afmerge_sub$ndvi_20091)
# number of obs should be 2338

#info from cross-section dataset, afwide
# number of obs and summary info should match
afwide_sub<-afwide[which(afwide$distance_to_canal.na<1),]
summary(afwide_sub$ndvi_20091)

# info from ndvi_pre_panel
ndvipre_sub<-ndvi_pre_panel[which(ndvi_pre_panel$distance_to_canal.na<1 & ndvi_pre_panel$qtr=="20091"),]
summary(ndvipre_sub$ndvi)


#-------------------
# Add NDVI pre-trends
# ------------------

# ***** REDO THIS SECTION IF CELLS OR NDVI DATA CHANGES ********
# ***** OTHERWISE, MERGES IN PRE-TRENDS PREVIOUSLY CREATED *****

# ## Create pre-trend using panel dataset
# 
# obj <- ndvi_pre_panel %>% split(.$reu_id) %>% lapply (lm, formula=formula(ndvi~qtr))
# 
# obj_coefficients <- as.data.frame(t(lapply(obj, function(x) as.numeric(x[1]$coefficients[2]))))
# obj_coefficients1<-as.data.frame(t(obj_coefficients))
# obj_coefficients1$rownumber <- as.numeric(rownames(obj_coefficients1))
# obj_coeff<-obj_coefficients1
# names(obj_coeff)[names(obj_coeff)=="V1"]="ndvipre_0612"
# names(obj_coeff)[names(obj_coeff)=="rownumber"]="reu_id"
# obj_coeff$ndvipre_0612<-as.numeric(obj_coeff$ndvipre_0612)
# 
# #write.csv(obj_coeff,"ndvipretrends_0612.csv")

# ##Double check pre-trend creation -- all three below should be the same
# 
# #this gives you value from obj_coeff for any reu_id
# S=obj_coeff$rownumber==19
# obj_coeff[S,]$ndvipretrend
# #this gives you the value from obj 
# obj$`19`$coefficients
# #this runs the same regression from the original ndvi_pre_panel dataset
# ndvi_pre_panel_19<-ndvi_pre_panel[ndvi_pre_panel$reu_id==19,]
# lm(ndvi~qtr, data=ndvi_pre_panel_19)
 

## Merge pre-trends into ndvi_pre_panel
# Read in file with pretrends created previously
obj_coeff <- read.csv("ndvipretrends_0612.csv")
obj_coeff<-obj_coeff[,2:3]
#Merge pre-trends into ndvi_pre_panel
ndvi_pre_panel1<-merge(ndvi_pre_panel, obj_coeff, by="reu_id")
ndvi_pre_panel<-ndvi_pre_panel1

write.csv (ndvi_pre_panel,"ndvi_pre_panel.csv")

## Merge pre-trends back into wide-form dataset

afwide2<-merge(afwide, obj_coeff, by="reu_id")
afwide<-afwide2

write.csv (afwide,"afwide.csv")



#---------
## Prep Full Panel Build 
# ---------



# ----------
## Build Panel 
# ----------

#Order variables by name/time to allow reshape to work properly
af_reshape<-afdata[,order(names(afwide))]

#Identify variables where values will change yearly in panel dataset
ndvi<-grep("ndvi_",names(af_reshape))

all_reshape <- c(ndvi)
af_panel <- reshape(af_reshape, varying=all_reshape, direction="long",idvar="unique",sep="_",timevar="qtr")



#----------------
#Workspace
#----------------


#reduce ndvi_pre_panel
prepaneltest <- ndvi_pre_panel[,c("reu_id", "qtr", "ndvi")]

ts<-as.ts(prepaneltest)
plot.ts(prepaneltest)
decompose(prepaneltest)

ts$seasonal<-ts$qtr
ts.stl <- stl(ts,s.window="periodic")  # decompose the TS
ts.sa <- seasadj(ts.stl)  # de-seasonalize
plot(prepaneltest, type="l")  # original series
plot(ts.sa, type="l")  # seasonal adjusted
seasonplot(ts.sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Airpassengers") # seasonal frequency set as 12 for monthly data.

#summary statistics by group to examine crop growth seasonality

province<- group_by(afwide, prov_name)
summarize(province, count=n(), ndvi20091=mean(ndvi_20091, na.rm=T), max=max(ndvi_20091, na.rm=T))

prepanel_bamyan <- ndvi_pre_panel[ndvi_pre_panel$prov_id==3,]
bamyan<-group_by(prepanel_bamyan, qtr)
bamyanndvi <- summarize(bamyan, mean=mean(ndvi, na.rm=T), sd=sd(ndvi, na.rm=T),median=median(ndvi, na.rm=T),
                        max=max(ndvi, na.rm=T), IQR=IQR(ndvi, na.rm=T))

