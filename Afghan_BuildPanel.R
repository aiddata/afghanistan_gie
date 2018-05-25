
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
#extract geometry and convert geometry dataset to dataframe
afcells_geo<-st_geometry(afcells)
st_geometry(afcells)<-NULL
afcells_geo <- st_set_geometry(as.data.frame(afcells$reu_id),afcells_geo)

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
# Create NDVI pre-trends
## ----------------------

## Create panel dataset for 2006-2012 only
#subset data to ndvi, id, end date, distance to canal
ndvi_pre <- afwide[,(1:51)]
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

#write.csv (ndvi_pre_panel,"ndvi_pre_panel.csv")

## Create pre-trend using panel dataset

obj <- ndvi_pre_panel %>% split(.$reu_id) %>% lapply (lm, formula=formula(ndvi~qtr))

obj_coefficients <- as.data.frame(t(lapply(obj, function(x) as.numeric(x[1]$coefficients[2]))))
obj_coefficients1<-as.data.frame(t(obj_coefficients))
obj_coefficients1$rownumber <- as.numeric(rownames(obj_coefficients1))
obj_coeff<-obj_coefficients1
names(obj_coeff)[names(obj_coeff)=="V1"]="ndvipre_0612"
names(obj_coeff)[names(obj_coeff)=="rownumber"]="reu_id"
obj_coeff$ndvipre_0612<-as.numeric(obj_coeff$ndvipre_0612)

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
#   

## Merge pre-trends back into wide-form dataset

afwide2<-merge(afdata, obj_coeff, by="reu_id")
afwide<-afwide2

write.csv (afwide,"afwide.csv")

#---------
## Prep Panel Build 
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



