
#USAID Afghanistan

library(sf)
library(readxl)
library(stringdist)
library(plyr)
library(devtools)
library(maptools)
library(rgdal)
library(forecast)
library(tidyr)


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


#-------
## Read in distance to canal and distance to canal start point file, merge with afcells
afdist<-read.csv("inputData/merge_canal_point_grid_distance.csv")
#drop out project id info
afdist <- afdist[,c(1:2,5)]
#shorten names of distance vars
colnames(afdist) <- sub("distance_to_canal.na.max","dist_canal",colnames(afdist))
colnames(afdist) <- sub("distance_to_starts.na.max","dist_start",colnames(afdist))


afcells<-merge(afcells,afdist,by="unique")

# ----------------
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
#drop distance_to_canal as it uses OLD process and was replaced with afdist file
#it averaged the distance to canal values of multiple cells, rather than identifying distance from cell that point falls in
#ask Seth if more questions
afmerge<-afmerge[,-grep("distance_",colnames(afmerge))]

#merge with afcells
afwide<-merge(afmerge, afcells, by="unique")

#-----
## Read in covariate data
afcovar<-read.csv("inputData/merge_canal_point_grid_other.csv")
afcovar <- afcovar[,-grep("project_",colnames(afcovar))]
afcovar <- afcovar[,-grep("end_date",colnames(afcovar))]

#create 2012 baseline values for udel precip and temp and drop others
colnames(afcovar) <- sub("udel_precip_v4_01_yearly_mean.2012","precip_2012",colnames(afcovar))
afcovar <- afcovar[,-grep("udel_precip",colnames(afcovar))]
colnames(afcovar) <- sub("udel_air_temp_v4_01_yearly_mean.2012","temp_2012",colnames(afcovar))
afcovar <- afcovar[,-grep("udel_air",colnames(afcovar))]

#rename ntl
colnames(afcovar) <- sub("v4composites_calibrated_201709.","dmsp_",colnames(afcovar))
colnames(afcovar) <- sub("viirs_ntl_yearly.","viirs_",colnames(afcovar))

#merge covar data

afwide1<-merge(afwide, afcovar, by="unique")
afwide<-afwide1

# --------------
#scratch

# GeoQuery Extract #2 
# Read in extract that Miranda performed
# temp data
# trying to change monthly mean, min, max to quarterly

# read in extract file
aftemp<-read.csv("inputData/merge_afg_GIE.csv")

# keep slope, elevation, dist to water and roads, travel time ("modis_lst_..."), temp, id info
aftemp<-aftemp[,c(1:14,288:683,786:789)]

#rename temp variable
colnames(aftemp) <- gsub("modis_lst_day_monthly.","temp_",colnames(aftemp),fixed=TRUE)

#subset for checking
aftempsub<-aftemp[aftemp$project_id=="B001",]
aftemp <- aftempsub

year<-c("2006","2007")
year<-c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")

for (i in year)
{
  spring<-c((paste0("temp_",i,"03.max")),(paste0("temp_",i,"04.max")),(paste0("temp_",i,"05.max")))
  aftemp[paste0("maxtemp_",i,"2")]<-apply(aftemp[spring],1,FUN=max)
  
}  

#do this for each season and min, mean, max

aftempsub<-aftemp[,c(15:16,27:28,41:43,113:115,411:427)]
table(aftempsub$maxtemp_20061)
table(aftempsub$mintemp_20071)
table(aftempsub$temp_200805.max, aftempsub$maxtemp_20082)



txt <- c("arm","foot","lefroo", "bafoobar")
if(any(i <- grep("foo",txt)))


#create quarterly min variable

year<-c("2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")

for (i in year)
{
  aftemp[paste0("")]
}

for(i in 1:8)
{
  x_merged[paste0("date_", i)][is.na(x_merged[paste0("date_", i)]) & 
                                 !is.na(x_merged[paste0("road_", i)])] <- paste0("9999-01-0", i)
}


# end scratch
# --------------------------


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
# ***** OTHERWISE, MERGES IN PRE-TRENDS PREVIOUSLY CREATED because it takes forever *****

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

#write.csv (ndvi_pre_panel,"ndvi_pre_panel.csv")

## Merge pre-trends back into wide-form dataset

afwide2<-merge(afwide, obj_coeff, by="reu_id")
afwide<-afwide2

#write.csv (afwide,"afwide.csv")



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
# table(dec$actual_end_date_iso)
# table(dec$end_yearqtr)

## create peak growing season identifier
table(afwide$prov_name,afwide$prov_id)
afwide$peakqtr<-2
afwide$peakqtr[afwide$prov_id==3|afwide$prov_id==11]<-3
table(afwide$prov_id,afwide$peakqtr)

##drop out vars we don't want to keep
afwide3<-afwide[,-grep("temp",colnames(afwide))]
afwide3<-afwide3[,-grep("precip",colnames(afwide3))]


# ----------
## Build Panel 
# ----------

#Order variables by name/time to allow reshape to work properly
af_reshape<-afwide3[,order(names(afwide3))]

#Identify variables where values will change yearly in panel dataset
ndvi<-grep("ndvi_",names(af_reshape))

all_reshape <- c(ndvi)
af_panel <- reshape(af_reshape, varying=all_reshape, direction="long",idvar="unique",sep="_",timevar="qtr")

write.csv(af_panel,"ProcessedData/af_panel.csv")


# ----------------
## Add Variables to Panel
# ----------------

#create dec 2012 baseline ndvi measure
#create temp and precip pre-trends

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

#subset to one canal
subcanal<-afwide[afwide$project_id=="N001",]
summary(subcanal$distance_to_canal.na)
plot(subcanal$reu_id,subcanal$distance_to_canal.na)
plot(subcanal$ndvi_20131,subcanal$distance_to_canal.na)
table(subcanal$distance_to_canal.na)

#subset afwide to look at distance to canal
distcanal<-afwide[,c(1:3,48:51)]
distcanal<-merge(afcells_geo,distcanal,by.x="afcells.reu_id",by.y="reu_id")

st_write(distcanal, "distcanal_geojson",layer="distcanal", driver="GeoJSON")

#subset to one canal
subcanal<-afwide[afwide$project_id=="N001",]
summary(subcanal$distance_to_canal.na)

