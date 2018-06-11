#install.packages("readxl")
library(readxl)
library(readr)

setwd("/Users/careyglenn/Box Sync/afghanistan_gie")
ndvi_pre_panel<-read.csv("ndvi_pre_panel.csv")

##SCRATCH WORK -- creating samples and graphs with those smaller samples
#ndvi_pre<-read.csv("ndvi_pre.csv")
#names(ndvi_pre)

#sample1 <- ndvi_pre_panel[sample(1:nrow(ndvi_pre_panel), 50, replace=FALSE),]
#sample2 <- ndvi_pre_reshape[sample(1:nrow(ndvi_pre_reshape), 50, replace=FALSE),]

#attach(sample1)
#plot(qtr, ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

##NEW WORK STARTS HERE
#creating new variable -- quarter treated called "quarter", download lubridate package
#indicates the year and quarter of treatment (from actual_end_date_iso)
require(lubridate)
ndvi_pre_panel$quarter<-quarter(ndvi_pre_panel$actual_end_date_iso,with_year=TRUE, fiscal_start=12)
#GRAPH 1
#creating a scatterplot
scatterplot = plot(ndvi_pre_panel$quarter, ndvi_pre_panel$ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

#creating boxplot
library(ggplot2)
#create variable that identifies just quarter of treatment (no year)
ndvi_pre_panel$quarteronly<-quarter(ndvi_pre_panel$actual_end_date_iso,with_year=FALSE, fiscal_start=12)

ggplot(ndvi_pre_panel,aes(x=as.factor(quarteronly),y=ndvi))+
  geom_boxplot(fill="slateblue",alpha=.2)
#GRAPH 2
plot(ndvi_pre_panel$quarteronly, ndvi_pre_panel$ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

##
#Creating new variable based on qtr 
#we want only the final number of qtr, which identifies quarter in which observation was measured
#cut the first three digits, and then cut one more -- cutting 4 digits didn't work
ndvi_pre_panel$qtrndvi <- ndvi_pre_panel$qtr - round(ndvi_pre_panel$qtr, -3)
ndvi_pre_panel$qtrndvi <- ndvi_pre_panel$qtrndvi - round(ndvi_pre_panel$qtrndvi, -1)

# Visualize seasonality of NDVI - scatterplot of all pre-project ndvi measures by quarter
# Each quarter includes all years of that quarter 

#creating a scatterplot for the new qtrndvi variable
# uses ndvi for all years pre-project, 2006-2012
#GRAPH 3
scatterplot = plot(ndvi_pre_panel$qtrndvi, ndvi_pre_panel$ndvi, main="NDVI by Quarter", xlab="Quarter", ylab="NDVI at Baseline", pch=19)

##
#GRAPH 4
#creating new plot
ggplot(ndvi_pre_panel,aes(x=as.factor(qtr),y=ndvi))+
  geom_boxplot(fill="slateblue",alpha=.2)

##
#GRAPH 5
#scatterplot with new variable ndvipre_0612
plot(ndvi_pre_panel$quarteronly, ndvi_pre_panel$ndvipre_0612, main="NDVI Pretrends", xlab="Quarter Cell Treated", ylab="NDVI Pretrends", pch=19)

##Work in progress
#provinces line graph
plot(ndvi_pre_panel$qtr, ndvi_pre_panel$ndvi,ndvi_pre_panel$qtrtrt,type = "l",col = "red", xlab = "qtr", ylab = "ndvi", 
     main = "NDVI by Province")

lines(t, type = "o", col = "blue")
lines(t, type = "o", col = "green")
lines(t, type = "o", col = "orange")
lines(t, type = "o", col = "yellow")
lines(t, type = "o", col = "purple")
lines(t, type = "o", col = "black")
