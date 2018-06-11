#install.packages("readxl")
library(readxl)
library(readr)

setwd("/Users/careyglenn/Box Sync/afghanistan_gie")
ndvi_pre_panel<-read.csv("ndvi_pre_panel.csv")
#ndvi_pre<-read.csv("ndvi_pre.csv")
#names(ndvi_pre)

#sample1 <- ndvi_pre_panel[sample(1:nrow(ndvi_pre_panel), 50, replace=FALSE),]
#sample2 <- ndvi_pre_reshape[sample(1:nrow(ndvi_pre_reshape), 50, replace=FALSE),]

#attach(sample1)
#plot(qtr, ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

##NEW WORK STARTS HERE
#calling in data frame in order to create new variable
df=read.csv("ndvi_pre_panel.csv",na.strings=c("", "NA"), header = TRUE)
#looking at the names of the variables
names(df)
#creating new variable -- quarter treated called "quarter", download lubridate package
require(lubridate)
ndvi_pre_panel$quarter<-quarter(ndvi_pre_panel$actual_end_date_iso,with_year=TRUE, fiscal_start=12)
#creating a scatterplotplot
scatterplot = plot(ndvi_pre_panel$quarter, ndvi_pre_panel$ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)



#calling in data frame in order to create new variable
df=read.csv("ndvi_pre_panel.csv",na.strings=c("", "NA"), header = TRUE)
names(df)
#creating boxplot

library(ggplot2)
ndvi_pre_panel$quarteronly<-quarter(ndvi_pre_panel$actual_end_date_iso,with_year=FALSE, fiscal_start=12)

ggplot(ndvi_pre_panel,aes(x=as.factor(quarteronly),y=ndvi))+
  geom_boxplot(fill="slateblue",alpha=.2)

plot(ndvi_pre_panel$quarteronly, ndvi_pre_panel$ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

#####
##Creating new variable based on qtr -- we want only the final number in that list
##cut the first three digits, and then cut one more -- cutting 4 digits didn't work
ndvi_pre_panel$qtrndvi <- ndvi_pre_panel$qtr - round(ndvi_pre_panel$qtr, -3)
ndvi_pre_panel$qtrndvi <- ndvi_pre_panel$qtrndvi - round(ndvi_pre_panel$qtrndvi, -1)

#creating a scatterplot for the new qtrndvi variable
scatterplot = plot(ndvi_pre_panel$qtrndvi, ndvi_pre_panel$ndvi, main="NDVI by Quarter", xlab="Quarter", ylab="NDVI at Baseline", pch=19)



