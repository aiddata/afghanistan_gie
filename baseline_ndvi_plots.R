#install.packages("readxl")
library(readxl)
library(readr)

setwd("/Users/careyglenn/Box Sync/afghanistan_gie")
#ndvi_pre_panel<-read.csv("ndvi_pre_panel.csv")
ndvi_pre<-read.csv("ndvi_pre.csv")
#ndvi_pre_panel<-read.csv("ndvi_pre_panel.csv")
names(ndvi_pre)


# #create test panel dataset
# #order by name/date to allow reshape to work properly
# ndvi_pre_reshape <- ndvi_pre[,order(names(ndvi_pre))]
# #cut out years 2013-2016 for ndvi
# ndvi_pre_reshape<-ndvi_pre_reshape[,-(32:47)]
# # cut out most rows to test (delete when done testing)
# ndvi_pre_reshape<-ndvi_pre_reshape[which(ndvi_pre_reshape$reu_id<11),]
# #Identify variables where values will change yearly in panel dataset
# ndvi_pre<-grep("ndvi_",names(ndvi_pre_reshape))
# pre_reshape_vars<-c(ndvi_pre)
# ndvi_pre_panel<-reshape(ndvi_pre_reshape, varying=pre_reshape_vars,direction="long",idvar="unique",sep="_",timevar="qtr")
# 
# write.csv (ndvi_pre_panel,"ndvi_pre_panel_subset.csv")

#read in panel dataset

ndvi_pre_panel<-read.csv("ndvi_pre_panel_subset.csv")


#create full panel dataset
#order by name/date to allow reshape to work properly
ndvi_pre_reshape <- ndvi_pre[,order(names(ndvi_pre))]
#cut out years 2013-2016 for ndvi
ndvi_pre_reshape<-ndvi_pre_reshape[,-(32:47)]
#Identify variables where values will change yearly in panel dataset
ndvi_pre<-grep("ndvi_",names(ndvi_pre_reshape))
pre_reshape_vars<-c(ndvi_pre)
ndvi_pre_panel<-reshape(ndvi_pre_reshape, varying=pre_reshape_vars,direction="long",idvar="unique",sep="_",timevar="qtr")

sample1 <- ndvi_pre_panel[sample(1:nrow(ndvi_pre_panel), 50, replace=FALSE),]
sample2 <- ndvi_pre_reshape[sample(1:nrow(ndvi_pre_reshape), 50, replace=FALSE),]

attach(sample1)
plot(qtr, ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

##NEW WORK STARTS HERE
#calling in data frame in order to create new variable
df=read.csv("ndvi_pre_panel.csv",na.strings=c("", "NA"), header = TRUE)
#looking at the names of the variables
names(df)
#creating new variable -- quarter treated called "quarter", download lubridate package
require(lubridate)
ndvi_pre_panel$quarter<-quarter(ndvi_pre_panel$actual_end_date_iso,with_year=TRUE, fiscal_start=12)
#creating a scatterplotplot
plot(ndvi_pre_panel$quarter, ndvi_pre_panel$ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)

#calling in data frame in order to create new variable
df=read.csv("ndvi_pre_panel.csv",na.strings=c("", "NA"), header = TRUE)
names(df)
#creating boxplot

library(ggplot2)
ndvi_pre_panel$quarteronly<-quarter(ndvi_pre_panel$actual_end_date_iso,with_year=FALSE, fiscal_start=12)

ggplot(ndvi_pre_panel,aes(x=as.factor(quarteronly),y=ndvi))+
  geom_boxplot(fill="slateblue",alpha=.2)

plot(ndvi_pre_panel$quarteronly, ndvi_pre_panel$ndvi, main="NDVI at Baseline", xlab="Quarter Cell Treated", ylab="NDVI at Baseline", pch=19)



