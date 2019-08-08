
library(foreign)

# setwd("/Users/christianbaehr/Box Sync/afghanistan_gie")
setwd("C:/Users/cbaehr/Box Sync/afghanistan_gie")

opioid <- read.csv("inputData/unodc_opiumCultivation.csv", stringsAsFactors = F)
names(opioid) <- c("province", "district", paste0("x", 2008:2018))

opioid <- as.data.frame(apply(opioid, 2, function(x) {ifelse(x=="-", "0", x)}), stringsAsFactors = F)

for(i in 2008:2018) {
  col <- paste0("x", i)
  opioid[is.na(sapply(opioid[,col], as.numeric)), col] <- "NA"
  
}

opioid$district[opioid$district=="Aybak (Provincial Center)"] <- "Aybak"
opioid$district[opioid$district=="Bamyan (Provincial Center)"] <- "Bamyan City"
opioid$district[opioid$district=="Chahar Bolak"] <- "Char Bolak"
opioid$district[opioid$district=="Darah-i- Noor"] <- "Dara-I-Nur"
opioid$district[opioid$district=="DehSabz"] <- "Dih Sabz"
opioid$district[opioid$district=="Dehdadi"] <- "Dihdadi"
opioid$district[opioid$district=="Dushi"] <- "Doshi"
opioid$district[opioid$district=="Hazrat-i-Sultan"] <- "Hazrati Sultan"
opioid$district[opioid$district=="Enjil"] <- "Injil"
opioid$district[opioid$district=="Estalef"] <- "Istalif"
opioid$district[opioid$district=="Khwajah Sabz Poshi Wali"] <- "Khwaja Sabz Posh"
opioid$district[opioid$district=="Kuzkunar"] <- "Kuz Kunar"
opioid$district[opioid$district=="Narang wa Badil"] <- "Narang Wa Badil"
opioid$district[opioid$district=="Noor Gal"] <- "Nurgal"
opioid$district[opioid$district=="Pul-i-Khumri (Provincial Center"] <- "Puli Khumri"
opioid$district[opioid$district=="Qarghayee"] <- "Qarghayi"
opioid$district[opioid$district=="Surkh Rud"] <- "Surkh Rod"
opioid$district[opioid$district=="Zendah Jan"] <- "Zinda Jan"
opioid$district[opioid$district=="Kahmard *"] <- "Kahmard"

opioid$matchid <- paste(opioid$province, ",", opioid$district)

opioid <- opioid[opioid$district!="" & !duplicated(opioid),]

###

panel <- read.dta("ProcessedData/af_panel.dta")

# panel <- panel[, c("prov_name", "district_name", "reu_id", "ndvi", "trt", "yearonly", "qtronly", "qtr")]
panel <- panel[, c("prov_name", "district_name", "reu_id", "ndvi", "trt", "yearonly")]

panel$prov_name <- as.character(panel$prov_name)
panel$district_name <- as.character(panel$district_name)

panel <- aggregate(panel, by=list(panel$reu_id, panel$yearonly), FUN = max)

panel$unique_dist <- paste(panel$prov_name, ",", panel$district_name)

panel <- aggregate(panel, by=list(panel$unique_dist, panel$yearonly), FUN = mean)

panel$matchid <- panel[,1]

panel <- panel[, c("ndvi", "trt", "yearonly", "matchid")]

panel <- reshape(panel, v.names = c("ndvi", "trt"), timevar = c("yearonly"), idvar = c("matchid"), direction = "wide")

###


newdata <- merge(panel, opioid, by="matchid")

newdata <- newdata[, !names(newdata) %in% c("x2017", "x2018", "trt.2006", "trt.2007", "ndvi.2006", "ndvi.2007")]

newdata <- reshape(newdata, varying = list(paste0("ndvi.", 2008:2016), paste0("trt.", 2008:2016), paste0("x", 2008:2016)), direction = "long")

names(newdata)[names(newdata)=="ndvi.2008"] <- "ndvi"
names(newdata)[names(newdata)=="trt.2008"] <- "trt"
names(newdata)[names(newdata)=="x2008"] <- "opiumProduction"


newdata$opiumProduction <- as.numeric(newdata$opiumProduction)

cor(newdata$trt, newdata$opiumProduction)
cor(x=newdata[complete.cases(newdata[,c("ndvi", "trt", "opiumProduction")]),c("ndvi", "trt", "opiumProduction")])
cor(x=newdata[complete.cases(newdata[,c("ndvi", "trt", "opiumProduction")])&newdata$opiumProduction<600,c("ndvi", "trt", "opiumProduction")])

summary(lm(newdata$opiumProduction ~ newdata$trt))
model <- lm(newdata$opiumProduction ~ newdata$trt)

noOutliers <- newdata[newdata$opiumProduction<100,]
cor(noOutliers$trt, noOutliers$opiumProduction)
summary(lm(noOutliers$opiumProduction ~ noOutliers$trt))
model2 <- lm(noOutliers$opiumProduction ~ noOutliers$trt)



install.packages("stargazer")
library(stargazer)
help("stargazer")

stargazer(model, out = "C:/Users/cbaehr/Desktop/full_model.html", type = "html")


