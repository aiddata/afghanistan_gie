
library(foreign)

setwd("/Users/christianbaehr/Box Sync/afghanistan_gie")

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

panel <- panel[, c("prov_name", "district_name", "reu_id", "ndvi", "trt", "yearonly", "qtronly", "qtr")]

test <- aggregate(panel, by=list(panel$reu_id, panel$yearonly), FUN = max)

test$unique_dist <- paste(test$prov_name, ",", test$district_name)

test <- aggregate(test, by=list(test$unique_dist, test$yearonly), FUN = mean)

###

















