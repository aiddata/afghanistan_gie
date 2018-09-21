
*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/afghanistan_gie"
global project "/Users/rbtrichler/Box Sync/afghanistan_gie"

*Import file

*import delimited "$project/ProcessedData/af_panel.csv", clear

*destring actual_end_date_iso, force replace
*destring ndvi, force replace

*Read file
use "$project/ProcessedData/af_panel.dta", clear

* Generate weights
*sort project_id
*egen canal_cells = count(reu_id), by (project_id qtr) 
*g	canal_weight = 1 / canal_cells

*generate baseline ndvi from 2012 q4
bys reu_id (qtr): gen ndvi_20124=ndvi[28]

*gen categorical distance to canal var, both quartiles and set values

xtile dist_canal_quart=dist_canal,n(4) 

generate dist_canal_cat = .
replace dist_canal_cat = 1 if (dist_canal<31)
replace dist_canal_cat=2 if (dist_canal>=31) & (dist_canal<91)
replace dist_canal_cat=3 if (dist_canal>=91) & (dist_canal<152)
replace dist_canal_cat=4 if (dist_canal>=152)

sum dist_canal, detail
sum dist_canal if dist_canal_cat==1
sum dist_canal if dist_canal_cat==2

*gen quartile distance to canal start

xtile dist_start_quart=dist_start,n(4)

* gen categorical distance to canal start

gen dist_start_cat=.
replace dist_start_cat=1 if (dist_start<750)
replace dist_start_cat=2 if (dist_start>=750) & (dist_start<1454)
replace dist_start_cat=3 if (dist_start>=1454) & (dist_start<1768)
replace dist_start_cat=4 if (dist_start>=1768)

* gen categorical ndvi at baseline var

xtile ndvi_20124_cat=ndvi_20124,n(4)

sum ndvi_20124, detail
sum ndvi_20124 if ndvi_20124_cat==1
sum ndvi_20124 if ndvi_20124_cat==3

* Create year average for 2012 baseline year
egen ndvi_baseline=mean(ndvi) if yearonly==2012, by (reu_id)
bys reu_id (qtr): gen ndvi_2012=ndvi_baseline[28]
*make quartile var for 2012 average
xtile ndvi_2012_cat=ndvi_2012,n(4)

*Create year minimum for 2012 baseline year
egen ndvi_base_min=min(ndvi) if yearonly==2012, by(reu_id)
bys reu_id (qtr): gen ndvi_2012_min=ndvi_base_min[28]

*Create identifier var if 2012 min value is less than 300

gen ndvi_2012_300=0
replace ndvi_2012_300=1 if ndvi_2012_min<=.003

* Create pre-treatment dummy for all years 2006-2012

gen pre_proj=0
replace pre_proj=1 if (yearonly<2013)
keep if ndvi<0.99

* save "${data}\all_community_panel_gimms", replace

*****************
** MAIN MODELS **
*****************

reghdfe ndvi trt [pweight = canal_weight], cluster(canal_id qtr) absorb(reu_id)

*No covars*
reghdfe ndvi trt i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
outreg2 using afreg.doc, replace drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*Add temp and precip covars*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c2
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*Peak Qtr ID interaction*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup peakqtr_id trt#peakqtr_id i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c3
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*NDVI at baseline interaction*

reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##ndvi_2012_cat i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c4
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*reghdfe ndvi trt ndvi_20124 meantemp maxtemp mintemp meancrup maxcrup mincrup trt#c.ndvi_20124 i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
*reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##ndvi_20124_cat i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
*reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt#c.ndvi_2012 i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)

* Distance to Canal*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##dist_canal_cat i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c5
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*Distance to Start of Canal*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##dist_start_cat i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c6
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*reghdfe ndvi trt dist_canal dist_start meantemp maxtemp mintemp meancrup maxcrup mincrup trt#c.dist_start#c.dist_canal i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)



**Robustness Checks with 0.03 threshold**

* No Covars
reghdfe ndvi trt i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
outreg2 using afreg300.doc, replace drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*Add temp and precip covars*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c2
outreg2 using afreg300.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*Peak Qtr ID interaction*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup peakqtr_id trt#peakqtr_id i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c3
outreg2 using afreg300.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*NDVI at baseline interaction*

reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##ndvi_2012_cat i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c4
outreg2 using afreg300.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

* Distance to Canal*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##dist_canal_cat i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c5
outreg2 using afreg300.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)

*Distance to Start of Canal*
reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##dist_start_cat i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c6
outreg2 using afreg300.doc, append drop(i.qtronly i.yearonly) addtext ("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y)




reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##dist_canal_cat i.qtronly i.yearonly if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id) 

reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup trt##dist_start_cat i.qtronly i.yearonly /*
*/ if ndvi_2012_300==0 [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)


** Weighted summary stats

reg ndvi [pweight=canal_weight] if pre_proj==1

univar ndvi if pre_proj==1

*for ndvi_2012_cat quartiles to use in paper
sum ndvi_2012,d





