
* this script can only be run after performing the data processing steps in the 
* Af_Models.do do-file

* generating temperature variables with a one season lag
bysort reu_id (qtr): gen lag_meantemp = meantemp[_n-1]
bysort reu_id (qtr): gen lag_maxtemp = maxtemp[_n-1]
bysort reu_id (qtr): gen lag_mintemp = mintemp[_n-1]

* generating precipitation variables with a one season lag
bysort reu_id (qtr): gen lag_meancrup = meancrup[_n-1]
bysort reu_id (qtr): gen lag_maxcrup = maxcrup[_n-1]
bysort reu_id (qtr): gen lag_mincrup = mincrup[_n-1]

* generating placebo treatment variable. Moving each value of the treatment variable
* sixteen seasons (or four years) ahead of the actual value
bysort reu_id (qtr): gen placebo_trt = trt[_n+16]

* generating a log of ndvi variable
gen ndvi_for_log = ndvi
replace ndvi_for_log = 0.00001 if ndvi==0
gen log_ndvi = log(ndvi_for_log)

label variable trt "Treatment"
label variable meantemp "Mean Temp"
label variable maxtemp "Max Temp"
label variable mintemp "Min Temp"
label variable meancrup "Mean Precip"
label variable maxcrup "Max Precip"
label variable mincrup "Min Precip"
label variable log_ndvi "log(ndvi)"
label variable lag_meantemp "Lagged Mean Temp"
label variable lag_maxtemp "Lagged Max Temp"
label variable lag_mintemp "Lagged Min Temp"
label variable lag_meancrup "Lagged Mean Precip"
label variable lag_maxcrup "Lagged Max Precip"
label variable lag_mincrup "Lagged Min Precip"
label variable placebo_trt "Placebo Treatment"

* Main model with temp and precip covariates

reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup i.qtronly i.yearonly ///
	[pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
est sto c1
outreg2 using afreg.doc, replace drop(i.qtronly i.yearonly) ///
	addtext("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y) see label ///
	addnote("All models are estimated for observations with ndvi<0.99. (1) is the baseline model, including temperature and precipitation covariates. (2) only includes observations whose minimum NDVI value for 2012 (last year before treatments began) was >0.07. (3) lags each temperature and precipitation covariates by 1 quarter. (4) replaces actual treatment with a placebo treatment taking place 4 years before actual treatment quarter. (5) replaces the dependent variable, NDVI, with log(NDVI).")

* NDVI threshold test with basline NDVI threshold of 0.07. Temp and precip covariates

reghdfe ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup i.qtronly i.yearonly ///
	[pweight = canal_weight] if ndvi_2012_700==0, cluster(project_id qtr) absorb(reu_id)
est sto c2
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) ///
	addtext("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y) see label

* One season lagged temp and precip tests. Temp and precip covariates

reghdfe ndvi trt lag_meantemp lag_maxtemp lag_mintemp lag_meancrup lag_maxcrup ///
	lag_mincrup i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) ///
	absorb(reu_id)
est sto c3
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) ///
	addtext("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y) see label

* Placebo treatment test. Temp and precip covariates

reghdfe ndvi placebo_trt meantemp maxtemp mintemp meancrup maxcrup mincrup ///
	i.qtronly i.yearonly [pweight = canal_weight] if yearonly <= 2012, cluster(project_id qtr) ///
	absorb(reu_id)
est sto c4
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) ///
	addtext("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y) see label

* Log(ndvi) dependent var with temp and precip covariates
reghdfe log_ndvi trt meantemp maxtemp mintemp meancrup maxcrup mincrup i.qtronly i.yearonly [pweight = canal_weight], ///
	cluster(project_id qtr) absorb(reu_id)
est sto c5
outreg2 using afreg.doc, append drop(i.qtronly i.yearonly) ///
	addtext("Grid cell FEs", Y, "Season FEs", Y, "Year FEs",Y) see label
	
*******************

gen qtrtotrt = (yearonly-end_yearadj)*4 + (qtronly-end_quarter)

egen qtrtotrt_p = cut(qtrtotrt), at(-50 -9(1)10)

levelsof qtrtotrt_p, loc(levels) sep()

foreach l of local levels{
	local j = `l' + 50
	local label `"`label' `j' "`l'" "'
	}

cap la drop qtrtotrt_p `label'
la def qtrtotrt_p `label'

replace qtrtotrt_p = qtrtotrt_p + 50
la values qtrtotrt_p qtrtotrt_p

local climate "meantemp maxtemp mintemp meancrup maxcrup mincrup"

reghdfe ndvi i.qtrtotrt_p `climate' i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)

coefplot, keep(*.qtrtotrt_p) drop(0.qtrtotrt_p) xline(10) yline(0) vertical omit   recast(line) color(blue) ciopts(recast(rline)  color(blue) lp(dash) ) graphregion(color(white)) bgcolor(white)  ///
	xtitle("Quarters to Treatment") ytitle("Treatment effects on NDVI") saving("testgraph.gph", replace)
* graph save Graph "/Users/christianbaehr/Desktop/testgraph.gph", replace
graph export "trt_byqtr.png", replace









