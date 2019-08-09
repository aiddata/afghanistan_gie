
global results "C:/Users/cbaehr/Downloads"

import delimited "C:/Users/cbaehr/Box Sync/afghanistan_gie/ProcessedData/district_panel.csv", clear

replace ndvi = "." if ndvi == "NA"
destring ndvi, replace

rename matchid district_num

reghdfe ndvi trt, cluster(district_num year) absorb(district_num year)
outreg2 using "$results/opioid_model.doc", replace noni nocons addtext("Year FEs", Y, "District FEs", Y)
reghdfe ndvi trt opiumproduction, cluster(district_num year) absorb(district_num year)
outreg2 using "$results/opioid_model.doc", append noni nocons addtext("Year FEs", Y, "District FEs", Y)

