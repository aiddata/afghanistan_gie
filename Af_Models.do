
*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/afghanistan_gie"
global project "/Users/rbtrichler/Box Sync/afghanistan_gie"

*Import file
import delimited "$project/ProcessedData/af_panel.csv", clear

destring actual_end_date_iso, force replace
destring ndvi, force replace

*Create canal id
encode project_id, generate (canal_id)

* Generate weights
sort canal_id
egen canal_cells = count(reu_id), by (canal_id qtr) 
g	canal_weight = 1 / canal_cells

save "${data}\all_community_panel_gimms", replace


reghdfe ndvi trt [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)

reghdfe ndvi trt i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
