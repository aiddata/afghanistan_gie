
*Clear memory
clear

*Set local macros
global project "/Users/careyglenn/Box Sync/afghanistan_gie"
global project "/Users/rbtrichler/Box Sync/afghanistan_gie"

*Import file
import delimited "$project/ProcessedData/af_panel.csv", clear

destring actual_end_date_iso, force replace



* Generate weights
bys project_id: egen canal_cells = count(reu_id) if qtr==20061
sort project_id canal_cells
by project_id: replace canal_cells=canal_cells[1]
g	canal_weight = 1 / canal_cells

save "${data}\all_community_panel_gimms", replace


reghdfe ndvi trt [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)

reghdfe ndvi trt i.qtronly i.yearonly [pweight = canal_weight], cluster(project_id qtr) absorb(reu_id)
