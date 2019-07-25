#usaid afghanistan_gie

##source of data files that are used in this project

## canal_data folder

original_project_data.csv -- taken from information provided by USAID Afghanistan (original is saved in Google Drive "project management/Geospatial Impact Evaluation/USAID Afghanistan/Geospatial Impact Eval?s (GIEs) w AidData/GIS Data & Maps/ARTF OFWMP Data/OFWMP-Phase0 Schemes List.xls), used to conduct task to map canals from PDFs

canal_lines.geojson -- lines of each canal, created through mapping activity

canal_polygons.geojoson -- polygon surrounding each canal (often called "command area" in PDF maps) that we used as cultivated area; in some cases was manually created based on size of command area for other similarly sized canals in that province

canal_starts.geojson -- starting points of each canal (used for distance to canal start)

canal_point_grid.geojson - point representing all grid cells used in analysis, which fill cultivated polygon surrounding each canal


## Scripts

Afghan_BuildPanel.R -- merging all data sources and formatting dataset into panel

Af_Models.do -- main modeling 

Af_Models_robustness.do -- robustness checks

see "afghanistan_tables_figures.xlsx" for chart explaining location and source data for all figures and tables in OFWMP final report


## OTHER DATA TOO BIG FOR GIT; STORED ON BOX "afghanistan_gie"

## Box folder: inputData

data provided via extracts from SciClone

## Box folder: ProcessedData

af_panel.csv -- file resulting from spatial merge process in R; imported into Stata for analysis

af_panel.dta -- dataset used for analysis in Stata

af_panel_10000.dta -- original dataset used for analysis in Stata, before dropping all observations with NDVI = 1

- some files have _badextract attached to end -- some extracts provided with incorrect information because used points to represent grid cells instead of actual boxes and this messed up some of the SciClone processes; problem was resolved


