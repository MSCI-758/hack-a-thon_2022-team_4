# hack-a-thon 2022

library(tidyverse)
library(sf)

# Read in boundaries of 8 major river basins in SC
# https://sc-department-of-health-and-environmental-control-gis-sc-dhec.hub.arcgis.com/datasets/sc-major-river-basins/explore
basin = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
basin

# Read in shapefile of all sites assessed for 2018 303d list
# Provided by Wade Cantrell at SC DHEC on 2022-04-11
shp_imp = st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
shp_imp

# Larger shapefile of ALL stations (including shellfish monitoring sites, macroinvertebrate sites, much more than the ambient water quality sites used for the 303d list)
# Provided by Wade Cantrell at SC DHEC on 2022-04-11
shp_all = st_read("data/bw.SDE.STATIONS", "bw.SDE.STATIONS")

# SC 2018 303d list: https://scdhec.gov/bow/south-carolina-303d-list-impaired-waters-tmdls
# Converted 303d list from .xls to .csv before reading in
imp = read.csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
summary(imp) # There are 861 empty rows

# Get rid of empty rows
dim(imp) #1979 rows
imp = imp %>%
  filter(priority_rank != "") 
dim(imp) # 1979-861 = 1118 rows

# Plot locations of impaired waters and stations that are not on the list (either bc they meet water quality standards or have a TMDL)
# TMDL = Total Maximum Daily Load - indicates site where approved pollutant limits have been identified and waters are being managed to meet these TMDL
ggplot() +
  geom_sf(data=shp_imp, aes(color=STATUS))

# Plot boundaries of 8 major river basins in SC
ggplot() +
  geom_sf(data=basin, aes(fill=basin))

summary(shp_imp)
table(shp_imp$IMPAIRMENT)
table(shp_imp$STATUS, shp_imp$BASIN)

(79/(79+117))*100


BROAD_IMPAIRED=(79/(79+117))*100
CATAWBA_IMPAIRED=(104/(32+104))*100
EDISTO_IMPAIRED=(95/(95+95))*100
PEEDEE_IMPAIRED=(229/(229+170))*100
SALKEHATCHIE_IMPAIRED=(128/(128+171))*100
SALUDA_IMPAIRED=(121/(121+76))*100
SANTEE_IMPAIRED=(171/(171+260))*100
SAVANNAH_IMPAIRED=(113/(113+177))*100



basin_percent_imp<- rbind(BROAD_IMPAIRED, CATAWBA_IMPAIRED, EDISTO_IMPAIRED, PEEDEE_IMPAIRED, SALKEHATCHIE_IMPAIRED,SALUDA_IMPAIRED,SAVANNAH_IMPAIRED)
  
basin_percent_imp <- as.data.frame.matrix(basin_percent_imp)

basin_percent_imp$V1 <- round(basin_percent_imp$V1, 0)

names(basin_percent_imp) <- "percent"


t.test(basin_percent_imp)
