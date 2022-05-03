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
head(shp_all)
plot(shp_all)
# SC 2018 303d list: https://scdhec.gov/bow/south-carolina-303d-list-impaired-waters-tmdls
# Converted 303d list from .xls to .csv before reading in
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
summary(imp) # There are 861 empty rows

# Get rid of empty rows
dim(imp) #1979 rows
imp = imp %>%
  filter(PRIORITY.RANK != "") 
dim(imp) # 1979-861 = 1118 rows

# Plot locations of impaired waters and stations that are not on the list (either bc they meet water quality standards or have a TMDL)
# TMDL = Total Maximum Daily Load - indicates site where approved pollutant limits have been identified and waters are being managed to meet these TMDL
ggplot() +
  geom_sf(data=imp, aes(color=STATUS))

# Plot boundaries of 8 major river basins in SC
ggplot() +
  geom_sf(data=basin, aes(fill=Basin))

#shp_imp has a lot of cool data look into more
summary(shp_imp)
table(shp_imp$IMPAIRMENT)
table(shp_imp$STAT)

head(shp_imp$STATUS)

total_bodies<- table(shp_imp$STATUS, shp_imp$BASIN)


head(total_bodies)
total_bodies<- data.frame(total_bodies)

total_bodies <- total_bodies %>%
  pivot_wider( c("IMPAIRED", "NOT ON LIST"))


