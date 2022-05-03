

library(tidyverse)
library(sf)
library(tidyverse)
library(ggpubr)
library(gridExtra)

#######
basin_sf = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
counties = st_read("data/tl_2016_45_cousub")
shp_imp = st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
shp_all = st_read("data/bw.SDE.STATIONS", "bw.SDE.STATIONS")
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
names(imp) <- c("prank","note","basin","huc12","county","desc","station","use","cause")
imp = imp %>%
  filter(prank != "") 

counties_new = counties %>% # changing to upper case county names
  mutate(NAME=toupper(NAME)) %>%
  rename(county = NAME)

imp_new = imp %>%
  left_join(counties_new,by=("county"))
group_mean <- aggregate(cause~county, data = imp_new,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] })

mean_plot = group_mean %>%
  left_join(counties_new,by="county")

test = mean_plot %>%
  left_join(counties_new)

counties_matchname = counties_new %>%
  rename("BEAUFORT-PORT ROYAL"= "BEAUFORT"
         )
plot1= ggplot() +
  geom_sf(data=st_transform(counties_new)) +
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=cause)) +
  theme_bw()
plot1




