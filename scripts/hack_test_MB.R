
##############
library(tidyverse)
library(sf)
library(tidyverse)
library(ggpubr)
library(gridExtra)
##############
basin_sf = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
counties = st_read("data/tl_2016_45_cousub")
shp_imp = st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
shp_all = st_read("data/bw.SDE.STATIONS", "bw.SDE.STATIONS")
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
names(imp) <- c("prank","note","basin","huc12","county","desc","station","use","cause")
imp = imp %>%
  filter(prank != "") 

shp_imp_NA = shp_imp %>%
  filter(!is.na(IMPAIRMENT)) %>%
  filter(!is.na(USE_))
###########

basin_up = basin_sf %>%
  mutate(basin=toupper(Basin))

cats <- imp %>% 
  left_join(basin_up,by=c("basin")) 
group_mean <- aggregate(cause~basin, data = cats,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] }) 
mean_plot <- group_mean %>%
  left_join(basin_up,by=c("basin"))
plot1= ggplot() +
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=cause)) +
  theme_bw()
plot1
##############
cats <- shp_imp_NA %>% 
  st_join(basin_sf) %>%
  group_by(USE_,Basin) %>%
  select(CNTYNAME,geometry,USE_,Basin,IMPAIRMENT)

group_mean <- aggregate(USE_~Basin, data = cats,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] }) 
mean_plot <- group_mean %>%
  left_join(basin,by=c("Basin"))

  plot1= ggplot() +
    theme_void()+
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=USE_)) 
  
  group_mean <- aggregate(IMPAIRMENT~Basin, data = cats,     function(x) { 
    ux <- unique(x) 
    ux[which.max(tabulate(match(x, ux)))] }) 
  mean_plot <- group_mean %>%
    left_join(basin,by=c("Basin"))
  
  plot2= ggplot() +
    theme_void()+
    geom_sf(data=mean_plot,aes(geometry=geometry,fill=IMPAIRMENT)) 
  bigplot=ggarrange(plot1,plot2,ncol =2,nrow=1,labels=c("A","B"))
  bigplot
  ggsave(bigplot,filename='figures/map_imp.png',device="png",height=5,width=7)