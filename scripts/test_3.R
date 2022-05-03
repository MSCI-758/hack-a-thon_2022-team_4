
library(tidyverse)
library(sf)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(wesanderson)

#######
basin_sf = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
counties = st_read("data/tl_2016_45_cousub")
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
names(imp) <- c("prank","note","basin","huc12","county","desc","station","use","cause")
imp_new = imp %>%
  filter(prank != "") %>%
  #separate(cause, c("cause1", "cause2", "cause3","cause4","cause5"))
  separate_rows(cause, prank, cause, convert = TRUE)

counties_new = counties %>%
  mutate(NAME=toupper(NAME)) %>%
  rename(county = NAME) %>%
  separate(county, c("county", "county2"),sep="-")

group_mean <- aggregate(cause~county, data = imp_new,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] })
mean_plot = group_mean %>%
  left_join(counties_new,by="county")
plot1= ggplot() +
  geom_sf(data=st_transform(counties_new)) +
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=cause)) +
  theme_bw() +
  labs(fill="Causes")+
  theme(legend.position="bottom") 

ggsave(plot1,filename='figures/map_imp_causes_1.png',device="png",height=7,width=8)

group_mean <- aggregate(use~county, data = imp_new,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] })
mean_plot = group_mean %>%
  left_join(counties_new,by="county")
plot2= ggplot() +
  geom_sf(data=st_transform(counties_new)) +
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=use)) +
  theme_bw() +
  labs(fill="Uses")+
  theme(legend.position="bottom")
ggsave(plot2,filename='figures/map_imp_uses_1.png',device="png",height=7,width=8)
bigplot=ggarrange(plot1,plot2,ncol =2,nrow=1,labels=c("A","B"))
ggsave(bigplot,filename='figures/map_imp_causes_the.png',device="png",height=5,width=8)

# BY BASIN - not county
shp_imp = st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
shp_imp_NA = shp_imp %>%
  filter(!is.na(IMPAIRMENT)) %>%
  filter(!is.na(USE_))
cats <- shp_imp_NA  %>% 
  st_join(basin_sf) %>%
  group_by(USE_,Basin) %>%
  select(CNTYNAME,geometry,USE_,Basin,IMPAIRMENT)

group_mean <- aggregate(USE_~Basin, data = cats,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] }) 
mean_plot <- group_mean %>%
  left_join(basin_sf,by=c("Basin"))
plot1= ggplot() +
  theme_void()+
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=USE_)) 

group_mean <- aggregate(USE_~Basin, data = cats,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] }) 
mean_plot <- group_mean %>%
  left_join(basin,by=c("Basin"))
plot3= ggplot() +
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=USE_)) +
  theme_bw() +
  labs(fill="Uses")+
  theme(legend.position="bottom") 
ggsave(plot3,filename='figures/map_imp_uses_basin.png',device="png",height=7,width=8)



cats <- shp_imp_NA  %>% 
  st_join(basin_sf) %>%
  group_by(IMPAIRMENT,Basin) %>%
  select(CNTYNAME,geometry,USE_,Basin,IMPAIRMENT)
group_mean <- aggregate(IMPAIRMENT~Basin, data = cats,     function(x) { 
  ux <- unique(x) 
  ux[which.max(tabulate(match(x, ux)))] }) 
mean_plot <- group_mean %>%
  left_join(basin_sf,by=c("Basin"))
plot4= ggplot() +
  geom_sf(data=mean_plot,aes(geometry=geometry,fill=IMPAIRMENT)) +
  theme_bw() +
  labs(fill="Impairment")+
  theme(legend.position="bottom") 
ggsave(plot4,filename='figures/map_imp_causes_basin.png',device="png",height=7,width=8)


