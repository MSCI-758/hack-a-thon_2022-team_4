# Hackathon 2022 Team 4 CMTK Priority Rank vs. Cause 

library(tidyverse)
library(sf)

imp = read.csv("data/2018303d_final.csv") #read impaired waters data set 

imp = imp %>%
  filter(PRIORITY.RANK != "")

names(imp) = c("priority_rank", "note", "basin", "huc_12", "county", "description", "station", "use", "cause")

unique(imp$use)

basin = st_read("data/SC_Major_River_Basins", "SC_Major_River_Basins")
shp_imp = st_read("data/bw.SDE.DHEC_303D_18", "bw.SDE.DHEC_303D_18")
shp_imp = shp_imp %>%
  filter(!is.na(USE_))

ggplot() +
  geom_sf(data = basin) +
  geom_sf(data = shp_imp, aes(color = USE_)) +
  transition_reveal(OBJECTID)


install.packages("gganimate")
install.packages("gifski")
install.packages("av")
library(gganimate)
library(gifski)

ggplot(data = imp) +
  geom_histogram(aes(x = use, fill = priority_rank), stat = "count", binwidth = 5) +
  coord_flip()

imp_AL = imp %>%
  filter(use == "AL")

ggplot(data = imp_AL) +
  geom_histogram(aes(x = priority_rank), stat = "count", binwidth = 5)

imp_REC = imp %>%
  filter(use == "REC")

ggplot(data = imp_REC) +
  geom_histogram(aes(x = priority_rank), stat = "count", binwidth = 5)

imp_FISH = imp %>%
  filter(use == "FISH")

ggplot(data = imp_FISH) +
  geom_histogram(aes(x = priority_rank), stat = "count", binwidth = 5)

imp_SHELLFISH = imp %>%
  filter(use =="SHELLFISH")

ggplot(data = imp_SHELLFISH) +
  geom_histogram(aes(x = priority_rank), stat = "count", binwidth = 5)


