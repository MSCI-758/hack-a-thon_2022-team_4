#hackathon

#303D_18 is a shapefile- which are imparied ve meeting standards
#the other folder is a shape file, biological information , more detail 
library(tidyverse)

#looking at impaired waters 
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
summary(imp) # There are 861 empty rows

# Get rid of empty rows
dim(imp) #1979 rows
imp = imp %>%
  filter(priorityRank != "") 
dim(imp) # 1979-861 = 1118 rows

summary(imp)
names(imp) <-  c("priorityRank", "note", "basin", "huc_12", "county", "description", "station", "use", "cause")
summary(imp)      

unique(imp$cause)

ggplot(data = imp)+
  geom_histogram(aes(x=cause, fill=priorityRank), stat="count", binwidth =5 )+
  coord_flip()

unique(imp$cause) 


# Seperating the causes 
imp_cause<- separate(imp,cause,
  into=c("cause1","cause2", "cause3", "cause4"),
  sep = ",")

tail(imp_cause, n=20)

imp_cause$totalCause <- 0

for(i in seq(dim(imp_cause)[1])){
  if(is.na(imp_cause$cause4)[i] == FALSE){
    imp_cause$totalCause[i] = 4
  }else if(is.na(imp_cause$cause3)[i] == FALSE){
        imp_cause$totalCause[i] = 3
    }else if(is.na(imp_cause$cause2)[i] == FALSE){
      imp_cause$totalCause[i] = 2
    }else if(is.na(imp_cause$cause1)[i] == FALSE){
      imp_cause$totalCause[i] = 1
    }
}



ggplot(data = imp_cause)+
  geom_histogram(aes(x=totalCause, fill=priorityRank), stat="count", binwidth =5 )+
  coord_flip()
#most only have 1, but the combo of all the different lists really clog up the image 


ggplot(data = imp_cause)+
  geom_histogram(aes(x=totalCause, fill=basin), stat="count", binwidth =5 )+
  coord_flip()
#seems distributed among the basins

table(imp_cause$totalCause, imp_cause$basin)

#how to do a count of all 

table(imp_cause[,c("cause1", "cause2", "cause3", "cause4")])

table(imp_cause$cause1, imp_cause$priorityRank)  
table(imp_cause$cause2, imp_cause$priorityRank)

causePR<- table(imp_cause$totalCause, imp_cause$priorityRank)
causePR<- as.matrix(causePR, row.names = TRUE, column.names = TRUE)

causePR_df <- as.data.frame.matrix(causePR, row.names = c(1,2,3,4), header=TRUE)
causePR_df$total <- 0
causePR_df$total <- c(sum(causePR_df[1,]),sum(causePR_df[2,]),sum(causePR_df[3,]),sum(causePR_df[4,])) 
causePR_df$percent2 <- causePR_df$`2`/causePR_df$total
causePR_df$percent3 <- causePR_df$`3`/causePR_df$total

#does this say anything IDK
causePR_df



#how to count them all 
test <- imp_cause %>%
  count(cause1, cause2, cause3, cause4)

table(imp_cause$totalCause)

apply(imp_cause[,c("cause1", "cause3")], 2, table)

as.matrix(table(imp_cause$cause1) + table(imp_cause$cause2) + table(imp_cause$cause3),
          + table(imp_cause$cause4))

dim(imp_cause)


cause2_df <-imp_cause %>%
  filter(!is.na(imp_cause$cause2))

ggplot(data = imp_cause)+
  geom_histogram(aes(x=cause1, fill=priorityRank), stat="count", binwidth =5 )+
  coord_flip()

ggplot(data = cause2_df)+
  geom_histogram(aes(x=cause2, fill=priorityRank), stat="count", binwidth =5 )+
  coord_flip()

#make a heatmap of those with 2 causes?? see if there is relation
#### heatmap-geom tile
heatmap_cause= cause2_df %>% 
  select(cause1, cause2) %>%
  group_by(cause1, cause2) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

### uhhh trying to do the heatmap idea
library(viridis)
dim(heatmap_cause)
sum(heatmap_cause$n)

#install.packages('viridis')
heatmap_2causes <- ggplot()+
  geom_tile(data=heatmap_cause, aes(x=cause2, y=cause1, fill=n),
            color="white",
            lwd =1.5,
            linetype=1)+
  theme_classic()+
  coord_fixed()+
  labs(fill="count", )+
  theme(axis.text.x= element_text(angle = 90, vjust=0.5, hjust=1))+
  scale_fill_viridis(option="rocket", begin=0.1)
ggsave(heatmap_2causes, file="figures/heatmap_2causes.png", width=5, height = 6)



# maggie cause split
imp = read_csv("data/2018303d_final.csv") # SC Impaired Waters List 303d for 2018
names(imp) <- c("prank","note","basin","huc12","county","desc","station","use","cause")
# generating new rows to deal with multiple causes - for Abby
imp_new = imp %>%
  filter(prank != "") %>%
  #separate(cause, c("cause1", "cause2", "cause3","cause4","cause5"))
  separate_rows(cause, prank, cause, convert = TRUE)
dim(imp_new)
