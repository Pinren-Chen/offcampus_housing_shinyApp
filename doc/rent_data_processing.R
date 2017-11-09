library(tidyr)
library(dplyr)
library(ggplot2)

rent<-as.data.frame(read.csv("/Users/ninashao/Desktop/rent.csv"))

region_rent=gather(rent,RegionName)
# region_rent
colnames(region_rent)<-c("regionname","time","rent")
region_rent$time<-gsub("X20", "", as.character(factor(region_rent$time)) )
region_rent$time<-gsub("[.]", "", as.character(factor(region_rent$time)) )
region_rent$year<-as.numeric(substr(region_rent$time, 0, 2))
region_rent<-region_rent%>%
  group_by(regionname,year)%>%
  summarise(rent=mean(rent))
save(region_rent,file="region_rent.RData")

target <- c("Harlem", "East Harlem")

region_rent<-filter(region_rent, regionname %in% target)
ggplot(data=region_rent,
       aes(x=year, y=rent, colour=regionname)) +geom_line()
# +theme(legend.position = c(0.9, 0.98),
#                     legend.justification=c(0.5, 1))
