packages.used=c("dplyr", "geosphere", "zipcode", "tigris", "sp","maptools","broom","httr","rgdal")
# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

# devtools::install_github("hadley/ggplot2@v2.2.0",force=TRUE)
# devtools::install_github('dkahle/ggmap',force=TRUE)

library(dplyr)
library(geosphere)
library(zipcode)
library(tigris)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
# library(RColorBrewer)

# load zip code data
data(zipcode)

# Pick out New York City Zipcode 
ny<- subset(zipcode, state=='NY'&city=='New York')
summary(ny)

###### map lat and long into zipcode level 
crime <- read.csv("NYPD_Complaint_Data_Current_YTD.csv")
nyc.crime <- subset(crime, crime$BORO_NM == "MANHATTAN" )

# create distance matrix
mat <- distm(nyc.crime[,c('Longitude','Latitude')], ny[,c('longitude','latitude')], fun=distVincentyEllipsoid)

# assign the zip code to the point in nyc crime data based on shortest distance in the matrix: May take more than 15 mins 
nyc.crime$locality <- ny$zip[max.col(-mat)]

# there are some NAs in Latitude and longtitude, which need to clean out 
nyc.crime$Longitude[nyc.crime$locality == 'NA']

nyc.crime.update<- 
  nyc.crime%>%
  filter(!is.na(locality)) 

nyccrime<-read.csv("/Users/ninashao/Desktop/ADS/Project2/data/Crime /NYC_Crime.csv") 
##### data Processing part 
# input geojson file
r<-GET('http://catalog.civicdashboards.com/dataset/11fd957a-8885-42ef-aa49-5c879ec93fac/resource/28377e88-8a50-428f-807c-40ba1f09159b/download/nyc-zip-code-tabulation-areas-polygons.geojson')
nyc <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
load("/Users/ninashao/Desktop/ADS/Project2/data/Crime /nyccrime.RData")
# calculate number of crimes for each zip code 
# nyccrime$locality<- as.character(nyccrime$locality)
points_by_zip <- nyccrime %>%
  group_by(locality) %>%
  summarize(num_points=n())
summary(points_by_zip)

# add num_points (number of crimes zip code level)
subdat_data=nyc@data[,c("PO_NAME", "postalCode")]
data<-left_join(subdat_data, points_by_zip, by= c('postalCode'='locality'))
data$num_points[data$num_points == 'NA'] <- 0
nyc@data$count = data$num_points

# output data as Rdata for drawing map in server
save(nyc,file="/Users/ninashao/Desktop/nyc.Rdata")