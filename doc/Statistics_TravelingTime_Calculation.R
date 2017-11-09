
# check packages that need to be installed.
# packages.needed=setdiff("ggmap", 
#                         intersect(installed.packages()[,1], 
#                                   packages.used))
# # install additional packages
# if(length(packages.needed)>0){
#   install.packages(packages.needed, dependencies = TRUE)
# }

library(ggmap)

# list out starting point locations 
place_name <- c("Midtown","NoHo","Tudor City","Greenwich Village","East Village","Murray Hill","Upper East Side","Battery Park","Garment District",
  "Upper West Side","Washington Heights","Financial District","Lower East Side","Gramercy","Chelsea, New York, NY","Harlem","East Harlem",
  "Flatiron District","Little Italy","Morningside Heights","Turtle Bay, Manhattan, New York, NY,","West Village","Clinton, Manhattan, New York, NY")

# calculate traveling time (driving)--Columbia
traveling.columbia<-lapply(place_name,function(x) mapdist(from= x, to  = "Columbia University New York, NY",mode = "driving"))
time.columbia<-matrix(NA,nrow=23,ncol=3)
for (i in 1:23){
  time.columbia[i,1]<-traveling.columbia[[i]]$from
  time.columbia[i,2]<-as.numeric(traveling.columbia[[i]]$minutes)
  time.columbia[i,3]<-as.numeric(traveling.columbia[[i]]$miles)
}
# time.columbia
time.columbia<-as.data.frame(time.columbia)
colnames(time.columbia)<-c("Neighbourhood","Columbia_Time","Miles")

# calculate traveling time (driving)--NYU
traveling.NYU<-lapply(place_name,function(x) mapdist(from= x, to  = "New York University New York, NY",mode = "driving"))
time.NYU<-matrix(NA,nrow=23,ncol=3)
for (i in 1:23){
  time.NYU[i,1]<-traveling.NYU[[i]]$from
  time.NYU[i,2]<-as.numeric(traveling.NYU[[i]]$minutes)
  time.NYU[i,3]<-as.numeric(traveling.NYU[[i]]$miles)
}
# time.NYU
time.NYU<-as.data.frame(time.NYU)
colnames(time.NYU)<-c("Neighbourhood","NYU_Time","Miles")


traveling.Fordham<-lapply(place_name,function(x) mapdist(from= x, to  = "Fordham Graduate School of Business, West 60th Street, New York, NY",mode = "driving"))
time.Fordham<-matrix(NA,nrow=23,ncol=3)
for (i in 1:23){
  time.Fordham[i,1]<-traveling.Fordham[[i]]$from
  time.Fordham[i,2]<-as.numeric(traveling.Fordham[[i]]$minutes)
  time.Fordham[i,3]<-as.numeric(traveling.Fordham[[i]]$miles)
}
time.Fordham<-as.data.frame(time.Fordham)
colnames(time.Fordham)<-c("Neighbourhood","Fordham_Time","Miles")

time<- cbind(time.columbia,time.NYU$NYU_Time,time.Fordham$Fordham_Time)
time$Neighbourhood<- c("Midtown","NoHo","Tudor City","Greenwich Village","East Village","Murray Hill","Upper East Side","Battery Park","Garment District",
                       "Upper West Side","Washington Heights","Financial District","Lower East Side","Gramercy","Chelsea","Harlem","East Harlem",
                       "Flatiron District","Little Italy","Morningside Heights","Turtle Bay","West Village","Clinton")
time
# Output results as csv file
write.csv(time,"/Users/ninashao/Desktop/traveling_Time.csv")
