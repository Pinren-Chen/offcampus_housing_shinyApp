#######subway station######
sub.station<-read.csv("~/Desktop/App/data/SUBWAY_STATION.csv",header=T)
sub.station$the_geom<-gsub("\\(|POINT|\\)","",sub.station$the_geom)
library(stringr)
sub.station$lng<-as.numeric(str_split_fixed(sub.station$the_geom," ",3)[,2])
sub.station$lat<-as.numeric(str_split_fixed(sub.station$the_geom," ",3)[,3])
sub.station$info<-paste0("Line ",sub.station$LINE," @ ",sub.station$NAME)
sub.station$cat<-rep("sub",nrow(sub.station))
sub.station<-sub.station[,(ncol(sub.station)-3):ncol(sub.station)]
save(sub.station, file="../Output/sub.station.RData")
write.csv(sub.staion,file="sub.station.csv")


#######bus stop##########
bus.stop<-read.csv("~/Desktop/App/data/Bus_Stop_Shelter.csv",header=T)
head(bus.stop)
bus.stop$lng<-bus.stop$LONGITUDE
bus.stop$lat<-bus.stop$LATITUDE
bus.stop$info<-paste0(bus.stop$CORNER," ",bus.stop$LOCATION," & ",bus.stop$AT_BETWEEN)
bus.stop$cat<-rep("bus",nrow(bus.stop))

save(bus.stop, file="~/Desktop/App/output/bus.stop.full.RData")
#trans.data<-rbind(sub.station[,(ncol(sub.station)-3):ncol(sub.station)],bus.stop[,(ncol(bus.stop)-3):ncol(bus.stop)])
#trans.data<-rbind(sub.station,bus.stop)


#####filter for bus stop###########
library("zipcode")
data(zipcode)
Manhattan1 <- c(10026, 10027, 10030, 10037, 10039,10001, 10011, 10018, 10019, 10020, 10036,10029, 10035,10010, 10016, 10017, 10022,10012, 10013, 10014,10004, 10005, 10006, 10007, 10038, 10280,10002, 10003, 10009,10021, 10028, 10044, 10065, 10075, 10128,10023, 10024, 10025,10031, 10032, 10033, 10034, 10040)
Manhattan1 <- as.character(Manhattan1)
ny.zip<-subset(zipcode, city=='New York'&state=='NY')

cal <- function(data,y){
  table <- y[,4:5]                 #long&lat in data(zipcode)
  table[,1] <- as.numeric(data[2]) #longilute col
  table[,2] <- as.numeric(data[1]) #latitude  col
  dif <- y[,4:5] - table
  differ <- (dif^2)[,1] + (dif^2)[,2]
  return(y[which.min(differ),1])
}


bus.stop$zip <- apply(bus.stop,1,cal,y=ny.zip)
nrow(bus.stop)
bus.stop<-subset(bus.stop, bus.stop$zip%in% Manhattan1)

save(bus.stop, file="../output/bus.stop.RData")
bus.stop<-bus.stop[,(ncol(bus.stop)-3):ncol(bus.stop)]
write.csv(sub.staion,file="bus.sta.csv")