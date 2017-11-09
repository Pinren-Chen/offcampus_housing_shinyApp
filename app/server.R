library(shiny)
library(leaflet)
library(data.table)
#library(choroplethrZip)
library(devtools)
library(MASS)
#library(vcd)
#library(zipcode)
library(dplyr)
library(tigris)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)

library(XML)
library(DT)
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("dplyr")
library(dplyr)
library(ggplot2)


#housing<- read.csv("../data/truliaRentPrice/housing_geo.csv",header=TRUE, stringsAsFactors =FALSE)
#housing<- subset(housing, !is.na(lng))
#save(housing, file="../output/housing.RData")
#markets<- read.csv("../data/markets.csv",header=TRUE, stringsAsFactors =FALSE)
#markets<- subset(markets, !is.na(longitude))
#markets<- markets[(markets$Square.Footage>=1500)&(markets$City=="NEW YORK"), ]
#save(markets, file="../output/markets.RData")
#restaurant=read.csv("../data/restaurant_data.csv",header=TRUE, stringsAsFactors =FALSE)
#save(restaurant, file="../output/restaurant.RData")
load("../output/markets.RData")
load("../output/restaurant.RData")
load("../output/sub.station.RData")
load("../output/bus.stop.RData")
load("../output/housing.RData")  
load("../output/nyc.RData")
load("../output/rank.Rdata")
load("../output/rent.Rdata")
load("../output/region_rent.Rdata")
load("../output/rank_all.Rdata")
source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")
color <- list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
              color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
              color3 = c("#F7FCF5","#74C476", "#005A32"))
bin <- list(bin1 = c(0,500,1000,1500,2000,2500,3000), bin2 = c(0,1,2,3,4,5,6,7))
pal <- colorBin(color[[1]], bins = bin[[1]])


shinyServer(function(input, output,session) {
  
  #Esri.WorldTopoMap
  #########main map######
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      addMarkers(data=housing,
               lng=~lng,
               lat=~lat,
               clusterOptions=markerClusterOptions(),
               group="housing_cluster"
    )
  })
  
  
  
  #############Housing#############
  
  
  # filter housing data:
  
  housingFilter=reactive({
    bedroom_filter=housing$bedrooms>input$min_bedrooms 
    bathroom_filter=housing$bathrooms>input$min_bathrooms
    price_filter=housing$price>=input$min_price & housing$price<=input$max_price
    filter=bedroom_filter & bathroom_filter & price_filter
    return(housing[filter,])
  })
  
  # show data in the map:
  observe({leafletProxy("map")%>%clearGroup("housing_cluster")%>%
      addMarkers(data=housingFilter(),
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })
  # show current status of icons:
  
  showStatus=reactive({
    if (is.null(input$map_bounds)){
      return("cloud")
      
    }
    else{
      if(input$map_zoom<16){
        return('cloud')
      }
      else{
        return('details')
      }
    }
  })
  # hide and show clouds 
  observe({
    if(showStatus()=="cloud"){
      
      leafletProxy("map") %>%showGroup("housing_cluster")%>%clearGroup("new_added")
    }
    else{
      leafletProxy("map") %>%hideGroup("housing_cluster")
      
    }
  })
  
  # show housing details when zoom to one specific level
  
  observe({
    if(showStatus()=="details"){
      if(nrow(marksInBounds())!=0){
        leafletProxy("map")%>%clearGroup(group="new_added")%>% 
          addCircleMarkers(data=marksInBounds(),
                           lat=~lat,
                           lng=~lng,
                           label=~as.character(price),
                           radius=5,
                           stroke=FALSE,
                           fillColor = "green",
                           fillOpacity=0.7,
                           group="new_added",
                           labelOptions = labelOptions(
                             noHide = T,
                             offset=c(20,-15),
                             opacity=0.7,
                             direction="left",
                             style=list(
                               background="green",
                               color="white"  
                             )
                           )
          )
      }
      else{
        leafletProxy("map")%>%clearGroup(group="new_added")
      }
      
      
      
      
    }
    
    
    
    
  })
  
  # get the housing data in the bounds
  marksInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(housing[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    return(
      subset(housingFilter(),
             lat>= latRng[1] & lat <= latRng[2] &
               lng >= lngRng[1] & lng <= lngRng[2])
    )
  })
  
  # sort housing in current zoom level
  
  observe({
    
    housing_sort=marksInBounds()
    
    if(nrow(housing_sort)!=0){
      
      action=apply(housing_sort,1,function(r){
        addr=r["addr"]
        lat=r["lat"]
        lng=r["lng"]
        paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",addr,'</a>')   
      }
      )
      
      housing_sort$addr=action
      output$rank <- renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")],escape=FALSE)
      
      
      
      
    }
    else{
      
      output$rank=renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")])
    }
    
  })
  
  # When point in map is hovered, show a popup with housing info
  observe({
    
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    if(showStatus()=="details"){
      isolate({
        showPopupHover(event$lat, event$lng,housing=housingFilter())
      })  
    }
    
  })
  
  # mouseout the point and cancel popup
  observe({
    
    event <- input$map_marker_mouseout
    if (is.null(event))
      return()
    
    isolate({
      leafletProxy("map") %>% clearPopups()
    })
  })
  
  # click name to go to that point
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      
      
      
      lat <- as.numeric(input$goto$lat)
      lng <- as.numeric(input$goto$lng)
      
      map %>% setView(lng = lng, lat = lat, zoom = 16)
    })
  })
  # hover the list to show info
  observe({
    if (is.null(input$showPop))
      return()
    isolate({
      remove=as.numeric(input$showPop$remove)
      map <- leafletProxy("map")
      
      if(remove==0){
        
        
        
        lat <- as.numeric(input$showPop$lat)
        lng <- as.numeric(input$showPop$lng)
        showPopupHover(lat, lng,housingFilter())   
      }
      else{
        map %>% clearPopups()
      }
      
      
    })
  })
  
  
  
  
  
  #############Search##############
  # geocodeAdddress <- function(address) {
  #        require(RJSONIO)
  #        url <- "http://maps.google.com/maps/api/geocode/json?address="
  #        url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  #        x <- fromJSON(url, simplify = FALSE)
  #       if (x$status == "OK") {
  #            out <- c(x$results[[1]]$geometry$location$lng,
  #                                      x$results[[1]]$geometry$location$lat)
  #          } else {
  #              out <- NA
  #            }
  #        Sys.sleep(0.2)  # API only allows 5 requests per second
  #        out
  #      }
  # #geocodeAdddress("time square")
  # # observe({
  # #   proxy<-leafletProxy("map")
  # #   output$test=renderText(input$place)
  # #   proxy%>% setView(lng=geocodeAdddress(x)[1], lat=geocodeAdddress(x)[2],zoom=15)
  # # 
  # # })
  #############Search###############
  observeEvent(input$button1,{
    url = paste0('http://maps.google.com/maps/api/geocode/xml?address=',input$location,'&sensor=false')
    doc = xmlTreeParse(url) 
    root = xmlRoot(doc) 
    lati = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lat']])) 
    long = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lng']]))
    
    leafletProxy("map") %>%
      setView(lng=long, lat=lati,zoom=15)%>%
      addMarkers(lng=long,lat=lati,layerId = "1",icon=icons(
        iconUrl = "../output/icons8-Location-50.png",iconWidth = 25, iconHeight = 25))
  })
  #################Clear Choices############
  observeEvent(input$button2,{
    proxy<-leafletProxy("map")
    proxy %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      removeMarker(layerId="1") %>%
      addMarkers(data=housing,
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster")
    updateTextInput(session, inputId="location", value = "")
  }
  
  )
  ##################ALL FILTERS############
  # observeEvent(input$filters,{
  #   p<-input$filters
  #   proxy<-leafletProxy("map")
  #   groups<-c("Subway","Bus","Restaurant","Market","crime")
  # 
  #   if(p==TRUE){
  #     proxy %>%
  #       addMarkers(data=sub.station, ~lng, ~lat,label = ~info,icon=icons(
  #         iconUrl = "../output/metro.png",
  #         iconWidth = 7, iconHeight = 7),group="Subway")%>%
  #       addMarkers(data=bus.stop, ~lng, ~lat,label = ~info,icon=icons(
  #         iconUrl = "../output/icons8-Bus-48.png",
  #         iconWidth = 7, iconHeight = 7),group="Bus")%>%
  #       addMarkers(lat=restaurant$lat, lng=restaurant$lon,label=restaurant$DBA,icon=icons(
  #         iconUrl = "../output/icons8-French Fries-96.png",
  #         iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),group="Restaurant") %>%
  #       addMarkers(lat=markets$latitude, lng=markets$longitude,label=markets$DBA.Name, icon=icons(
  #         iconUrl = "../output/icons8-Shopping Cart-48.png",
  #         iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),group="Market")%>%
  #       addPolygons(data=nyc, fillColor = ~pal(count), color = 'grey', weight = 1,
  #                   fillOpacity = .6,group="crime") %>%
  #       addLayersControl(overlayGroups = groups, options = layersControlOptions(collapsed = FALSE))%>%
  #       hideGroup("Subway")%>%
  #       hideGroup("Bus")%>%
  #       hideGroup("Crime")%>%
  #       hideGroup("Restaurant")
  # 
  #   }
  # 
  # })
  
  #############Clear button###########
  observeEvent(input$clear, {
    leafletProxy('map')%>% setView(lng = -73.971035, lat = 40.775659, zoom = 12)
    
  })
 
  
  ############Subway##############
  observeEvent(input$Subway,{
    p<-input$Subway
    proxy<-leafletProxy("map")
    
    if(p==TRUE){
      proxy %>% 
        addMarkers(data=sub.station, ~lng, ~lat,label = ~info,icon=icons(
          iconUrl = "../output/icons8-Bus-48.png",
          iconWidth = 7, iconHeight = 7),group="subway")
    }
    else proxy%>%clearGroup(group="subway")
    
  })
  
  ###############bus###############
  observeEvent(input$Bus,{
    p<-input$Bus
    proxy<-leafletProxy("map")
    
    if(p==TRUE){
      proxy %>% 
        addMarkers(data=bus.stop, ~lng, ~lat,label = ~info,icon=icons(
          iconUrl = "../output/icons8-Bus-48.png",
          iconWidth = 7, iconHeight = 7),layerId=as.character(bus.stop$info))
    }
    else proxy%>%removeMarker(layerId=as.character(bus.stop$info))
    
  })
  
  
  ##############Market#####################
  observeEvent(input$Market,{
    p<- input$Market
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy%>%
        addMarkers(lat=markets$latitude, lng=markets$longitude,icon=icons(
          iconUrl = "../output/icons8-Shopping Cart-48.png",
          iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),layerId=as.character(markets$License.Number))
    }
    else{
      proxy %>%
        removeMarker(layerId=as.character(markets$License.Number))
    }
  })
  
  ##############Resturant#####################
  observeEvent(input$Restaurant,{
    p<- input$Restaurant
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy%>%
        addMarkers(lat=restaurant$lat, lng=restaurant$lon,icon=icons(
          iconUrl = "../output/icons8-French Fries-96.png",
          iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),layerId=as.character(restaurant$CAMIS))
    }
    else{
      proxy %>%
        removeMarker(layerId=as.character(restaurant$CAMIS))
    }
  })
  ##############Crime#####################
  observeEvent(input$Crime,{
    p<- input$Crime
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy %>%
        addPolygons(data=nyc, fillColor = ~pal(count), color = 'grey', weight = 1,
                    fillOpacity = .6)%>%
        addLegend(pal = pal, values = nyc$count,position="bottomleft")
    }
    else proxy%>%clearShapes()%>%clearControls()
    
  })
  # observeEvent("Crime"%in% input$filters,{
  #   proxy<-leafletProxy("map")
  #   
  #   if("Crime"%in% input$filters){
  #     proxy %>%
  #       addPolygons(data=nyc, fillColor = ~pal(count), color = 'grey', weight = 1,
  #                   fillOpacity = .6)%>%
  #       addLegend(pal = pal, values = nyc$count)
  #   }
  #   else proxy%>%clearShapes()
  #   
  # })
  
  
  #######for statistics####
  
  
  #draw table 
  output$ranktable<-renderDataTable({
    select1<-as.character(input$First)
    select2<-as.character(input$Second)
    select3<-as.character(input$Third)
    school<-as.character(input$university)
    
    rank<-rank_all[,1:5]
    if(school == "columbia"){
      rank$Distance<-rank_all$Travel_Columbia
    }
    if(school == "nyu"){
      rank$Distance<-rank_all$Travel_NYU
    }
    if(school == "fordham"){
      rank$Distance<-rank_all$Travel_Fordham
    }
    
    #rank calculation 
    rank<-as.data.frame(rank)
    rank$score<- 0.5*rank[[select1]]+0.3*rank[[select2]]+0.2*rank[[select3]]
    order.score<-order(rank$score)
    rank$TotalRank[order.score] <- 1:nrow(rank)
    
    #sort table 
    rank<-rank[order(rank$TotalRank),]
    
    rank<-rank %>%
      dplyr::select(neighbourhood,TotalRank,select1,select2,select3,everything()) %>%
      dplyr::select(-score)
    
    rownames(rank)<-rank$neighbourhood
    rank$neighbourhood<-NULL
    
    rank
  })
  
  ##########rent plot##############
  output$rentTrend <- renderPlot({
    
    rent<-rent%>%
      select(-Average.Price)
    region_rent=gather(rent,RegionName)
    colnames(region_rent)<-c("regionname","time","rent")
    region_rent$time<-gsub("X", "", as.character(factor(region_rent$time)) )
    head(region_rent)
    region_rent=region_rent%>%
      arrange(time)%>%
      group_by(regionname)
    
    region.selected <- reactive({region_rent[, input$regionname, drop = FALSE]})
    region.names <- reactive( {unique(input$regionname)})
    
    
    
    xrange <- range(region_rent$time)
    yrange <- c(0,5000)
    plot(xrange, yrange, type="n", xlab="Year-Month",ylab="Rent Price")
    for(i in 1:length(region.names())){
      d <- subset( region_rent, region_rent$regionname == region.names()[i] )
      lines(d$time, d$rent, col = i)
    }
  })
  
  output$rentTrendgg <- renderPlot({
    target <- as.character(input$regionname)
    
    new<-filter(region_rent, regionname %in% target)
    ggplot(data=new,
           aes(x=year, y=rent, colour=regionname)) +
      geom_line()+
      scale_x_continuous(breaks = round(seq(min(new$year), max(new$year), by = 1),1)) +
      ylim(0,5000)
  })
  
  
  
  
  
})#shiney server
