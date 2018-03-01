library(tidyverse)
library(leaflet)
library(maptools)
library(raster)
library(rgeos)
library(htmlwidgets)

r<-stack(list.files("Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")
poly<-readShapeSpatial("Data/NWMR/NWShelf.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

all<-read.csv("Data/SS_occurences_2018-02-28.csv")
coordinates(all)<-c("Longitude", "Latitude"); projection(all)<-CRS("+proj=longlat +datum=WGS84")

#data<-gIntersection(all, poly)
data<- all[poly,]

ap<-data[data$species%in%"apraefrontalis",]
fo<-data[data$species%in%"foliosquama",]
fu<-data[data$species%in%"fuscus",]
po<-data[data$species%in%"pooleorum",]
te<-data[data$species%in%"tenuis",]

p0 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[6]]),na.color = "transparent")
p1 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[1]]),na.color = "transparent")
p2 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[2]]),na.color = "transparent")
p3 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[3]]),na.color = "transparent")
p4 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[4]]),na.color = "transparent")
p5 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[5]]),na.color = "transparent")

rad=3
op=0.7
wt= 10
  
maxentmap<-
  
  leaflet() %>% addTiles() %>%
  #addProviderTiles(providers$Esri.WorldStreetMap, group = "Map") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  #addRasterImage(r[[6]], colors = p0, group="All sea snakes", opacity = 0.7) %>%
  #addCircleMarkers(lat=all$lat, lng=all$lon, radius= rad, weight=wt, opacity=op, color=1, group="All sea snakes", fillOpacity=op, popup=paste("species =",all$genus,all$species," source =",all$lab)) %>%
  
  addRasterImage(r[[1]], colors = p1, group="A. apraefrontalis", opacity = 0.7) %>%
  addCircleMarkers(lat=ap$Latitude, lng=ap$Longitude, radius= rad, weight=wt, opacity=op, color=1, group="A. apraefrontalis", fillOpacity=op, 
                   popup=paste(sep="", 
                               "<b><i>Aipysurus apraefrontalis</i></b> <br/>",
                               "Source: ", ap$Source, "<br/>",
                               "ID: ", ap$ID, "<br/>",
                               "Record Type: ", ap$Record.Type, "<br/>",
                               "Collector: ", ap$Collector, "<br/>",
                               "Date collected: ", ap$Date.collected, "<br/>",
                               "Sex: ", ap$Sex, "<br/>")) %>%
  addRasterImage(r[[2]], colors = p2, group="A. foliosquama", opacity = 0.7) %>%
  addCircleMarkers(lat=fo$Latitude, lng=fo$Longitude, radius= rad, weight=wt, opacity=op, color=1, group="A. foliosquama", fillOpacity = op, 
                   popup=paste(sep="", 
                               "<b><i>Aipysurus foliosquama</i></b> <br/>",
                               "Source: ", fo$Source, "<br/>",
                               "ID: ", fo$ID, "<br/>",
                               "Record Type: ", fo$Record.Type, "<br/>",
                               "Collector: ", fo$Collector, "<br/>",
                               "Date collected: ", fo$Date.collected, "<br/>",
                               "Sex: ", fo$Sex, "<br/>")) %>%
  addRasterImage(r[[3]], colors = p3, group="A. fuscus",  opacity = 0.7) %>%
  addCircleMarkers(lat=fu$Latitude, lng=fu$Longitude, radius= rad, weight=wt, opacity=op, color=1, group="A. fuscus", fillOpacity = op, 
                   popup=paste(sep="", 
                               "<b><i>Aipysurus fuscus</i></b> <br/>",
                               "Source: ", fu$Source, "<br/>",
                               "ID: ", fu$ID, "<br/>",
                               "Record Type: ", fu$Record.Type, "<br/>",
                               "Collector: ", fu$Collector, "<br/>",
                               "Date collected: ", fu$Date.collected, "<br/>",
                               "Sex: ", fu$Sex, "<br/>")) %>%
  addRasterImage(r[[4]], colors = p4, group="A. l. pooleorum", opacity = 0.7) %>%
  addCircleMarkers(lat=po$Latitude, lng=po$Longitude, radius= rad, weight=wt, opacity=op, color=1, group="A. l. pooleorum", fillOpacity = op, 
                   popup=paste(sep="", 
                               "<b><i>Aipysurus laevis pooleorum</i></b> <br/>",
                               "Source: ", po$Source, "<br/>",
                               "ID: ", po$ID, "<br/>",
                               "Record Type: ", po$Record.Type, "<br/>",
                               "Collector: ", po$Collector, "<br/>",
                               "Date collected: ", po$Date.collected, "<br/>",
                               "Sex: ", po$Sex, "<br/>")) %>%
  addRasterImage(r[[5]], colors = p5, group="A. tenuis", opacity = 0.7) %>%
  addCircleMarkers(lat=te$Latitude, lng=te$Longitude, radius= rad, weight=wt, opacity=op, color=1, group="A. tenuis", fillOpacity = op, 
                   popup=paste(sep="", 
                               "<b><i>Aipysurus tenuis</i></b> <br/>",
                               "Source: ", te$Source, "<br/>",
                               "ID: ", te$ID, "<br/>",
                               "Record Type: ", te$Record.Type, "<br/>",
                               "Collector: ", te$Collector, "<br/>",
                               "Date collected: ", te$Date.collected, "<br/>",
                               "Sex: ", te$Sex, "<br/>")) %>%
  
  addLayersControl(
    #baseGroups = c("Map","Satellite"),
    baseGroups = c("A. apraefrontalis", "A. foliosquama", "A. fuscus","A. l. pooleorum","A. tenuis"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
            position = "topleft") %>%
  
  addLegend("bottomright", pal = p1, values = values(r[[1]]),
            title = "Habitat Suitability", opacity=1) %>%
  
  
  hideGroup(c("A. foliosquama", "A. fuscus","A. l. pooleorum","A. tenuis"))


saveWidget(maxentmap, file="maxentmap.html")

