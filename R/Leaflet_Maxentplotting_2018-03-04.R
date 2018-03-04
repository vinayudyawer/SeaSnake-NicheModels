library(tidyverse)
library(leaflet)
library(maptools)
library(raster)
library(rgeos)
library(htmlwidgets)

r<-stack(list.files("Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")
r<-dropLayer(r, 5)
poly<-readShapeSpatial("Data/NWMR/NWShelf.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))
mpa<-crop(readShapeSpatial("Data/MPAs/capad_marine.shp", proj4string = CRS("+proj=longlat +datum=WGS84")), r)
mpa_mp<-mpa[mpa$IUCN%in%c("IA","II"),]
mpa_mu<-mpa[mpa$IUCN%in%c("III","IV","V","VI"),]

all<-read.csv("Data/SS_occurences_2018-02-28.csv")
coordinates(all)<-c("Longitude", "Latitude"); projection(all)<-CRS("+proj=longlat +datum=WGS84")

#data<-gIntersection(all, poly)
data<- all[poly,]
data<-data[!data$species%in%"laevis",]
data$spp<-droplevels(data$spp)

# ap<-data[data$species%in%"apraefrontalis",]
# fo<-data[data$species%in%"foliosquama",]
# fu<-data[data$species%in%"fuscus",]
# te<-data[data$species%in%"tenuis",]
# # la<-data[data$species%in%"laevis",]
# po<-data[data$species%in%"pooleorum",]

p1 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[1]]),na.color = "transparent")
p2 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[2]]),na.color = "transparent")
p3 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[3]]),na.color = "transparent")
p4 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[4]]),na.color = "transparent")
p5 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[5]]),na.color = "transparent")
# p6 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[6]]),na.color = "transparent")

rad=3
op=0.7
wt= 10

maxentmap<-
  
  leaflet() %>% addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  addRasterImage(r[[1]], colors = p1, group="<i>Aipysurus apraefrontalis</i>", opacity = 0.7) %>%
  addRasterImage(r[[2]], colors = p1, group="<i>Aipysurus foliosquama</i>", opacity = 0.7) %>%
  addRasterImage(r[[3]], colors = p1, group="<i>Aipysurus fuscus</i>", opacity = 0.7) %>%
  addRasterImage(r[[4]], colors = p1, group="<i>Aipysurus tenuis</i>", opacity = 0.7) %>%
  # addRasterImage(r[[5]], colors = p1, group="Aipysurus laevis laevis", opacity = 0.7) %>%
  addRasterImage(r[[5]], colors = p1, group="<i>Aipysurus laevis pooleorum</i>", opacity = 0.7) %>%
  
  addCircleMarkers(lat=data$Latitude, lng=data$Longitude, radius= rad, weight=wt, opacity=op, color=1, group=paste("<i>",data$spp,"</i>", sep=""), fillOpacity = op, 
                   popup=paste(sep="", 
                               "<b><i>", data$spp ,"</i></b> <br/>",
                               "Source: ", data$Source, "<br/>",
                               "ID: ", data$ID, "<br/>",
                               "Record Type: ", data$Record.Type, "<br/>",
                               "Collector: ", data$Collector, "<br/>",
                               "Date collected: ", data$Date.collected, "<br/>",
                               "Sex: ", data$Sex, "<br/>")) %>%
  addPolygons(data=mpa_mp, color="#99CC00", weight=2, group="Marine Protected Areas") %>%
  addPolygons(data=mpa_mu, color="#FFBA00", weight=2, group="Marine Protected Areas") %>%
  
  addLayersControl(
    baseGroups = paste("<i>",levels(data$spp),"</i>", sep=""),
    overlayGroups = "Marine Protected Areas",
    options = layersControlOptions(collapsed = FALSE)) %>%
  
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
             position = "topleft") %>%
  
  addLegend("bottomright", pal = p1, values = values(r[[1]]),
            title = "Habitat Suitability", opacity=1) %>%
  
  hideGroup(c(paste("<i>",levels(data$spp),"</i>", sep="")[-1], "Marine Protected Areas"))



maxentmap

saveWidget(maxentmap, file="ModelMap.html")

