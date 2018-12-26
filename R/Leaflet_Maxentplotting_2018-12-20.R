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

validation<-
  read.csv("Data/Field Validation data/KimberlyPilbaraTrawls.csv") %>%
  mutate(valid = 
           case_when(
             animal %in% "fuscus" ~ "firebrick",
             animal %in% "snake" ~ "grey",
             animal %in% "none" ~ "grey"
             )) %>%
  arrange(desc(animal))
coordinates(validation)<-c("Longitude", "Latitude"); projection(validation)<-CRS("+proj=longlat +datum=WGS84")

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


maxentmap<-
  
  leaflet() %>% addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  addRasterImage(r[[1]], colors = p1, group="<i>Aipysurus apraefrontalis</i>", opacity = 0.7) %>%
  addRasterImage(r[[2]], colors = p1, group="<i>Aipysurus foliosquama</i>", opacity = 0.7) %>%
  addRasterImage(r[[3]], colors = p1, group="<i>Aipysurus fuscus</i>", opacity = 0.7) %>%
  addRasterImage(r[[4]], colors = p1, group="<i>Aipysurus tenuis</i>", opacity = 0.7) %>%
  # addRasterImage(r[[5]], colors = p1, group="Aipysurus laevis laevis", opacity = 0.7) %>%
  addRasterImage(r[[5]], colors = p1, group="<i>Aipysurus laevis pooleorum</i>", opacity = 0.7) %>%
  
  addCircleMarkers(lat=data$Latitude, lng=data$Longitude, radius= 2, weight=2, opacity=1, color=1, group=paste("<i>",data$spp,"</i>", sep=""), fillOpacity = 1, 
                   popup=paste(sep="", 
                               "<b><i>", data$spp ,"</i></b> <br/>",
                               "Source: ", data$Source, "<br/>",
                               "ID: ", data$ID, "<br/>",
                               "Record Type: ", data$Record.Type, "<br/>",
                               "Collector: ", data$Collector, "<br/>",
                               "Date collected: ", data$Date.collected, "<br/>",
                               "Sex: ", data$Sex, "<br/>")) %>%
  addPolygons(data=mpa_mp, color="#99CC00", weight=2, group="Marine Protected Areas", 
              popup=paste(sep="",
                          "<b>", mpa_mp$NAME, "</b> <br/>",
                          "<br/>", mpa_mp$TYPE,"<br/>", mpa_mp$COMMENTS,"<br/>"))%>%
  addPolygons(data=mpa_mu, color="#FFBA00", weight=2, group="Marine Protected Areas",
              popup=paste(sep="",
                          "<b>", mpa_mu$NAME, "</b> <br/>",
                          "<br/>", mpa_mu$TYPE,"<br/>", mpa_mu$COMMENTS,"<br/>"))%>%
  
  addCircleMarkers(lat=validation$Latitude, lng=validation$Longitude, radius= 2, weight=2, opacity=1, color=validation$valid, group="Field Validation", 
                   fillOpacity = 1, popup=paste(sep="", "<b><i>", validation$Species ,"</i></b> <br/>")) %>%
  
  addLayersControl(
    baseGroups = paste("<i>",levels(data$spp),"</i>", sep=""),
    overlayGroups = c("Marine Protected Areas", "Field Validation"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
             position = "bottomleft") %>%
  
  addMeasure("topleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters") %>%
  
  addLegend("bottomright", pal = p1, values = values(r[[1]]),
            title = "Habitat Suitability", opacity=1) %>%
  
  hideGroup(c(paste("<i>",levels(data$spp),"</i>", sep="")[-1], "Marine Protected Areas", "Field Validation"))



maxentmap

saveWidget(maxentmap, file="ModelMap.html")

