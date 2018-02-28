
library(leaflet)
library(maptools)
library(raster)
library(rgeos)
library(htmlwidgets)

r<-stack(list.files("~/Documents/GitHub/SeaSnake-NicheModels/Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")
poly<-readShapeSpatial("~/Dropbox/Manuscripts/15. WA BRUVS/Data/WA rasters/NWMR/NWShelf.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

data<-read.csv("~/Dropbox/Manuscripts/18. Ru SS Modelling/Data/Occurence/SS_occ_master2.csv")
data<-rbind(data, data.frame(source=c("ala"), lat=c(-18.01), lon=c(122.16), date=c("1988-04-01"), count=c(1), species=c("tenuis"), genus=c("Aipysurus")))
data<-data[!data$source%in%"Ru_dataset",]
data[data$source%in%"ala","lab"]<-"Atlas of Living Australia"
data[data$source%in%c("QLDbruvs","WAbruvs"),"lab"]<-"BRUVs (AIMS)"
data[data$source%in%"rls","lab"]<-"Reef Life Survey"
data[data$source%in%c("Survey_Blanche"),"lab"]<-"Snorkel Survey (D'Anastasi,B)"
data[data$source%in%c("Survey_Char","Survey_Kate"),"lab"]<-"Snorkel/Spotlight Survey (Sanders,K)"
data[data$source%in%c("WAFish_14-16","WAFish_Char","WAFish_Nat07","WAFish_Nat17","WAFish_Nat17b"),"lab"]<-"Research Trawl Surveys (DoF,WA)"

coordinates(data)<-~lon+lat; projection(data)<-CRS("+proj=longlat +datum=WGS84")

all<-gIntersection(data, poly)
ap<-data[data$species%in%"apraefrontalis",]; #apr<-rasterToPoints(rasterize(ap, r, field=1), spatial=T); apr@data[,"species"]<-"apr"
fo<-data[data$species%in%"foliosquama",]; #fol<-rasterToPoints(rasterize(fo, r, field=1), spatial=T); fol@data[,"species"]<-"fol"
fu<-data[data$species%in%"fuscus",]; #fus<-rasterToPoints(rasterize(fu, r, field=1), spatial=T); fus@data[,"species"]<-"fus"
po<-data[data$species%in%"pooleorum",]; #poo<-rasterToPoints(rasterize(po, r, field=1), spatial=T); poo@data[,"species"]<-"poo"
te<-data[data$species%in%"tenuis" & data$source%in%c("ala","Survey_Kate"),]; #ten<-rasterToPoints(rasterize(te, r, field=1), spatial=T); ten@data[,"species"]<-"ten"

p0 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[6]]),na.color = "transparent")
p1 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[1]]),na.color = "transparent")
p2 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[2]]),na.color = "transparent")
p3 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[3]]),na.color = "transparent")
p4 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[4]]),na.color = "transparent")
p5 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[5]]),na.color = "transparent")

rad=2
op=1
wt= 10
  
maxentmap<-
  
  leaflet() %>% addTiles() %>%
  #addProviderTiles(providers$Esri.WorldStreetMap, group = "Map") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  #addRasterImage(r[[6]], colors = p0, group="All sea snakes", opacity = 0.7) %>%
  #addCircleMarkers(lat=all$lat, lng=all$lon, radius= rad, weight=wt, opacity=op, color=1, group="All sea snakes", fillOpacity=op, popup=paste("species =",all$genus,all$species," source =",all$lab)) %>%
  
  addRasterImage(r[[1]], colors = p1, group="A. apraefrontalis", opacity = 0.7) %>%
  addCircleMarkers(lat=ap$lat, lng=ap$lon, radius= rad, weight=wt, opacity=op, color=1, group="A. apraefrontalis", fillOpacity=op, 
                   popup=paste("source =",ap$lab)) %>%
  addRasterImage(r[[2]], colors = p2, group="A. foliosquama", opacity = 0.7) %>%
  addCircleMarkers(lat=fo$lat, lng=fo$lon, radius= rad, weight=wt, opacity=op, color=1, group="A. foliosquama", fillOpacity = op, 
                   popup=paste("source =",fo$lab)) %>%
  addRasterImage(r[[3]], colors = p3, group="A. fuscus",  opacity = 0.7) %>%
  addCircleMarkers(lat=fu$lat, lng=fu$lon, radius= rad, weight=wt, opacity=op, color=1, group="A. fuscus", fillOpacity = op, 
                   popup=paste("source =",fu$lab)) %>%
  addRasterImage(r[[4]], colors = p4, group="A. pooleorum", opacity = 0.7) %>%
  addCircleMarkers(lat=po$lat, lng=po$lon, radius= rad, weight=wt, opacity=op, color=1, group="A. pooleorum", fillOpacity = op, 
                   popup=paste("source =",po$lab)) %>%
  addRasterImage(r[[5]], colors = p5, group="A. tenuis", opacity = 0.7) %>%
  addCircleMarkers(lat=te$lat, lng=te$lon, radius= rad, weight=wt, opacity=op, color=1, group="A. tenuis", fillOpacity = op, 
                   popup=paste("source =",te$lab)) %>%
  
  addLayersControl(
    #baseGroups = c("Map","Satellite"),
    baseGroups = c("A. apraefrontalis", "A. foliosquama", "A. fuscus","A. pooleorum","A. tenuis"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
            position = "topleft") %>%
  
  addLegend("bottomright", pal = p1, values = values(r[[1]]),
            title = "Habitat Suitability", opacity=1) %>%
  
  hideGroup(c("A. foliosquama", "A. fuscus","A. pooleorum","A. tenuis"))


saveWidget(maxentmap, file="maxentmap.html")

