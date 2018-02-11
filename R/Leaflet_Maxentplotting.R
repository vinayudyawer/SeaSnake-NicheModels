
library(leaflet)
library(raster)
library(htmlwidgets)

r<-stack(list.files("~/Documents/GitHub/SeaSnake-NicheModels/Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")

data<-read.csv("~/Dropbox/Manuscripts/18. Ru SS Modelling/Data/Occurence/SS_occ_master2.csv")
data<-rbind(data, data.frame(source=c("ala"), lat=c(-18.01), lon=c(122.16), date=c("1988-04-01"), count=c(1), species=c("tenuis"), genus=c("Aipysurus")))
coordinates(data)<-~lon+lat; projection(data)<-CRS("+proj=longlat +datum=WGS84")

ap<-data[data$species%in%"apraefrontalis",]; apr<-rasterToPoints(rasterize(ap, r, field=1), spatial=T); apr@data[,"species"]<-"apr"
fo<-data[data$species%in%"foliosquama",]; fol<-rasterToPoints(rasterize(fo, r, field=1), spatial=T); fol@data[,"species"]<-"fol"
fu<-data[data$species%in%"fuscus",]; fus<-rasterToPoints(rasterize(fu, r, field=1), spatial=T); fus@data[,"species"]<-"fus"
po<-data[data$species%in%"pooleorum",]; poo<-rasterToPoints(rasterize(po, r, field=1), spatial=T); poo@data[,"species"]<-"poo"
te<-data[data$species%in%"tenuis" & data$source%in%c("ala","Survey_Kate"),]; ten<-rasterToPoints(rasterize(te, r, field=1), spatial=T); ten@data[,"species"]<-"ten"

p1 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[1]]),na.color = "transparent")
p2 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[2]]),na.color = "transparent")
p3 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[3]]),na.color = "transparent")
p4 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[4]]),na.color = "transparent")
p5 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[5]]),na.color = "transparent")

maxentmap<-
  
  leaflet() %>% addTiles() %>%
  #addProviderTiles(providers$Esri.WorldStreetMap, group = "Map") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  addRasterImage(r[[1]], colors = p1, group="A. apraefrontalis", opacity = 0.7) %>%
  addCircles(lat=ap$lat, lng=ap$lon, opacity=0.5, color=1, group="A. apraefrontalis") %>%
  addRasterImage(r[[2]], colors = p2, group="A. foliosquama", opacity = 0.7) %>%
  addCircles(lat=fo$lat, lng=fo$lon, opacity=0.5, color=1, group="A. foliosquama") %>%
  addRasterImage(r[[3]], colors = p3, group="A. fuscus",  opacity = 0.7) %>%
  addCircles(lat=fu$lat, lng=fu$lon, opacity=0.5, color=1, group="A. fuscus") %>%
  addRasterImage(r[[4]], colors = p4, group="A. pooleorum", opacity = 0.7) %>%
  addCircles(lat=po$lat, lng=po$lon, opacity=0.5, color=1, group="A. pooleorum") %>%
  addRasterImage(r[[5]], colors = p5, group="A. tenuis", opacity = 0.7) %>%
  addCircles(lat=te$lat, lng=te$lon, opacity=0.5, color=1, group="A. tenuis") %>%
  
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

