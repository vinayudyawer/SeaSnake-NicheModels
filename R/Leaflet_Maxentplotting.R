
library(leaflet)
library(raster)
library(htmlwidgets)

r<-stack(list.files("~/Documents/GitHub/SeaSnake-NicheModels/Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")

p1 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[1]]),na.color = "transparent")
p2 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[2]]),na.color = "transparent")
p3 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[3]]),na.color = "transparent")
p4 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[4]]),na.color = "transparent")
p5 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[5]]),na.color = "transparent")

maxentmap<-
  
  leaflet() %>% addTiles() %>%
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Map") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  
  addRasterImage(r[[1]], colors = p1, group="A. apraefrontalis", opacity = 0.7) %>%
  addRasterImage(r[[2]], colors = p2, group="A. foliosquama", opacity = 0.7) %>%
  addRasterImage(r[[3]], colors = p3, group="A. fuscus",  opacity = 0.7) %>%
  addRasterImage(r[[4]], colors = p4, group="A. pooleorum", opacity = 0.7) %>%
  addRasterImage(r[[5]], colors = p5, group="A. tenuis", opacity = 0.7) %>%

  addLayersControl(
    baseGroups = c("Map","Satellite"),
    overlayGroups = c("A. apraefrontalis", "A. foliosquama", "A. fuscus","A. pooleorum","A. tenuis"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  
  # addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
  #            position = "topleft") %>%
  
  addLegend("bottomright", pal = p1, values = values(r[[1]]),
            title = "Habitat Suitability", opacity=1) %>%
  
  hideGroup(c("A. foliosquama", "A. fuscus","A. pooleorum","A. tenuis"))


saveWidget(maxentmap, file="maxentmap.html")

