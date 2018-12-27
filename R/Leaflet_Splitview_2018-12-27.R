library(leaflet)
library(htmltools)
library(htmlwidgets)


r<-stack(list.files("Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")
r<-dropLayer(r, 5)

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



LeafletSideBySidePlugin <- htmlDependency("leaflet-side-by-side","2.0.0",
                                          src = normalizePath("Plugins/leaflet-side-by-side/"),
                                          script="leaflet-side-by-side.js")

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

leaflet() %>%
  
  registerPlugin(LeafletSideBySidePlugin) %>%
  onRender("
           function(el, x) {
           var mylayer1 = L.tileLayer(
           'https://server.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer//tile/{z}/{y}/{x}',{
           maxZoom: 14
           }).addTo(this);
           var mylayer2 = L.tileLayer(
           'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',{
           maxZoom: 14
           }).addTo(this);
           L.control.sideBySide(mylayer1, mylayer2).addTo(this);
           } ") %>%
  addCircleMarkers(lat=validation$Latitude, lng=validation$Longitude, radius= 2, weight=2, opacity=1, color=validation$valid, group="Field Validation", 
                   fillOpacity = 1, popup=paste(sep="", "<b><i>", validation$Species ,"</i></b> <br/>")) %>%
  
  addRasterImage(r[[1]], colors = p1, group="<i>Aipysurus apraefrontalis</i>", opacity = 0.7) %>%
  addRasterImage(r[[2]], colors = p1, group="<i>Aipysurus foliosquama</i>", opacity = 0.7) %>%
  addRasterImage(r[[3]], colors = p1, group="<i>Aipysurus fuscus</i>", opacity = 0.7) %>%
  addRasterImage(r[[4]], colors = p1, group="<i>Aipysurus tenuis</i>", opacity = 0.7) %>%
  addRasterImage(r[[5]], colors = p1, group="<i>Aipysurus laevis pooleorum</i>", opacity = 0.7) %>%
  
  addLegend("bottomright", pal = p1, values = values(r[[1]]),
            title = "Habitat Suitability", opacity=1) %>%
            
  addLayersControl(
    baseGroups = paste("<i>",levels(data$spp),"</i>", sep=""),
    overlayGroups = c("Field Validation"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE,
             position = "bottomleft") %>%
  
  addMeasure("topleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters")
  



