library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(raster)
library(htmltools)
library(htmlwidgets)


#### Helpful links
# https://jsfiddle.net/x8r0pvg1/22/
# https://github.com/digidem/leaflet-side-by-side/issues/4
# https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
# https://stackoverflow.com/questions/49022056/dynamically-select-layers-for-leaflet-sidebyside-control


## Input and format data
r<-stack(list.files("Data/Rasters", full.names = T)); projection(r)<-CRS("+proj=longlat +datum=WGS84")
r<-dropLayer(r, 5)

poly<-
  read_sf("Data/NWMR/NWShelf.shp") %>%
  st_transform(crs = 4326) %>%
  as_Spatial()

all<-read.csv("Data/SS_occurences_2018-02-28.csv")
coordinates(all)<-c("Longitude", "Latitude"); projection(all)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

data<- all[poly,]
data<-data[!data$species%in%"laevis",]
data$spp<-droplevels(data$spp)

validation<-
  read.csv("Data/Field Validation data/KimberlyPilbaraTrawls.csv") %>%
  mutate(valid = 
           case_when(
             animal %in% "fuscus" ~ "firebrick",
             animal %in% "snake" ~ "grey",
             animal %in% "none" ~ "grey"
           )) %>%
  arrange(desc(animal)) %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326) %>%
  as_Spatial()

p1 <- colorNumeric(c("steelblue2","gold", "red"), values(r[[1]]),na.color = "transparent")


##################### 

ui <- 
  bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%")
  )    

server = function(input, output) {
  
  ## Register sidebyside plugin
  LeafletSideBySidePlugin <- htmlDependency("leaflet-side-by-side","2.0.0",
                                            src = normalizePath("Plugins/leaflet-side-by-side/"),
                                            script="leaflet-side-by-side.js")
  
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  output$map <- renderLeaflet({
    leaflet() %>%
      registerPlugin(LeafletSideBySidePlugin) %>%
      onRender(
        "function(el, x) {
        map.createPane('left');
        map.createPane('right');

        var imageBounds = [[111.5417, -26.74167], [130.35, -9.683333]];

        var base = L.tileLayer(
        'https://server.arcgisonline.com/arcgis/rest/services/World_Imagery/MapServer//tile/{z}/{y}/{x}',{
        maxZoom: 14
        }).addTo(this);

        var leftpane = L.imageOverlay('test.tif', imageBounds, {opacity:0.7, pane: 'left'}).addTo(this);
        var rightpane = L.imageOverlay('test2.tif', imageBounds, {opacity:0.7, pane: 'right'}).addTo(this);

        var mapLayer1 = L.layerGroup([base, leftpane]);
        var mapLayer2 = L.layerGroup([base, rightpane]).addTo(this);


        L.control.sideBySide(mapLayer1, mapLayer2).addTo(this);
        } ") %>%
      addCircleMarkers(
        lat = coordinates(validation)[,2],
        lng = coordinates(validation)[,1],
        radius = 2,
        weight = 2,
        opacity = 1,
        color = validation$valid,
        group = "Field Validation",
        fillOpacity = 1,
        popup = paste(sep = "", "<b><i>", validation$Species , "</i></b> <br/>")
      ) %>%
      
      addRasterImage(r[[1]],
                     colors = p1,
                     group = "<i>Aipysurus apraefrontalis</i>",
                     opacity = 0.7) %>%
      addRasterImage(r[[2]],
                     colors = p1,
                     group = "<i>Aipysurus foliosquama</i>",
                     opacity = 0.7) %>%
      addRasterImage(r[[3]],
                     colors = p1,
                     group = "<i>Aipysurus fuscus</i>",
                     opacity = 0.7) %>%
      addRasterImage(r[[4]],
                     colors = p1,
                     group = "<i>Aipysurus tenuis</i>",
                     opacity = 0.7) %>%
      addRasterImage(r[[5]],
                     colors = p1,
                     group = "<i>Aipysurus laevis pooleorum</i>",
                     opacity = 0.7) %>%
      
      addLegend(
        "bottomright",
        pal = p1,
        values = values(r[[1]]),
        title = "Habitat Suitability",
        opacity = 1
      ) %>%
      
      addLayersControl(
        baseGroups = paste("<i>", levels(data$spp), "</i>", sep = ""),
        overlayGroups = c("Field Validation"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        position = "bottomleft"
      ) %>%
      
      addMeasure("topleft",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters")
    

  })
}

shinyApp(ui, server)

