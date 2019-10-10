library(leaflet.extras)
library(sp)
#library(rgdal)
library(sf)

server <- function(input, output) {
  
  
  output$mymap <- renderLeaflet(
    if (input$city == 2) {
      leaflet() %>%
        addTiles() %>%
        # setView(-74.00, 40.71, zoom = 12) %>%
        # https://rpubs.com/jhofman/nycmaps
        setView(-122.2, 37.6, zoom = 10) %>%
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>%
        addLayersControl(
          overlayGroups = c("draw"), options =
            layersControlOptions(collapsed = FALSE)
        )
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(-74.00, 40.71, zoom = 12) %>%
        # setView(-122.2, 37.6, zoom = 10) %>%
        # https://rpubs.com/infomind/245869
        addDrawToolbar(
          targetGroup = "draw",
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
        ) %>%
        addLayersControl(
          overlayGroups = c("draw"), options =
            layersControlOptions(collapsed = FALSE)
        )
    }
  )
  
  
  
  latlongs <- reactiveValues() # temporary to hold coords
  latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  
  #########
  # empty reactive spdf
  # https://stackoverflow.com/questions/44979900/how-to-download-polygons-drawn-in-leaflet-draw-as-geojson-file-from-r-shiny
  value <- reactiveValues()
  SpatialPolygonsDataFrame(SpatialPolygons(list()), data = data.frame(notes = character(0), stringsAsFactors = F)) -> value$drawnPoly
  
  # fix the polygon to start another
  
  observeEvent(input$mymap_draw_new_feature, {
    coor <- unlist(input$mymap_draw_new_feature$geometry$coordinates)
    
    Longitude <- coor[seq(1, length(coor), 2)]
    
    Latitude <- coor[seq(2, length(coor), 2)]
    
    isolate(latlongs$df2 <- rbind(latlongs$df2, cbind(Longitude, Latitude)))
    
    poly <- Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
    polys <- Polygons(list(poly), ID = input$mymap_draw_new_feature$properties$`_leaflet_id`)
    spPolys <- SpatialPolygons(list(polys))
    
    
    #
    value$drawnPoly <- rbind(value$drawnPoly, SpatialPolygonsDataFrame(spPolys,
                                                                       data = data.frame(
                                                                         notes = NA, row.names =
                                                                           row.names(spPolys)
                                                                       )
    ))
    
    latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))
  })
  ##
  # observeEvent(input$mymap_draw_new_feature,{
  #   feature <- input$mymap_draw_new_feature
  #   store = c(store, feature)
  #   #print(store)
  #   print(length(store))
  #
  # })
  #
  # stored <- reactive({
  #   store
  # })
  #
  #
  #
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("mydata", ".rds", sep = "")
  #   },
  #   content = function(file) {
  #     saveRDS(stored(), file)
  #   }
  # )
  
  
  output$downloadData <- downloadHandler(
    filename = "shpExport.zip",
    content = function(file) {
      if (length(Sys.glob("shpExport.*")) > 0) {
        file.remove(Sys.glob("shpExport.*"))
      }
      
      proj4string(value$drawnPoly) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      st_write(st_as_sf(value$drawnPoly), dsn = "shpExport.shp", layer = "shpExport", driver = "ESRI Shapefile")
      zip(zipfile = "shpExport.zip", files = Sys.glob("shpExport.*"))
      file.copy("shpExport.zip", file)
      if (length(Sys.glob("shpExport.*")) > 0) {
        file.remove(Sys.glob("shpExport.*"))
      }
    }
  )
}