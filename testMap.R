#https://stackoverflow.com/questions/48858581/get-coordinates-from-a-drawing-object-from-an-r-leaflet-map
library(leaflet.extras)

# Define UI 
ui <- fluidPage(
  leafletOutput("mymap",height=800)
)

# Define server logic 
server <- function(input, output) {
  
 
  
  output$mymap <- renderLeaflet(
    leaflet() %>%
      addTiles() %>%
      setView(-74.00, 40.71, zoom = 12) %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
  )
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    
    print(feature)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)


#http://rpubs.com/bhaskarvk/leaflet-draw

## put on binder
## download shape files or coordinates or whatever

#https://rpubs.com/jhofman/nycmaps