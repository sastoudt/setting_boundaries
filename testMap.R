#https://stackoverflow.com/questions/48858581/get-coordinates-from-a-drawing-object-from-an-r-leaflet-map
library(leaflet.extras)
library(rlist)
# Define UI 
ui <- fluidPage(
  radioButtons("city", h4("Choose your region"),
               choices = list("NYC" = 1, "Bay Area" = 2),selected = 1),
  h4("Draw shapes of where you are willing to go. Note: the maps are zoomable. Click `Download` when finished."),
  leafletOutput("mymap",height=800), 
  downloadButton("downloadData", "Download")
  
)

# Define server logic 
server <- function(input, output) {
  
  store = vector("list",0)

  
  output$mymap <- renderLeaflet(
    
    if(input$city==2){
      leaflet() %>%
        addTiles() %>%
        #setView(-74.00, 40.71, zoom = 12) %>%
        setView(-122.2, 37.6, zoom = 10) %>%
        addDrawToolbar(
          targetGroup='draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
        addLayersControl(overlayGroups = c('draw'), options =
                           layersControlOptions(collapsed=FALSE))
      
    }else{
      leaflet() %>%
        addTiles() %>%
        setView(-74.00, 40.71, zoom = 12) %>%
        #setView(-122.2, 37.6, zoom = 10) %>%
        addDrawToolbar(
          targetGroup='draw',
          editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
        addLayersControl(overlayGroups = c('draw'), options =
                           layersControlOptions(collapsed=FALSE))
    }
    
  )
  
  observeEvent(input$mymap_draw_new_feature,{
    feature <- input$mymap_draw_new_feature
    store = list.append(store, feature)
    print(store)

  })
  
 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("mydata", ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(store, file)
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


#http://rpubs.com/bhaskarvk/leaflet-draw

## put on binder
## download shape files or coordinates or whatever

#https://rpubs.com/jhofman/nycmaps