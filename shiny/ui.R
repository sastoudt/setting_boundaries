library(shiny)
library(leaflet.extras)
library(sp)
#library(rgdal)
#library(sf)
library(maptools)
library(leaflet)

ui <- fluidPage(
  radioButtons("city", h4("Choose your region"),
               choices = list("NYC" = 1, "Bay Area" = 2), selected = 1
  ),
  h4("Use the buttons on the left hand side of the map to draw shapes of where you **are** willing to go. Avoid using circles for now (they are buggy). Note: the maps are zoomable. Click `Download` when finished."),
  downloadButton("downloadData", "Download"),
  leafletOutput("mymap", height = 800)
)