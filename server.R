
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# Author: Paolo Tagliolato (CNR IREA), Alessandro Oggioni (CNR IREA), Iacopo Neri (Polimi)
#

library(shiny)
library(osmdata)
library(sf)
library(sp)
library(leaflet)
library(leaflet.extras)
#library(mapview)


shinyServer(function(input, output, session) {

  # output$feature <- renderUI({
  #   choices_osm_features <- available_features()
  #   selectizeInput("features","available features",choices = choices_osm_features)
  # })
  # 
  
  output$tags <- renderUI({
    #inutile <- input$features
    #choices_osmTags <- list("primaria" = "primary", "secondary" ="secondary") 
    choices_osmTags <- available_tags(input$features)
    selectizeInput("tags", "available tags", choices = choices_osmTags, multiple = TRUE)
  })
  
  output$mapleaflet <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      #addProviderTiles(providers$CartoDB.Positron) %>%
      setView(0,0,1) %>%  addSearchOSM() %>%
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
        singleFeature = TRUE
    )
  })
  
  # m <- mapview()
  # output$mapleaflet <- renderLeaflet({
  #   m@map %>% setView(0,0,1) %>%  addSearchOSM() %>%
  #     addDrawToolbar(
  #       targetGroup = "draw",
  #       polylineOptions = FALSE,
  #       markerOptions = FALSE,
  #       circleOptions = FALSE,
  #       circleMarkerOptions = FALSE,
  #       polygonOptions = FALSE,
  #       rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0, color = 'white', weight = 3)),
  #       singleFeature = TRUE
  #       
  #     )
  # })
     
  
  # intercept new bbx
  observeEvent(input$mapleaflet_draw_new_feature,{
    
      proxy <- leafletProxy("mapleaflet")
      
      shape <- input$mapleaflet_draw_new_feature
      
      polygon_coordinates <- shape$geometry$coordinates
      
      feature_type <- shape$properties$feature_type
      if(feature_type == "rectangle"){
        NW = polygon_coordinates[[1]][[2]]
        SE = polygon_coordinates[[1]][[4]]
        updateTextInput(session,"bbxN",value = paste(NW[[2]]))
        updateTextInput(session,"bbxW",value = paste(NW[[1]]))
        updateTextInput(session,"bbxS",value = paste(SE[[2]]))
        updateTextInput(session,"bbxE",value = paste(SE[[1]]))
      }
  })
  
  RV <- reactiveValues(polys = NULL, lines = NULL, points = NULL)
  currentOSMData <- observeEvent(input$downloadOverpass, ignoreInit = TRUE, {
    
    #req(input$bbxN, input$bbxE, input$bbxS, input$bbxW)
    #input$downloadOverpass
    
    xmin <- input$bbxW
    ymin <- input$bbxS
    xmax <- input$bbxE
    ymax <- input$bbxN
    
    browser()
    bb <- c(xmin, ymin, xmax, ymax)
    x <-
      osmdata::opq(bbox = as.numeric(bb)) %>% # Chiswick Eyot in London, U.K.
      add_osm_feature(key = 'highway', value = "primary") %>%
      add_osm_feature(key = 'highway', value = "secondary") %>%
      add_osm_feature(key = 'highway', value = "tertiary") %>%
      osmdata_sf()
    
    # workaround for leaflet bug when geometry column is named
    names(st_geometry(x$osm_lines)) <- NULL
    names(st_geometry(x$osm_points)) <- NULL
    names(st_geometry(x$osm_polygons)) <- NULL
    
    RV$lines <- x$osm_lines
    RV$points <- x$osm_points
    RV$polys <- x$osm_polygons
    
    proxy <- leafletProxy("mapleaflet")
    proxy %>% addPolygons(x$osm_lines)
  })
    
})
