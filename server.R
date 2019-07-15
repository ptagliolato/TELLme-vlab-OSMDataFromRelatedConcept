
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
library(mapview)


shinyServer(function(input, output, session) {
  
  mycolors<- RColorBrewer::brewer.pal(9,name="Set1")
  myblues <- RColorBrewer::brewer.pal(9, name="Blues")
  mygrays <- RColorBrewer::brewer.pal(11,"RdGy")
  
  # list of lists. The named list contains the related concepts. 
  # To each (named) relatedConcept corresponds a list of the following (named) items:
  #   key: OSM key
  #   features: tags to be selected within the key
  #   type: character vector with the type the layer to extract (values must be among: "points", "lines", "polygons").
  #   color: the color to use for plotting the layer in the leaflet map.
  rc2osmKeyFeat<-list(
    roads=list(key="highway",features=c('motorway',
                                        'primary',
                                        'secondary'),
               type=c("lines"), color=mygrays[8]),
    body_of_water_rivers=list(key="waterway", features='rivers',type=c("lines"), color=myblues[9]),
    body_of_water_streams=list(key="waterway", features='stream',type=c("lines"), color=myblues[7]),
    body_of_water_canals=list(key="waterway", features='canal',type=c("lines"), color=myblues[5]),
    railways=list(key="railway",features=NA, type=c("lines"), color=mygrays[10]),
    land_use=list(key="landuse", features=c('commercial',
                                            'industrial',
                                            'park',
                                            'forest'), type=c("polygons"), color=mycolors[7])
  )
  
  output$relConc <- renderUI({
    shiny::selectizeInput("relConc","TELLme Related Concept", choices=names(rc2osmKeyFeat), multiple=TRUE)
  })
  
  # output$feature <- renderUI({
  #   choices_osm_features <- available_features()
  #   selectizeInput("features","available features",choices = choices_osm_features)
  # })
  # 
  
  # output$tags <- renderUI({
  #   choices_osmTags <- available_tags(input$features)
  #   selectizeInput("tags", "available tags", choices = choices_osmTags, multiple = TRUE)
  # })
  
  output$mapleaflet <- renderLeaflet({
    leaflet() %>% #addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
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
  
  BBX <- reactiveValues(N = NULL, S = NULL, E = NULL, W=NULL)
  
  # the following is lazy and cached. It is reactive: it is notified when its dependencies change
  bbx<-reactive({
    # it is the same to explicitly return the value:
    # return(c(BBX$W, BBX$S, BBX$E, BBX$N))
    # or to simply end the expression with:
    c(BBX$W, BBX$S, BBX$E, BBX$N)
  })
  
  # print bounding box bbx (reactive) in label
  output$bbx<-renderText({bbx()})
  
  # intercept new bounding box drawn by the user and store it in BBX reactive values
  # (note: its within the pattern: observer->no return value, but side effects)
  observeEvent(input$mapleaflet_draw_new_feature,{
    
    shape <- input$mapleaflet_draw_new_feature
    
    polygon_coordinates <- shape$geometry$coordinates
    
    feature_type <- shape$properties$feature_type
    if(feature_type == "rectangle"){
      NW = polygon_coordinates[[1]][[2]]
      SE = polygon_coordinates[[1]][[4]]
      
      BBX$N <- paste(NW[[2]])
      BBX$W <- paste(NW[[1]])
      BBX$S <- paste(SE[[2]])
      BBX$E <- paste(SE[[1]])
    }
  })
  
  # observeEvent(input$reset,{
  #   proxy<-leafletProxy("mapleaflet")
  #   clearShapes(proxy)
  # })
  
  observeEvent(bbx(),{
    proxy<-leafletProxy("mapleaflet")
    clearShapes(proxy)
    RV$layers=list()
  })
  
  observeEvent(input$relConc,{
    proxy<-leafletProxy("mapleaflet")
    clearShapes(proxy)
    RV$layers=list()
  })
  
  # print bbx components in input elements 
  # (also here there is an antipattern, maybe? 
  # It would be more appropriate to use html output elements instead of input.. wouldn't it?)
  observe({
    updateTextInput(session,"bbxN",value = BBX$N)
    updateTextInput(session,"bbxW",value = BBX$W)
    updateTextInput(session,"bbxS",value = BBX$S)
    updateTextInput(session,"bbxE",value = BBX$E)
  })
  
  RV <- reactiveValues(layers=list())
  #osmlayers <- reactiveValues(layers=list())
  
  #currentOSMData <- 
  # nonsense to assign an observers to a variable... it has no output!
  observeEvent(input$downloadOverpass, ignoreInit = TRUE, {
    # precondition
    req(bbx())
    req(input$relConc)
    
    selectedConcepts<-input$relConc
    #browser()
    
    # now let's retrieve data...
    # but don't abuse of the user patience
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Retrieving data...", value = 0)
    i=0
    for(selectedConcept in selectedConcepts){
      i=i+1
      # ...progress: 
      progress$inc(i/length(selectedConcepts), detail = paste("...retrieving OSM data for", selectedConcept))
      
      # required inputs
      #selectedConcept<-input$relConc
      
      # presets
      key<-rc2osmKeyFeat[[selectedConcept]]$key
      features<-rc2osmKeyFeat[[selectedConcept]]$features
      type<-rc2osmKeyFeat[[selectedConcept]]$type
      color<-rc2osmKeyFeat[[selectedConcept]]$color
      
      # ...ok , let's retrieve data
      if(!is.na(features)){
        try({
          x <-
            osmdata::opq(bbox = as.numeric(bbx())) %>% 
            add_osm_feature(key=key,value=features) %>%
            osmdata_sp()
        })
      }
      else{
        try({
          x <-
            osmdata::opq(bbox = as.numeric(bbx())) %>% 
            add_osm_feature(key=key) %>%
            osmdata_sp()
        })
      }
      #browser()
      
      
      # plot data on the map
      proxy <- leafletProxy("mapleaflet")
      #removeShape(proxy, layerId = selectedConcept)#if a "concept" is alredy added, remove it
      
      # progress
      progress$inc(i/length(selectedConcepts)+0.01*i, detail = paste("...processing", selectedConcept))
      
      if(!is.na(match("lines",type)) && !is.null(x$osm_lines) && length(x$osm_lines)>0){
        
        lines<-spTransform(x$osm_lines,CRSobj ="+init=epsg:4326")
        
        RV$layers[selectedConcept]<-lines
        #osmlayers$layers[selectedConcept]<-lines
        #browser()
        #RV$lines <- raster::union(RV$lines, x$osm_lines)
        #bbx<-stringr::str_replace_all(bbx()," ","_")
        #lid<-paste(selectedConcept,paste(bbx(),collapse = "_"),sep="-")
        if(FALSE){
          proxy %>% leaflet::addPolylines(data=lines, #layerId=lid, 
                                        weight=2, color=color)
        }
      }
      # if(!is.na(match("points",type)) && !is.null(x$osm_points) && length(x$osm_points)>0){
      #   points<-spTransform(x$osm_points,CRSobj ="+init=epsg:4326")
      #   RV$points <- x$osm_points
      #   proxy %>% leaflet::addMarkers(data=points)
      # }
      
      if(!is.na(match("polygons",type)) && !is.null(x$osm_polygons) && length(x$osm_polygons)>0){
        #browser()
        polygons<-spTransform(x$osm_polygons,CRSobj ="+init=epsg:4326")
        
        RV$layers[selectedConcept]<-polygons
        
        #osmlayers$layers[selectedConcept]<-
        #RV$polys <- x$osm_polygons
        if(FALSE){
          proxy %>% leaflet::addPolygons(data=polygons)
        }
      }
      
    }
  })
  
  observe({
    #lookup layer type
    
    for(rc in names(RV$layers)){
      #browser()
      osmdata<-RV$layers[[rc]]
        
      key<-rc2osmKeyFeat[[rc]]$key
      features<-rc2osmKeyFeat[[rc]]$features
      type<-rc2osmKeyFeat[[rc]]$type
      color<-rc2osmKeyFeat[[rc]]$color
      
      proxy <- leafletProxy("mapleaflet")
      if(!is.na(match("lines",type))){
        proxy %>% leaflet::addPolylines(data=osmdata, weight=2, color=color, group = rc)
      }
      if(!is.na(match("polygons",type))){
        proxy %>% leaflet::addPolygons(data=osmdata, color=color, group = rc)
      }
      proxy %>% addLayersControl(overlayGroups = names(RV$layers))
      
      
    }
    #plot layer
  })
  
  # observe({
  #   osmlayers$layers[selected]
  # })
  bbx_concat<-reactive({
    paste(bbx(),sep="_",collapse="_")
  })
  
  # current assetname without any extension (it is composed by the layers names - relatedConcepts - and the bounding box)
  assetname<-reactive({
    paste(names(RV$layers),"bbx",bbx_concat(),sep = "_")
  })
  
  observeEvent(assetname(),{enable("downloadShapeFiles")})
  
  output$downloadShapeFiles<-downloadHandler(
    filename=paste(assetname(),"zip", sep="."), # .zip extension is added here...
    content=function(file){
      #fs<-c() # array of filenames. Is it useful here?
      tmpdir<-tempdir() # e.g. "/var/folders/74/0djbrhs173376yz5pmdwdlr00000gn/T//RtmpDMosAR" note: it is for the entire session. Subsequent calls to the method do not change the path.
      
      # metto i file shape in una sottodir di tempdir, che ha il nome dell'asset
      # subdir of tmpdir: it contains all the shapefiles.
      tmpshapedir=paste(tempdir(),assetname(),sep="/") # I can use the assetname for the subfolder with temporary shapefiles too.
      
      browser()
      
      #remove any pre-existing file from the folder
      unlink(paste(tmpshapedir,"*",sep="/"))
             
      #zipname<-paste(assetname(),"zip",sep=".")
      
      #tmpzipdir could be the same tmpdir. The zip archive will be named with the assetname.
      # at the end we will have: tmpdir/<asset>/
      ##                          tmpdir/<asset>.zip
      
      #setwd(tmpdir)
      for(curlayer_name in names(RV$layers)){
        curlayer<-RV$layers[[curlayer_name]]
        #fs<-c(fs,curlayer_name) # is this useful here?
        rgdal::writeOGR(obj=curlayer, dsn=tmpshapedir, layer=curlayer_name, driver="ESRI Shapefile")
      }
      
      zip(zipfile=file #paste(tmpdir,zipname,sep="/")
          ,files=paste(tmpshapedir,dir(path=tmpshapedir),sep="/"))
      
      # can we make a loop here, produce the shapes, zip them and send the zip as the content?
      
      
      #writeRaster(currentProcessedRaster(),file, datatype = "INT1U", options=c("COMPRESSION=DEFLATE"))
    },
    contentType = "application/zip"
  )

})
