

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# Author: Paolo Tagliolato (CNR IREA), Alessandro Oggioni (CNR IREA), Iacopo Neri (Polimi)
#

# TODO: problema attributi mancanti in shp risultante da amenity:recycling. Serve avere i tag "paper:yes" etc.
library(shiny)
library(osmdata)
library(sf)
library(sp)
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(geosapi) #libreria per accesso a geoserver API
#library(mapview)
library(digest) # xxhash64 is used to hash bbx in layer names in order to limit their nchar<64 (ncher(layernames)>=64 is invalid in postgis and causes errors)
library(RColorBrewer)

shinyServer(function(input, output, session) {
  # settings (colors, relatedConcepts->OSMfeature presets)
  {
    mycolors <- RColorBrewer::brewer.pal(9, name = "Set1")
    myblues <- RColorBrewer::brewer.pal(9, name = "Blues")
    mygrays <- RColorBrewer::brewer.pal(11, "RdGy")
    
    # TELLme palette
    color.canals.line="#90a4bf"
    color.streams.line="#cdd2d7"
    color.rivers.line="#23446b"
    color.buildArea.area="#9a0923"
    color.roads.line="#59080a"
    
    
    set_overpass_url("https://overpass-api.de/api/interpreter")
    
    #
    # curl http://tellmehub.get-it.it/api/keywords/ | jq -r '.objects[] | [.slug, .name] | @csv' | grep concept
    rc2concept_id<-list(roads=1,
                  body_of_water_rivers=2,
                  body_of_water_streams=3,
                  body_of_water_canals=4,
                  railways=5,
                  land_use=6,
                  recycling_points=7,
                  archelogical_sites=8,
                  belvedere=9,
                  market_global=10,
                  built_up_area=11,
                  water_harvesting=12,
                  educational_institutes=13,
                  subway=185
                  )
    
    if(ENABLE_UPLOAD){
      shinyjs::show("uploadToGetIt")
      shinyjs::show("user")
      shinyjs::show("password")
    }
    
    
    # list of lists. The named list contains the related concepts.
    # To each (named) relatedConcept corresponds a list of the following (named) items:
    #   key: OSM key
    #   features: tags to be selected within the key
    #   type: character vector with the type the layer to extract (values must be among: "points", "lines", "polygons").
    #   color: the color to use for plotting the layer in the leaflet map.
    rc2osmKeyFeat <- list(
      roads = list(
        key = "highway",
        features = c('motorway',
          'primary',
          'secondary',
          'tertiary',
          'trunk',
          'motorway_link',
          'primary_link',
          'secondary_link',
          'tertiary_link',
          'trunk_link',
          'residential'
        ),
        type = c("lines"),
        color = color.roads.line,
        conceptId = 1
      ),
      # TODO: mancano ancora: 
      # body_of_water_lakes, 
      # body_of_water_rivers, 
      # body_of_water_wetlands,
      # body_of_water_springs, 
      body_of_water_rivers = list(
        key = "waterway",
        features = 'river',
        type = c("lines"),
        color = color.rivers.line,
        conceptId=11
      ),
      body_of_water_streams = list(
        key = "waterway",
        features = 'stream',
        type = c("lines"),
        color = color.streams.line,
        conceptId=12
      ),
      body_of_water_canals = list(
        key = "waterway",
        features = 'canal',
        type = c("lines"),
        color = color.canals.line,
        conceptId=119
      ),
      railways = list(
        key = "railway",
        features = NA,
        type = c("lines"),
        color = mygrays[10],
        conceptId=24
      ),
      subway = list(
        key="railway",
        features=c("subway"),
        type=c("lines"),
        color="#b41d0f",
        conceptId=185
      ),
      land_use = list(
        key = "landuse",
        features = c('commercial',
                     'industrial',
                     'park',
                     'forest',
                     'farm'),
        type = c("polygons"),
        color = mycolors[7],
        conceptId=26
      ),
      recycling_points=list( #TODO: serve che restituisca cosa ricicla. es. recycling:paper
        key="amenity",
        features=c('recycling'),
        renameTagsForFeature=c('recycling'),
        type=c("points"),
        color=mycolors[7],
        conceptId=111
      ),
      archelogical_sites=list(
        key="historic",
        features=c('archaeological_site'),
        type=c("points"),
        color=mycolors[],
        conceptId=94
      ),
      belvedere=list(
        key="tourism",
        features=c('viewpoint'),
        type=c("points"),
        color=mycolors[],
        conceptId=29
      ),
      # landmark_and_monuments=list(
      #   key="",
      #   features=c(''),
      #   type=c("points"),
      #   color=mycolors[],
      #   conceptId=119
      # ),
      market_global=list(
        key="amenity",
        features=c('marketplace'),
        type=c("points"),
        color=mycolors[],
        conceptId=64
      ),
      # NOTE: very massive data. It must be limited.
      built_up_area=list(
        key="building",
        features=c(''),
        type=c("polygons"),
        color=color.buildArea.area,
        conceptId=2
      ),
      water_harvesting=list(
        key="man_made",
        features=c('water_tower','storage_tank'),
        type=c("points"),
        color=mycolors[],
        conceptId=125
      ),
      educational_institutes=list(
        key="amenity",
        features=c('college','kindergarden','language_school','library','school','university'),
        type=c("polygons"),
        color=mycolors[],
        conceptId=191
      )
    )
  }
  
  for(i in seq(length(rc2osmKeyFeat))){
    names(rc2osmKeyFeat)[i]<-TELLmeHub.getConceptCurrentLabel(rc2osmKeyFeat[[i]]$conceptId)
  }
  
  timeout <- reactive({
    input$overpass_timeout
  })
  
  
  # compose UI preset values with settings in the previous section
  output$relConc <- renderUI({
    shiny::selectizeInput(
      "relConc",
      "TELLme Related Concept",
      choices = names(rc2osmKeyFeat),
      multiple = TRUE
    )
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
  
  # leaflet map control: enable bounding box selection
  output$mapleaflet <- renderLeaflet({
    leaflet() %>% #addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(0, 0, 1) %>%  addSearchOSM() %>%
      addDrawToolbar(
        targetGroup = "draw",
        polylineOptions = FALSE,
        markerOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        polygonOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions = drawShapeOptions(
          fillOpacity = 0,
          color = 'white',
          weight = 3
        )),
        singleFeature = TRUE
      )
  })
  
  # RV reactive values. It contains the slots "layers" (list of downloaded layers - sp objects)
  # and a boolean flag "layersPresent" indicating if layers are already present in the layers list slot.
  RV <- reactiveValues(layers = list(), layersPresent = FALSE, queries= list())
  
  # print overpass queries
  observe({
    headings<-paste("Overpass API query composed for TELLme relatedConcept", names(RV$queries))
    output<-paste(headings, paste(RV$queries), sep=" \n-----\n ", collapse="\n*****\n")
    updateTextAreaInput(session, inputId = "overpass_query" , value = output)
  })
    
  # TODO: investigate (in shinyProxy) how we can obtain info about the logged-in user (it seems to be not so trivial... unluckily) - 
  # then move to input$user this info 
  # download button is enabled according to RV$layersPresent boolean value
  observe({
    if (RV$layersPresent) {
      enable("downloadShapeFiles")
      if(input$user!="" && input$password!="")
        shinyjs::enable("uploadToGetIt")
    }
    else{
      shinyjs::disable("downloadShapeFiles")
      shinyjs::disable("uploadToGetIt")
    }
  })
  
  # BOUNDING BOX
  {
    # BBX reactive values. It contains "N" "S" "E" "W" slots.
    BBX <- reactiveValues(N = NULL,
                          S = NULL,
                          E = NULL,
                          W = NULL)
    
    # bbx reactive. It returns the concatenation (array) of BBX slots.
    # [the following is lazy and cached. It is reactive: it is notified when its dependencies change]
    bbx <- reactive({
      # it is the same to explicitly return the value:
      # return(c(BBX$W, BBX$S, BBX$E, BBX$N))
      # or to simply end the expression with:
      c(BBX$W, BBX$S, BBX$E, BBX$N)
    })
    
    # concatenation of bbx with "_" separator
    bbx_concat <- reactive({
      digest(paste(bbx(), sep = "_", collapse = "_"), algo="xxhash64")
    })
    
    # print bounding box bbx (reactive) in label
    output$bbx <- renderText({
      bbx()
    })
    
    # intercept new bounding box drawn by the user and store it in BBX reactive values
    # (note: its within the pattern: observer->no return value, but side effects)
    observeEvent(input$mapleaflet_draw_new_feature, {
      shape <- input$mapleaflet_draw_new_feature
      
      polygon_coordinates <- shape$geometry$coordinates
      
      feature_type <- shape$properties$feature_type
      if (feature_type == "rectangle") {
        NW = polygon_coordinates[[1]][[2]]
        SE = polygon_coordinates[[1]][[4]]
        
        BBX$N <- paste(NW[[2]])
        BBX$W <- paste(NW[[1]])
        BBX$S <- paste(SE[[2]])
        BBX$E <- paste(SE[[1]])
      }
    })
    
    # print bbx components in TextInput elements
    # (also here there is an antipattern, maybe?
    # It would be more appropriate to use html output elements instead of input.. wouldn't it?)
    # TODO (?): let the user change the bbx through these inputs. The issue here is to synchronize the leaflet map bbx visualization.
    observe({
      updateTextInput(session, "bbxN", value = BBX$N)
      updateTextInput(session, "bbxW", value = BBX$W)
      updateTextInput(session, "bbxS", value = BBX$S)
      updateTextInput(session, "bbxE", value = BBX$E)
    })
    
    
  }
  
  # RESET LAYERS
  # reset the layers (in the map and in the RV layers slot) when bbx() changes or when the input$relConc changes
  # (i.e. when the user changes the selection of relatedConcepts)
  {
    observeEvent(bbx(), {
      proxy <- leafletProxy("mapleaflet")
      clearShapes(proxy)
      RV$layers = list()
      RV$layersPresent = FALSE
      RV$queries=list()
      proxy %>% leaflet::removeLayersControl()
    })
    
    observeEvent(input$relConc, {
      proxy <- leafletProxy("mapleaflet")
      clearShapes(proxy)
      RV$layers = list()
      RV$layersPresent = FALSE
      RV$queries=list()
      proxy %>% leaflet::removeLayersControl()
    })
  }
  
  #####----- download data -----
  # compose the overpass query and
  # download the data from overpass API
  observeEvent(input$downloadOverpass, ignoreInit = TRUE, {
    # precondition
    req(bbx())
    req(input$relConc)
    
    selectedConcepts <- input$relConc
    #browser()
    
    # now let's retrieve data...
    # but don't abuse of the user patience
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Processing request", value = 0)
    #i = 0
    
    errorRaised<-FALSE
    logstring<-c()
    logger$logger<-c(osmdata::overpass_status(),paste("OSM data download for bounding box: ",paste(bbx(), collapse=",")))
    increment<-1/(length(selectedConcepts)*3) # 2 steps for each concept: download and reproject
    for (selectedConcept in selectedConcepts) {
      errorRaised<-TRUE
      ####
      tryCatch(
        {
          ####
          #i = i + 1
          # ...progress:
          progress$inc(
            increment,
            detail = paste(selectedConcept,": retrieving OSM data..." )
          )
          
          # presets for the current selectedConcept
          key <- rc2osmKeyFeat[[selectedConcept]]$key
          features <- rc2osmKeyFeat[[selectedConcept]]$features
          type <- rc2osmKeyFeat[[selectedConcept]]$type
          color <- rc2osmKeyFeat[[selectedConcept]]$color
          
          # ...ok , let's retrieve data
          if (!is.na(features)) {
            q <- osmdata::opq(bbox = as.numeric(bbx()), timeout = timeout()) %>%
              add_osm_feature(key = key, value = features)
          }
          else{
            q <-
              osmdata::opq(bbox = as.numeric(bbx()), timeout = timeout()) %>%
              add_osm_feature(key = key) 
          }
          
          string_osm_query<-osmdata::opq_string(q)
          RV$queries[selectedConcept] <- string_osm_query
          
          # progress
          progress$inc(
            increment,
            detail = paste(selectedConcept,": elaborating data..." )
          )
          
          # tryCatch(
          #   {
          
          # x_xml<-q %>% osmadata_xml(filename=tempfile())
          # RV$xml[selectedConcept]<-x_xml
          
          ##> use sf instead of sp
          #x <- q %>% osmdata_sp()
          x<-q %>% osmdata::osmdata_sf()
          
          # workaround to solve leaflet issue. See https://github.com/r-spatial/sf/issues/880
          names(x$osm_points$geometry)<-NULL
          names(x$osm_lines$geometry)<-NULL
          names(x$osm_multilines$geometry)<-NULL
          names(x$osm_polygons$geometry)<-NULL
          names(x$osm_multipolygons$geometry)<-NULL
          
          
          #   },
          #   warning=function(w){
          #     output<-w
          #     logstring<-c(logstring,output)
          #     updateTextAreaInput(session, inputId = "errorlog" , "", value = logstring)
          #   },
          #   error=function(e){
          #     output<-e
          #     logstring<-c(logstring,output)
          #     updateTextAreaInput(session, inputId = "errorlog" , "", value = logstring)
          #   },
          #   finally={
          #     #output<-paste("-----\n\ndownloaded data summary:\n",summary(x),sep="\n")
          #     output<-paste("----\nquery terminated for",selectedConcept,"\n---\n")
          #     logstring<-c(logstring,output)
          #     updateTextAreaInput(session, inputId = "errorlog" , "", value = logstring)
          #   }
          # )
          
          ##> CRS in sf is already ok
          # # string for CRS settings
          # epsg=4326
          # #epsg=3857
          # CRS=paste("+init=epsg:",epsg,sep="")
          
          ####
          if (!is.na(match("points", type)) &&
              !is.null(x$osm_points) && length(x$osm_points) > 0) {
            # in order to correctly plot the layer in the leaflet component, we must reproject the data.
            
            ##> CRS in sf is already ok
            # progress$inc(
            #   increment,
            #   detail = paste(selectedConcept,": reprojecting data" )
            # )
            # points <- spTransform(x$osm_points, CRSobj = CRS)
           # points<-x$osm_points
            
            ##> TODO: check RV$layers usage after switching to SF
            RV$layers[[selectedConcept]] <- x$osm_points #points 
          }
          
          
          ####
          
          if (!is.na(match("lines", type)) &&
              !is.null(x$osm_lines) && length(x$osm_lines) > 0) {
            # in order to correctly plot the layer in the leaflet component, we must reproject the data.
            
            ##> CRS in sf is already ok
            # progress$inc(
            #   increment,
            #   detail = paste(selectedConcept,": reprojecting data" )
            # )
            # 
            # lines <- spTransform(x$osm_lines, CRSobj = CRS)
            #lines <- x$osm_lines
            RV$layers[[selectedConcept]] <- x$osm_lines #lines
          }
          
          if (!is.na(match("polygons", type)) &&
              !is.null(x$osm_polygons) && length(x$osm_polygons) > 0) {
            
            ##> CRS in sf is already ok
            # # in order to correctly plot the layer in the leaflet component, we must reproject the data.
            # polygons <-
            #   spTransform(x$osm_polygons, CRSobj = CRS)
            #polygons <- x$osm_polygons
            RV$layers[[selectedConcept]] <- x$osm_polygons #polygons
          }
          errorRaised<-FALSE
          rm(x)
          ##
        },
        # warning=function(w){
        #   output<-c("warning",w)
        #   browser()
        #   #logstring<-paste(c(logstring,paste(output,sep=";",collapse="\n")))
        #   logger$logger<-c(logger$logger,w)
        #   #updateTextAreaInput(session, inputId = "errorlog" , "", value = logstring)
        # },
        error=function(e){
          cat(paste(e))
          
          output<-c(paste("*** An error occurred while processing", selectedConcept),e)
          #browser()
          #logstring<-c(logstring,paste(output,sep=";",collapse="\n"))
          logger$logger<-c(logger$logger,output)
          #updateTextAreaInput(session, inputId = "errorlog" , "", value = logstring)
        },
        finally={
          msg=""
          if(errorRaised){
            output<-paste("Process failed for:",selectedConcept)
          }
          else{
            output<-paste("Process succeeded for:",selectedConcept)
          }
          #logstring<-c(logstring,paste(output,sep=";",collapse="\n"))
          logger$logger<-c(logger$logger,output)
          #updateTextAreaInput(session, inputId = "errorlog" , "", value = logstring)
          
        }
      )
      ##
    }
    RV$layersPresent <- TRUE
  })
  
  ####---- LOG ----
  logger<-reactiveValues(logger=c())
  observe({
    logstring<-paste(logger$logger,sep="; ", collapse="\n------\n")
    updateTextAreaInput(session, inputId = "errorlog" , "log", value = logstring)
  })

  ####---- plot downloaded data ----
  observe({
    #lookup layer type
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Plotting data...", value = 0)
    
    for (rc in names(RV$layers)) {
      osmdata <- RV$layers[[rc]]
      #warning(paste0("DEBUG: class RV$layers[[]] is: ",class(RV$layers[[rc]])))
      #warning(paste0("DEBUG: class RV$layers[] is: ",class(RV$layers[rc])))
      
              
      
      #names(st_geometry(osmdata))=NULL
      
      
      key <- rc2osmKeyFeat[[rc]]$key
      features <- rc2osmKeyFeat[[rc]]$features
      type <- rc2osmKeyFeat[[rc]]$type
      color <- rc2osmKeyFeat[[rc]]$color
      
      proxy <- leafletProxy("mapleaflet")
      
      ######
      if (!is.na(match("points", type))) {
        proxy %>% leaflet::addCircles(
          data = osmdata,
          weight = 2,
          color = color,
          group = rc
        )
      }
      #####
      
      
      if (!is.na(match("lines", type))) {
        proxy %>% leaflet::addPolylines(
          data = osmdata,
          weight = 2,
          color = color,
          group = rc
        )
      }
      if (!is.na(match("polygons", type))) {
        proxy %>% leaflet::addPolygons(data = osmdata,
                                       fillColor = color,
                                       color=color,
                                       group = rc)
      }
      proxy %>% addLayersControl(overlayGroups = names(RV$layers))
    }
    #plot layer
  })
  
  # current asset(s)name without any extension (it is composed by the layers names - relatedConcepts - and the bounding box)
  assetname <- reactive({
    paste(paste(names(RV$layers), collapse = "-"), bbx_concat(), sep = "_")
  })
  
  # download shape files
  # TODO: insert uploading to geoserver and get-it here (?)
  output$downloadShapeFiles <- downloadHandler(
    filename = function(){paste(assetname(), "zip", sep = ".")},# .zip extension is added here...
    content = function(file) {
      
      tmpdir <-
        tempdir() # e.g. "/var/folders/74/0djbrhs173376yz5pmdwdlr00000gn/T//RtmpDMosAR" note: it is for the entire session. Subsequent calls to the method do not change the path.
      
      # metto i file shape in una sottodir di tempdir, che ha il nome dell'asset
      # subdir of tmpdir: it contains all the shapefiles.
      #tmpshapedir = paste(tempdir(), assetname(), sep = "/") # I can use the assetname for the subfolder with temporary shapefiles too.
      message("-- tmpdir: ", tmpdir)
      
      #remove any pre-existing file from the folder
      unlink(paste(tmpdir, "*", sep = "/"),recursive=TRUE)
      message("-- removing old files from tmpdir: ", paste(tmpdir, "*", sep = "/"))
      
      #zipname<-paste(assetname(),"zip",sep=".")
      
      #tmpzipdir could be the same tmpdir. The zip archive will be named with the assetname.
      # at the end we will have: tmpdir/<asset>/
      ##                          tmpdir/<asset>.zip
      
      #setwd(tmpdir)
      for (curlayer_name in names(RV$layers)) {
        curlayer_file_name <- paste(curlayer_name, bbx_concat(), sep = "_")
        curlayer <- RV$layers[[curlayer_name]]
        
        # ##> debug shortening attribute names when writing shapefile
        # # change column names to control trimming names with the following convention on OSM Tags:
        # # e.g. if osm "feature" is "onefeaturexxx" then for each tag ("onetag") we have a boolean column named "onefeaturexxx.onetag"
        # # we want to write in the shapefile something like "t_onetag" in order to preserve the tag name as much as possible
        # nomi_feature<-rc2osmKeyFeat[[curlayer_name]]$features #lista?
        # posizioni_con_tag<-names(curlayer) %>%  grep(pattern=paste0(curlayer$features,".") )
        # names(curlayer)[posizioni_con_tag]<-gsub(names(curlayer)[posizioni_con_tag],pattern=recycling_points$features, replacement = "t")
        # ## QUI
        
        
        # ##> use sf instead of sp, so substitute rgdal::writeOGR with sf::st_write 
        # rgdal::writeOGR(
        #   obj = curlayer,
        #   dsn = tmpshapedir,
        #   layer = curlayer_file_name,
        #   driver = "ESRI Shapefile"
        # 
        dsn_tmpshapedir = paste(tmpdir, curlayer_file_name, sep = "/") 
        message("-- trying to write single shapefile to folder", dsn_tmpshapedir)
        sf::st_write(obj = curlayer, dsn=dsn_tmpshapedir,layer=curlayer_file_name, driver = "ESRI Shapefile") 
        #sf::st_write(obj = curlayer, dsn=tmpshapedir,layer=curlayer_file_name, driver = "ESRI Shapefile") 
        
        message("-- written single shapefile to folder")
      }
      
      fileslist <- list.files(path = tmpdir, full.names = TRUE, recursive = TRUE)
      message(fileslist)
      
      # TODO: check how many zip files. For the upload to geoserver/get-it there must be as many zip as the layers
      zip(zipfile = file #paste(tmpdir,zipname,sep="/")
          ,
          files = fileslist,
          flags = "-j")
      
    },
    contentType = "application/zip"
  )
  
  # upload to get-it
  
  observeEvent(input$uploadToGetIt, ignoreInit = TRUE, {
    # TODO: add trycatch block
    upload2getit=TRUE
    # TODO: we need the following params from somewhere in order to perform the get-it ingestion workflow:
    #shinyproxy_userName <- Sys.getenv("SHINYPROXY_USERNAME")
    #ownername<-shinyuser2getituser(shinyproxy_userName)# must be the connected (?) user counterpart in the get-it
    
    # layername<-"" # current layer name: must be instantiated during the for loop, layer per layer
    # keyword<-"" # it must be derived from the curlayer concept name
    concept2style<-list(
      c1=c("c_1-p_1-s_L-overpass","c_1-p_1-s_M-overpass","c_1-p_1-s_XL-overpass"),
      c11="c_11-p_1-s_XL-overpass",
      c119="c_119-p_1-s_L-overpass",
      c12="c_12-p_1-s_L-overpass" ,
      c125="c_125-p_1-s_L-overpass",
      c185="c_185-p_1-s_L-overpass",
      c191="c_191-p_1-s_L-overpass",
      c2="c_2-p_1-s_XL-overpass" ,
      c24="c_24-p_1-s_XL-overpass",
      c26="c_26-p_1-s_L-overpass",
      c64="c_64-p_1-s_M-overpass" )
    
    # TODO: show progress bar during operation.
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Uploading data to TELLme-Hub...", value = 0)
    #logger$logger<-c(
    increment<-1/(length(names(RV$layers))) # 1 step for each concept
    
    #progress::
    tmpdir <- 
      tempdir() # e.g. "/var/folders/74/0djbrhs173376yz5pmdwdlr00000gn/T//RtmpDMosAR" note: it is for the entire session. Subsequent calls to the method do not change the path.
    
    # metto i file shape in una sottodir di tempdir, che ha il nome dell'asset
    # subdir of tmpdir: it contains all the shapefiles.
    #tmpshapedir = paste(tempdir(), assetname(), sep = "/") # I can use the assetname for the subfolder with temporary shapefiles too.
    
    message("-- tmpdir: ", tmpdir)
    
    #remove any pre-existing file from the folder
    unlink(paste(tmpdir, "*", sep = "/"),recursive=TRUE)
    message("-- removing old files from tmpdir: ", paste(tmpdir, "*", sep = "/"))
    
    for (curlayer_name in names(RV$layers)) {
      
      curlayer_file_name <- paste(curlayer_name, bbx_concat(), sep = "_") # the filename without extension
      curlayer <- RV$layers[[curlayer_name]]
      curlayer_conceptId<-rc2osmKeyFeat[[curlayer_name]]$conceptId
      stylesforCurrentLayer<-concept2style[[paste0("c",curlayer_conceptId)]]
      
      progress$inc(
        increment,
        detail = paste("saving", curlayer_name,"..." )
      )
      
      message("-- concept id: ",curlayer_conceptId)
      warning("[dev] try using the RV archived layer directly (do not copy)")
      
      dsn_tmpshapedir = paste(tmpdir, curlayer_file_name, sep = "/") 
      message("-- trying to write single shapefile to folder", dsn_tmpshapedir)
      
      sf::st_write(obj = curlayer, dsn=dsn_tmpshapedir,layer=curlayer_file_name, driver = "ESRI Shapefile") 
      
      message("-- written single shapefile to folder")
      
      if(upload2getit){
        zipname=paste0(curlayer_file_name,".zip")
        fileslist <- list.files(path = dsn_tmpshapedir, full.names = TRUE)
        zipfullpath=paste(tmpdir, zipname, sep="/")
        zip(zipfile = zipfullpath,
            files = fileslist,
            flags = "-j")
        
        # TODO: uncomment the following function calls once checks of the previuos todo have been completed and the needed parameters (username, admin account, etc.) have been cabled into the app\
        keyword<-curlayer_name
        layername<-curlayer_file_name
        
        stylenames<-c("","","...") # possibly more than one!! need protocol number, scale, and also concept number, that should be mapped in the dictionary of relatedconcepts
        
        
        message("-- zipname_and_path (zipfullpath): ", zipfullpath)
        result<-getit_uploadLayer(getit_url = getit_url,
                          getit_user = input$user,
                          getit_userpassword = input$password,
                          zipname_and_path = zipfullpath,
                          layertitle = layername)
        
        status<-result$status #must be 200 - else signal - e.g. 400->content(result$response)$errors
        l_url<-result$getit_layerurl
        git_lname<-result$getit_layername
        geo_lname<-result$geoserver_layername
        
        msg<-paste("upload", curlayer_file_name, "(",l_url,")","to tellme hub - response status:", status)
        
        logger$logger<-c(logger$logger,msg)
        message(msg)
        
        #geoserver_layer_set_default_style()
        #getit_updatelayers()
        # TODO: set conceptId via tellmehub API
        if(status==200){
          resSetConcept<-tellmehub_layername_set_conceptid(getit_url = getit_url,
                                          getit_user = input$user,
                                          getit_password = input$password,
                                          concept_id = curlayer_conceptId,
                                          layername = git_lname
                                          )
          statusSetConcept<-status_code(resSetConcept)
          logger$logger<-c(logger$logger,paste("set concept id", git_lname, "to tellme hub - response status:", statusSetConcept))
          
          conc<-paste0("c",curlayer_conceptId)
          stylesforCurrentLayer<-concept2style[[paste0("c",curlayer_conceptId)]]
          # add styles and set default style
          for(styname in stylesforCurrentLayer){
            geoserver_layer_add_style(geoserver_url, geoserver_user, geoserver_password, 
                                      layername = git_lname,
                                      stylename = styname
            )
          }
          
          # cleanup
          tellmehub_delete_non_tellme_hierarchicalKeywords(getit_url = getit_url, getit_user = getit_superuser, getit_password = getit_superuser_password)
          tellmehub_api_clean_missing_style_title(getit_url = getit_url, getit_user = getit_superuser, getit_password = getit_superuser_password)
        }
      }
    }
    #TODO: close progress bar and complete logging
    
  })
  
})
