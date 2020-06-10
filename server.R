

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
library(rgdal)
library(leaflet)
library(leaflet.extras)
library(geosapi) #libreria per accesso a geoserver API
#library(mapview)


shinyServer(function(input, output, session) {
  # settings (colors, relatedConcepts->OSMfeature presets)
  {
    mycolors <- RColorBrewer::brewer.pal(9, name = "Set1")
    myblues <- RColorBrewer::brewer.pal(9, name = "Blues")
    mygrays <- RColorBrewer::brewer.pal(11, "RdGy")
    
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
                  educational_institutes=13
                  )
    
    
    
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
        color = mygrays[8],
        conceptId = 1
      ),
      # TODO: mancano ancora: 
      # body_of_water_lakes, 
      # body_of_water_rivers, 
      # body_of_water_wetlands,
      # body_of_water_springs, 
      body_of_water_rivers = list(
        key = "waterway",
        features = 'rivers',
        type = c("lines"),
        color = myblues[9],
        conceptId=11
      ),
      body_of_water_streams = list(
        key = "waterway",
        features = 'stream',
        type = c("lines"),
        color = myblues[7],
        conceptId=12
      ),
      body_of_water_canals = list(
        key = "waterway",
        features = 'canal',
        type = c("lines"),
        color = myblues[5],
        conceptId=119
      ),
      railways = list(
        key = "railway",
        features = NA,
        type = c("lines"),
        color = mygrays[10],
        conceptId=24
      ),
      land_use = list(
        key = "landuse",
        features = c('commercial',
                     'industrial',
                     'park',
                     'forest'),
        type = c("polygons"),
        color = mycolors[7],
        conceptId=26
      ),
      recycling_points=list(
        key="amenity",
        features=c('recycling'),
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
        color=mycolors[],
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
    
  # download button is enabled according to RV$layersPresent boolean value
  observe({
    if (RV$layersPresent) {
      enable("downloadShapeFiles")
      enable("uploadToGetIt")
    }
    else{
      disable("downloadShapeFiles")
      enable("uploadToGetIt")
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
      paste(bbx(), sep = "_", collapse = "_")
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
    logger$logger<-c(osmdata::overpass_status(),paste("OSM data download for bounding box: ",bbx()))
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
          
          x <- q %>% osmdata_sp()
          
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
          
          # string for CRS settings
          epsg=4326
          #epsg=3857
          CRS=paste("+init=epsg:",epsg,sep="")
          
          ####
          if (!is.na(match("points", type)) &&
              !is.null(x$osm_points) && length(x$osm_points) > 0) {
            # in order to correctly plot the layer in the leaflet component, we must reproject the data.
            
            progress$inc(
              increment,
              detail = paste(selectedConcept,": reprojecting data" )
            )
            
            points <- spTransform(x$osm_points, CRSobj = CRS)
            RV$layers[selectedConcept] <- points
          }
          
          
          ####
          
          if (!is.na(match("lines", type)) &&
              !is.null(x$osm_lines) && length(x$osm_lines) > 0) {
            # in order to correctly plot the layer in the leaflet component, we must reproject the data.
            
            progress$inc(
              increment,
              detail = paste(selectedConcept,": reprojecting data" )
            )
            
            lines <- spTransform(x$osm_lines, CRSobj = CRS)
            RV$layers[selectedConcept] <- lines
          }
          
          if (!is.na(match("polygons", type)) &&
              !is.null(x$osm_polygons) && length(x$osm_polygons) > 0) {
            # in order to correctly plot the layer in the leaflet component, we must reproject the data.
            polygons <-
              spTransform(x$osm_polygons, CRSobj = CRS)
            RV$layers[selectedConcept] <- polygons
          }
          errorRaised<-FALSE
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
  
  #
  # log
  #
  logger<-reactiveValues(logger=c())
  observe({
    logstring<-paste(logger$logger,sep="; ", collapse="\n------\n")
    updateTextAreaInput(session, inputId = "errorlog" , "log", value = logstring)
  })

  # plot downloaded data.
  observe({
    #lookup layer type
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Plotting data...", value = 0)
    
    for (rc in names(RV$layers)) {
      osmdata <- RV$layers[[rc]]
      
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
                                       color = color,
                                       group = rc)
      }
      proxy %>% addLayersControl(overlayGroups = names(RV$layers))
    }
    #plot layer
  })
  
  # current asset(s)name without any extension (it is composed by the layers names - relatedConcepts - and the bounding box)
  assetname <- reactive({
    paste(paste(names(RV$layers), collapse = "-"), "bbx", bbx_concat(), sep = "_")
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
      tmpshapedir = paste(tempdir(), assetname(), sep = "/") # I can use the assetname for the subfolder with temporary shapefiles too.
      
      #remove any pre-existing file from the folder
      unlink(paste(tmpshapedir, "*", sep = "/"))
      
      #zipname<-paste(assetname(),"zip",sep=".")
      
      #tmpzipdir could be the same tmpdir. The zip archive will be named with the assetname.
      # at the end we will have: tmpdir/<asset>/
      ##                          tmpdir/<asset>.zip
      
      #setwd(tmpdir)
      for (curlayer_name in names(RV$layers)) {
        curlayer_file_name <- paste(curlayer_name, bbx_concat(), sep = "_bbx_")
        curlayer <- RV$layers[[curlayer_name]]
        rgdal::writeOGR(
          obj = curlayer,
          dsn = tmpshapedir,
          layer = curlayer_file_name,
          driver = "ESRI Shapefile"
        )
      }
      
      fileslist <- list.files(path = tmpshapedir, full.names = TRUE)
      
      # TODO: check how many zip files. For the upload to geoserver/get-it there must be as many zip as the layers
      zip(zipfile = file #paste(tmpdir,zipname,sep="/")
          ,
          files = fileslist,
          flags = "-j")
      
    },
    contentType = "application/zip"
  )
  
  # upload to get-it
  # TODO: we need more info about the final get-it instance and the user currently acting on the app.
  # TODO: investigate (in shinyProxy) how we can obtain info about the logged-in user (it seems to be not so trivial... unluckily)
  observeEvent(input$uploadToGetIt, ignoreInit = TRUE, {
    # TODO: refactor parts of this code in order to have just one replica of saving shapefiles (here and in the previous download handler)
    # TODO: we need the following params from somewhere in order to perform the get-it ingestion workflow:
    shinyproxy_userName <- Sys.getenv("SHINYPROXY_USERNAME")
    ownername<-shinyuser2getituser(shinyproxy_userName)# must be the connected (?) user counterpart in the get-it
    
    # to be defined for the entire app (they could be in the global.R, in the project case)
    # getit_url<-"http://tellmehub.get-it.it"
    # getit_superuser<-"" # admin user in the get-it: it is needed to invoke the updatelayers rest API
    # getit_password<-""  # it is the admin password
    # geoserver_url<-"http://tellmehub.get-it.it/geoserver"
    # workspacename<-"geonode" # it should be this one for the TELLme project
    # datastorename<-"vlab" # same as previous
    # geoserver_user<-"" #geoserver admin user
    # geoserver_password<-"" #geoserver admin password

    # layername<-"" # current layer name: must be instantiated during the for loop, layer per layer
    # keyword<-"" # it must be derived from the curlayer concept name
    
    
    # TODO: show progress bar during operation.
    #progress::
    tmpdir <-
      tempdir() # e.g. "/var/folders/74/0djbrhs173376yz5pmdwdlr00000gn/T//RtmpDMosAR" note: it is for the entire session. Subsequent calls to the method do not change the path.
    
    # metto i file shape in una sottodir di tempdir, che ha il nome dell'asset
    # subdir of tmpdir: it contains all the shapefiles.
    #tmpshapedir = paste(tempdir(), assetname(), sep = "/") # I can use the assetname for the subfolder with temporary shapefiles too.
    
    #remove any pre-existing file from the folder
    #unlink(paste(tmpshapedir, "*", sep = "/"))
    
    #zipname<-paste(assetname(),"zip",sep=".")
    
    #tmpzipdir could be the same tmpdir. The zip archive will be named with the assetname.
    # at the end we will have: tmpdir/<asset>/
    ##                          tmpdir/<asset>.zip
    
    #setwd(tmpdir)
    for (curlayer_name in names(RV$layers)) {
      curlayer_file_name <- paste(curlayer_name, bbx_concat(), sep = "_bbx_") # the filename without extension
      
      tmpshapedir<-paste(tempdir(),curlayer_file_name,sep="/") # this is the directory for a single shapefile
      unlink(paste(tmpshapedir, "*", sep = "/"))
      
      curlayer <- RV$layers[[curlayer_name]]
      rgdal::writeOGR(
        obj = curlayer,
        dsn = tmpshapedir,
        layer = curlayer_file_name,
        driver = "ESRI Shapefile"
      )
      if(upload2getit){
        zipname=paste0(curlayer_file_name,"zip",sep=".")
        fileslist <- list.files(path = tmpshapedir, full.names = TRUE)
        zipfullpath=paste(tmpdir,zipname,sep="/")
        zip(zipfile = zipfullpath,
            files = fileslist,
            flags = "-j")
        # perform curl related actions
        # TODO: for the moment just add some textual output with curl statements to follow the code
        
        # TODO: uncomment the following function calls once checks of the previuos todo have been completed and the needed parameters (username, admin account, etc.) have been cabled into the app\
        keyword<-curlayer_name
        layername<-curlayer_file_name
        stylename<-"" # need protocol number, scale, and also concept number, that should be mapped in the dictionary of relatedconcepts
        
        getit_geoserver_upload_layer(geoserver_url = geoserver_url, 
                                     geoserver_user = geoserver_user, 
                                     geoserver_password = geoserver_password,
                                     workspacename=workspacename,
                                     datastorename = datastorename,
                                     layername = layername,
                                     keyword = keyword
                                    )
        #geoserver_layer_set_default_style()
        #getit_updatelayers()
        # TODO: make some output visible at least in log
      }
    }
    #TODO: close progress bar and complete logging
    
  })
  
})
