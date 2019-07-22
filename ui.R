#
# TELLme Erasmus Plus Project
# 
# Shiny app for ...
#
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
# cf. https://www.gdal.org/gdaldem.html
library(shiny)
# devtools::install_github("RinteRface/shinydashboardPlus", force = TRUE)
library(shinydashboardPlus)
library(shinydashboard)
#devtools::install_github('andrewsali/shinycssloaders')
library(shinycssloaders)
#install.packages("shinyjs")
library(shinyjs)
library(leaflet)
library(leaflet.extras)
#library(mapview)
#library(mapedit)
library(osmdata)

choices_relatedConcepts <- list("Gray infrastructure - roads" = "roads")
choices_scales <- list("XL", "L", "M")
# Hp is to obtain these from a reactive value bound to osm available_features
# choices_osmFeatures <- list("highway" = "highway", "streams" = "streams") 
#choices_osmFeatures <- osmdata::available_features()



panels.about<-div(style="padding:1em;",
                    shiny::helpText(
                      h4("Description"),p("This application exploits the third-party service overpass API to download OSM data in form of shapefiles (within zip archive)."),
                      h4("Usage"), p("Select a bounding box, then one or more TELLme related concepts.",
                                     "Click \"Obtain data for related concept\" and evaluate the retrieved geographical features.",
                                     "When you are satisfied with the selection, click the \"Download results\" button to obtain a zip archive with a shapefile per related concept." )
                      ,h4("Credits"), p("Application developed for the TELLme ERASMUS+ project, O4. See"),a("source code repository on github",href="https://github.com/ptagliolato/TELLme-vlab-OSMDataFromRelatedConcept")
                    )
              )
# choices_osmTags <- NULL

dashboardPagePlus(
  skin = "black-light",
  collapse_sidebar = FALSE,
  sidebar_fullCollapse = TRUE,
  dashboardHeaderPlus(
    title = tagList(
      tags$div(class = "logo-lg",
               tags$img(src = "http://tellmehub.get-it.it/static/img/logo1_200px.png", width = "80px", height = "40px"),
               tags$span("TELLme Erasmus+ Project - TELLme related concepts to OSM data to shapefiles")
      )
    ),
    titleWidth = 400
    # fixed = FALSE,
    ,enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  rightsidebar = rightSidebar( 
    rightSidebarTabContent(id="help", icon="info", title="About", active = TRUE, panels.about),
    rightSidebarTabContent(id="controls",icon="gear",title = "Settings",
      numericInput(inputId="overpass_timeout",label = "overpass timeout", value = "30",min = 10, max=600, step = 10),
      textAreaInput(inputId="overpass_query", label="overpass generated query", resize="vertical", height = 300)
    ),
    rightSidebarTabContent(id="troubleshooting", icon="fire-extinguisher", title="Troubleshooting",
      textAreaInput(inputId="errorlog", label="reported issues", resize="vertical"),
      shiny::helpText(p(style="font-size:smaller;",
        "If you incur in any issue, please check the \"log\" box here provided.",
        "The most common problems are due to the remote overpass service.",
        "You can try to extend the timeout (the time while the application waits for the remote service to answer with data) with the following numeric control.",
        "Note that the bigger the bounding box you select, the longer the time the application needs to wait the results for.",
        "Note also that not all the requests can be handled by the remote service (e.g. too big bounding boxes).",
        "If, after having extended the timeout, you cannot see the expected results,", 
        "you can also copy-paste the query composed by this application from the \"overpass generated query\" box",
        "and you can use it to directly invoke the remote API, via the",
        #"<a href=\"https://overpass-turbo.eu/\">overpass-turbo</a>",
        a("overpass-turbo",href="https://overpass-turbo.eu"),
        "web application, or via a terminal using CURL program."))
    )
  ),
  sidebar = 
    # dashboardSidebar(
    #   disable = FALSE,collapsed = FALSE,
    #   sidebarMenu(menuItem("Map", tabname="site"))
    #   #div(style="padding:1em;",
    #       # shiny::helpText(
    #       #   h4("Description"),p("This application exploits the third-party service overpass API to download OSM data in form of shapefiles (within zip archive)."),
    #       #   h4("Usage"), p("Select a bounding box, then one or more TELLme related concepts.", 
    #       #                  "Click \"Obtain data for related concept\" and evaluate the retrieved geographical features.", 
    #       #                  "When you are satisfied with the selection, click the \"Download results\" button to obtain a zip archive with a shapefile per related concept." )
    #       #   ,h4("Credits"), p("Application developed for the TELLme ERASMUS+ project, O4. See"),a("source code repository on github",href="https://github.com/ptagliolato/TELLme-vlab-OSMDataFromRelatedConcept")
    #       # )
    #   #)
    #),
    dashboardSidebar(
      collapsed = TRUE,
      disable = TRUE,
      width = 0,
      sidebarMenu(
        menuItem("Elaboration", tabName = "site", icon = icon("map", lib = "font-awesome"))
        #menuItem("About", tabName = "about", icon = icon("info", lib = "font-awesome"))
        #menuItem("Map", tabName = "map", icon = icon("map", lib = "font-awesome"))
        # ,
        # dropdownBlock(
        #   fileInput("file1", "Choose Digital Elevation Model File (raster image)",
        #             multiple = FALSE, accept = c("*/*","*,*",".*")),
        #   selectInput("mode", "choose mode", choices = choices),
        #   sliderInput("z", "Vertical exaggeration", min = 1, max = 50, value = 1),
        #   sliderInput("az", "Azimuth of the light", min = 1, max = 360, value = 315),
        #   sliderInput("alt", "Altitude of the light", min = 0, max = 90, value = 45),
        #   id = "ss", icon = icon("map", lib = "font-awesome"), title = "inputs"
        # )
      )
  ),
  
  body = dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName = "site",
        fluidRow(
          boxPlus(# inputs menu
            width = 12,
            #title = "input",
            background = "light-blue",
            closable = FALSE, status = "primary", solidHeader = FALSE, collapsible = FALSE,
            enable_sidebar = FALSE,
            #style = "background-color:black; color:white; padding: 0 10px;",
            fluidRow(
              column(
                width = 11,
                offset=1,
                # fileInput("file1", "Choose Digital Elevation Model File (raster image)",
                #           multiple = FALSE, accept = c("*/*","*,*",".*"))
                #selectInput("relatedconcept", "choose related concept", choices = choices_relatedConcepts)
                uiOutput("relConc")
               )
              # ,
              # # column(
              # #   width = 3,
              # #   selectInput("scale", "choose scale", choices = choices_scales)
              # # ),
              # # column(width = 3,
              # #        selectizeInput("features", "available features", choices = choices_osmFeatures, multiple = FALSE)
              # #        ),
              # column(width = 3,
              #        uiOutput("tags")
              #        #selectizeInput("tags","available tags",choices = choices_osmTags,multiple = TRUE)
              # )

            ),
            fluidRow(
              column(
                offset=1,
                width = 5,
                style = "text-align:left",
                actionButton("downloadOverpass", "Obtain data for Related Concept")
              ),
              column(
                offset=0,
                width = 6,
                style = "text-align:right;",
                disabled(downloadButton("downloadShapeFiles", "Download results",width=6))
              )
            )
          ),
          
          boxPlus(
            width = 12,
            leafletOutput("mapleaflet")
          ),
          
          boxPlus(
            width = 12,
            fluidRow(
              column(width=12, textOutput("bbx")),
              #column(width = 12,textInput("debug","debug")),
                column(width = 3, style = "text-align:left", textInput("bbxN", "NW-lat")),
                column(width = 3, style = "text-align:left", textInput("bbxW", "NW-lon")),
                column(width = 3, style = "text-align:left", textInput("bbxS", "SE-lat")),
                column(width = 3, style = "text-align:left", textInput("bbxE","SE-lon"))
            )
            
          )
          
        
          
        )#end fluidRow
      )#end tabItem
    )#end tabItems
  )#end dashboardBody
)#end dashboardPagePlus
