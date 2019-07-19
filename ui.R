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


# choices_osmTags <- NULL

dashboardPagePlus(
  skin = "black-light",
  collapse_sidebar = FALSE,
  # sidebar_fullCollapse = TRUE,
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
    #fileInput("file1", "Choose Digital Elevation Model File (raster image)",
    #                      multiple = FALSE, accept = c("*/*", "*,*", ".*")),
    #            selectInput("mode", "choose mode", choices = choices),
    # sliderInput("z", "Vertical exaggeration", min = 1, max = 50, value = 1),
    # sliderInput("az", "Azimuth of the light", min = 1, max = 360, value = 315),
    # sliderInput("alt", "Altitude of the light", min = 0, max = 90, value = 45)
    textOutput("log"),
    numericInput(inputId="overpass_timeout",label = "overpass timeout", value = "30",min = 10, max=600, step = 10),
    textAreaInput(inputId="overpass_query", label="overpass generated query", resize="vertical"),
    textAreaInput(inputId="errorlog", label="reported issues", resize="vertical")

  ),
  sidebar = dashboardSidebar(
    collapsed = FALSE,
    disable = FALSE,
    width = 0,
    sidebarMenu(
      menuItem("Elaboration", tabName = "site", icon = icon("map", lib = "font-awesome"))
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
            #enable_sidebar = FALSE,
            #style = "background-color:black; color:white; padding: 0 10px;",
            fluidRow(
              column(
                width = 3,
                offset=1,
                # fileInput("file1", "Choose Digital Elevation Model File (raster image)",
                #           multiple = FALSE, accept = c("*/*","*,*",".*"))
                #selectInput("relatedconcept", "choose related concept", choices = choices_relatedConcepts)
                uiOutput("relConc")
                
               ),
              # column(
              #   width = 3,
              #   selectInput("scale", "choose scale", choices = choices_scales)
              # ),
              # column(width = 3,
              #        selectizeInput("features", "available features", choices = choices_osmFeatures, multiple = FALSE)
              #        ),
              column(width = 3,
                     uiOutput("tags")
                     #selectizeInput("tags","available tags",choices = choices_osmTags,multiple = TRUE)
              )

            ),
            fluidRow(
              column(
                offset=1,
                width = 4,
                style = "text-align:left",
                actionButton("downloadOverpass", "obtain data for Related Concept")
              ),
              # column(
              #   width = 2,
              #   style = "text-align:left",
              #   actionButton("reset", "clear map")
              # ),
              # column(
              #   offset = 1,
              #   width = 3,
              #   style = "text-align:left;",
              #   disabled(actionButton("doPlotMap", "Compute and plot output map",icon("cog")))
              # ),
              column(
                offset=3,
                width = 2,
                style = "text-align:right;",
                disabled(downloadButton("downloadShapeFiles", "Download results"))
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
