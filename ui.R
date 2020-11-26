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

#choices_relatedConcepts <- list("Gray infrastructure - roads" = "roads")
#choices_scales <- list("XL", "L", "M")
# Hp is to obtain these from a reactive value bound to osm available_features
# choices_osmFeatures <- list("highway" = "highway", "streams" = "streams") 
#choices_osmFeatures <- osmdata::available_features()

panels.about_text<- shiny::helpText(
                      p("Download OSM data for a selection of terms from the TELLme Glossary, exploiting the third-party service overpass API"),
                      h4("Usage"), 
                      div(style="font-size:smaller;",
                        p("1. Select one or more TELLme related concepts in the top bar."),
                        #div(class="leaflet-draw-toolbar leaflet-bar leaflet-draw-toolbar-top",
                        #                a(style="float:left; margin-right:2px;", class="leaflet-draw-draw-rectangle")),
                        p("2. Click on the black-square-icon control in the map and draw a rectangle to select a bounding box."),
                        p("3. Click \"Obtain data for related concept\" and evaluate the retrieved geographical features.",
                                       "When you are satisfied with the selection,"),
                        p("4. Click the \"Download results\" button to obtain a zip archive with as many shapefiles as the related concepts." )
                        ,h4("Credits"), p("Application developed for the TELLme ERASMUS+ project, O4. See"),a("source code repository on github",href="https://github.com/ptagliolato/TELLme-vlab-OSMDataFromRelatedConcept")
                      )
                    )
              

panels.troubleshooting_text<-shiny::helpText(p(style="font-size:smaller;",
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
    rightSidebarTabContent(id="help", icon="info", title="About", active = TRUE, panels.about_text),
    rightSidebarTabContent(id="troubleshooting", icon="fire-extinguisher", title="Troubleshooting",
      textAreaInput(inputId="errorlog", label="reported issues", resize="vertical"),
      panels.troubleshooting_text
    ),
    rightSidebarTabContent(id="controls",icon="gear",title = "Settings",
                           hidden(shiny::textInput(inputId = "user", label = "your tellme-hub userid", value = "")),
                           hidden(shiny::passwordInput(inputId = "password", label="your tellme-hub password", value = "")),
                           shiny::numericInput(inputId="overpass_timeout",label = "overpass timeout", value = "30",min = 10, max=600, step = 10),
                           div(style="font-size:smaller;",shiny::textAreaInput(inputId="overpass_query", label="overpass generated query", resize="vertical", height = 300))
    )
  ),
  sidebar = 
    dashboardSidebar(
      collapsed = TRUE,
      disable = TRUE,
      width = 0,
      sidebarMenu(
        menuItem("Elaboration", tabName = "site", icon = icon("map", lib = "font-awesome"))
      )
  ),
  
  body = dashboardBody(
    tags$head(tags$style(HTML('#overpass_query, #errorlog{font-size:smaller}'))),
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
                width = 2,
                style = "text-align:left",
                actionButton("downloadOverpass", "Obtain data for Related Concept")
              ),
              column(
                offset=0,
                width = 5,
                style = "text-align:right;",
                disabled(downloadButton("downloadShapeFiles", "Download results",width=6)),
                hidden(disabled(actionButton("uploadToGetIt", "Upload to TELLme HUB")))
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
              #column(width = 12,textInput("debug","debug")),
                column(width = 3, style = "text-align:left", disabled(textInput("bbxN", "NW-lat"))),
                column(width = 3, style = "text-align:left", disabled(textInput("bbxW", "NW-lon"))),
                column(width = 3, style = "text-align:left", disabled(textInput("bbxS", "SE-lat"))),
                column(width = 3, style = "text-align:left", disabled(textInput("bbxE","SE-lon"))),
                column(width=12, div(style="visibility:hidden",textOutput("bbx")))
            )
            
          )
          
        
          
        )#end fluidRow
      )#end tabItem
    )#end tabItems
  )#end dashboardBody
)#end dashboardPagePlus
