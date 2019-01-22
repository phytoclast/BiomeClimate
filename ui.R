#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
#calculate percentiles
library(plyr)

fluidPage(
  
  headerPanel('Climate Graphs - solar radiation based'),
  sidebarPanel(
    htmlOutput("Biome"),
    htmlOutput("ECO_NAME"),
    htmlOutput("country"),
    htmlOutput("elev"),
    htmlOutput("lat"),
    htmlOutput("lon"),
    htmlOutput("temp"),
    htmlOutput("prec")


  ),
  mainPanel(
    plotOutput("climplot"),
    verbatimTextOutput('Climtext'),
    fluidRow(
      column(width=5,
    radioButtons("RadioNorm", label = ("Select Timeframe"),
                 choices = list('Last Glacial Maximum ' = -25000, 'Mid Holocene' = -4000,
                                'Current' = 1990,
                                'Moderate global warming' = 2070
                                ,
                                'Stronger global warming' = 2071), 
                 selected = 1990)
      ),

    column(width = 2,
           radioButtons("RadioUnits", label = ("Select Units"),
                        choices = list('Metric System' = 'm', 
                                       'Medieval Units' = 'USC'), 
                        selected = 'm')
    ),
    column(width = 5,
           radioButtons("RadioGraphtype",inline = T,  label = ("Select Graph"),
                        choiceNames = list(HTML("<font size=-2>Monthly"), 
                                           HTML("Summer × Winter"),
                                           HTML("Summer × Moisture"), 
                                           HTML("Surplus × Deficit"),
                                           HTML("Summer × pAET"),  
                                           HTML("Winter × pAET"),
                                           HTML("Moisture × Deficit"),
                                           HTML("Moisture × Seasonality"),
                                           HTML("Map</font>")),
                        
                        choiceValues = list(1,2,4,5,6,7,8,3,9),
                        selected = 1),
           HTML("</font>")
    )),
           fluidRow(
                  HTML("<font size=-2>Last Glacial Maximum: ~26,500 years ago (model: CCSM4);<br>
                 Mid Holocene: ~6000 years ago (model: CCSM4);<br> Current: ~1961-1990 (WorldClim 1.4, http://worldclim.org/);<br>
                 Moderate global warming: at year 2070 (scenario = RCP 4.5, model = CCSM4);<br>
Stronger global warming: at year 2070 (scenario = RCP 8.5, model = CCSM4)
                       <br>More information about the 
                      "),
                  tags$a(href="https://www.researchgate.net/publication/327537609_Climate_Classification_Outline", "climate classification"),
                  HTML(" used above.</font>")
           )

  )
)
