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
  
  headerPanel('Climate Graphs'),
  sidebarPanel(
    htmlOutput("Biome"),
    htmlOutput("ECO_NAME"),
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

    column(width = 3,
           radioButtons("RadioUnits", label = ("Select Units"),
                        choices = list('Metric System' = 'm', 
                                       'Medieval Units' = 'USC'), 
                        selected = 'm')
    )),
           fluidRow(
                  HTML("<font size=-1.5>Last Glacial Maximum: ~26,500 years ago (model: CCSM4);<br>
                 Mid Holocene: ~6000 years ago (model: CCSM4);<br> Current: ~1961-1990 (WorldClim 1.4, http://worldclim.org/);<br>
                 Moderate global warming: at year 2070 (scenario = RCP 4.5, model = CCSM4);<br>
Stronger global warming: at year 2070 (scenario = RCP 8.5, model = CCSM4)</font>")
           )

  )
)
