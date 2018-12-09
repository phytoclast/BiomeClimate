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

pageWithSidebar(
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
    radioButtons("RadioNorm", label = h2("Select Timeframe"),
                 choices = list('Last Glacial Maximum ~26,500 years ago (model: CCSM4)' = -25000, 'Current ~1961-1990 (WorldClim 1.4, http://worldclim.org/)' = 1990,
                                'Moderate global warming at year 2070 (scenario = rcp45, model = CCSM4)' = 2070), 
                 selected = 1990)
  )
)
                                
