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
    verbatimTextOutput('Climtext')
    
  )
)

