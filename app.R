library(shiny)
library(readxl)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(stringi)
library(openxlsx)
library(leaflet)
library(shinyjs)


source("server.R")
source("ui.R")


shinyApp(ui = ui, server = server)