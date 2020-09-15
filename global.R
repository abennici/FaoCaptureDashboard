#---------------------------------------------------------------------------------------------------------
#packages
library("ows4R")
library("sp")
library("shiny")
library("DT")
library("shinyWidgets")
library("shinycssloaders")
library("jsonlite")
library("shinydashboard")
library("ggplot2")
library("gganimate")
library("plotly")
library("dplyr")
library("shinyjs")
library("shinybusy")

#load module functions
source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/resources/shinyModule/QueryInfo.R")
#source("D:/FAO-BLUECLOUD_04052020_11082020/02-R/04-Github/OpenFairViewer/src/resources/shinyModule/QueryInfo.R")
source("Registers.R")
source("modules/Home.R")
source("modules/Subset.R")
source("modules/RaceChart.R")
source("modules/TimeChart.R")
source("modules/DataTableWide.R")
source("ui.R")
source("server.R")

