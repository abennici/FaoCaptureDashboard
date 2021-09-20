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
library("gsheet")
library("gifski")
library("png")
#library("av")
library("paletteer")
library("stringr")
#library("orca")
#library("rmdformats")
library("glue")
library("reshape")
library("xtable")

####global function
collapse_transformer <- function(regex = "[*]$", ...) {
  function(text, envir) {
    collapse <- grepl(regex, text)
    if (collapse) {
      text <- sub(regex, "", text)
    }
    res <- identity_transformer(text, envir)
    if (collapse) {
      glue_collapse(res, ...)  
    } else {
      res
    }
  }
}



#load module functions
#source("https://raw.githubusercontent.com/eblondel/OpenFairViewer/master/src/resources/shinyModule/QueryInfo.R")
#source("D:/FAO-BLUECLOUD_04052020_11082020/02-R/04-Github/OpenFairViewer/src/resources/shinyModule/QueryInfo.R")
source("modules/DataSource.R")
source("Registers.R")
source("modules/Home.R")
source("modules/AreaChart.R")
source("modules/SpChart.R")
source("modules/FlagChart.R")
source("modules/PieMarineChart.R")
source("modules/PieInlandChart.R")
source("modules/FlagChart.R")
source("modules/StaticChart.R")
source("modules/Subset.R")
source("modules/InfoBox.R")
source("modules/RaceChart.R")
source("modules/TimeChart.R")
source("modules/DataTableWide.R")
source("modules/DownloadReport.R")
#source("modules/DataWithoutDsd.R")
source("ui.R")
source("server.R")

