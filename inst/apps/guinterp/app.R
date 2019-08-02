#   app.R                                                          ####
#
#   Script used to initialize theguinterp app
#

#   ____________________________________________________________________________
#   Load required libraries                                               ####

require(shiny)
require(shinydashboard)
require(shinyFiles)
require(shinyWidgets)
require(shinyjs)
require(sf)
require(ggplot2)
require(foreach)
require(magrittr)
require(leaflet)
require(jsonlite)
require(data.table)

jscode <- "shinyjs.closeWindow = function() { open(location, '_self').close(); }"

#   ____________________________________________________________________________
#   Clean up on exit                                                        ####

onStop(function() {
  cat("Session stopped\n")
})

#   ____________________________________________________________________________
#   Source initialization script and main UI                                ####
# source("global.R")

source(
  system.file("apps/guinterp/ui/ui.R", package="guinterp"),
  local=TRUE
)$value

source(
  system.file("apps/guinterp/server/server.R", package="guinterp"),
  local=TRUE
)$value


shiny::shinyApp(ui_guinterp, server_guinterp)
