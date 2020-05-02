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
require(shiny.i18n)
require(sf)
require(ggplot2)
require(foreach)
require(magrittr)
require(leaflet)
require(jsonlite)
require(data.table)
library(guinterp)

jscode <- "shinyjs.closeWindow = function() { open(location, '_self').close(); }"

#   ____________________________________________________________________________
#   Clean up on exit                                                        ####

onStop(function() {
  cat("Session stopped\n")
})

# read dictionary
i18n <- shiny.i18n::Translator$new(translation_csvs_path = "./translations")
i18n$set_translation_language(getShinyOption("ui_lang", "en"))

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
