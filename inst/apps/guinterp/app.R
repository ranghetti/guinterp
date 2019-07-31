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
require(stats)
require(graphics)
require(ggplot2)
require(foreach)
require(magrittr)
require(leaflet)
require(methods)
require(jsonlite)
require(data.table)

# req_pkgs <- c("leaflet", "shiny", "shinyjs", "sf", "mapview", "rhandsontable",
#               "pals", "shinydashboard", "shinyFiles", "dplyr", "data.table",
#               "DT","shinyalert", "shinyBS", "openxlsx", "shinycssloaders",
#               "velox", "parallel", "shinyWidgets", "SearchTrees", "raster",
#               "ggplot2", "gstat", "sp", "rgdal", "rgeos", "leafem")
#
# for (pkg in req_pkgs) {
#   suppressPackageStartupMessages(library(pkg, character.only = TRUE))
# }

jscode <- "shinyjs.closeWindow = function() { open(location, '_self').close(); }"

#   ____________________________________________________________________________
#   Clean up on exit                                                        ####

onStop(function() {
  cat("Session stopped\n")

  # # Cleanup variables - use pos = 1 to get to "calling" env
  # rm(list_varieta, list_colture, list_ferts, list_fitos,
  #    options, RV_all, RV_pcolt, RV_treat, RV_yield, RV_init,
  #    ui_editdata, dicts_dir, jscode, list_tenims, map.types, opts_file,
  #    start_mainpath, volumes, modal_message, my_alpha2, exp_files, ff, pos = 1)

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
