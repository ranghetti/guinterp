#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive observeEvent renderUI sliderInput req actionLink
#'  renderText updateTextInput updateNumericInput modalDialog showModal fluidRow
#'  column isolate div strong icon div p HTML span sliderInput renderPlot
#'  removeModal updateSelectInput outputOptions conditionalPanel wellPanel
#'  tagList observe updateSliderInput
#' @importFrom shinydashboard box updateTabItems
#' @importFrom shinyFiles getVolumes shinyFileSave parseSavePath shinyDirChoose
#'  parseDirPath parseFilePaths shinyDirButton
#' @importFrom shinyWidgets switchInput progressBar updateRadioGroupButtons
#'  sendSweetAlert pickerInput
#' @importFrom shinyjs hide show enable disable disabled delay
#' @import shiny.i18n
#' @importFrom sf st_crs st_sf st_transform st_crs st_bbox st_geometry st_read
#'  st_polygon st_cast st_zm st_as_sfc
#' @importFrom ggplot2 ggplot geom_histogram scale_fill_manual scale_y_continuous
#'  xlab ylab geom_vline aes
#' @importFrom magrittr "%>%"
#' @importFrom leaflet addCircleMarkers addLegend leaflet addPolygons fitBounds
#'  addLayersControl showGroup hideGroup renderLeaflet leafletProxy removeMarker
#'  addRasterImage clearImages addTiles colorBin labelOptions layersControlOptions
#'  colorFactor
#' @importFrom leafpm pmToolbarOptions pmDrawOptions
#' @importFrom mapedit selectMod selectModUI editMod editModUI
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom shinyBS addTooltip bsTooltip
#' @import data.table
#' @importFrom DT renderDT datatable dataTableOutput
#' @noRd
guinterp_server <- function( input, output, session ) {

  rv <- reactiveValues()

  # read dictionary
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = system.file("app/translations", package="guinterp"))
  i18n$set_translation_language(getShinyOption("ui_lang", "en"))

  # add path for images
  addResourcePath( 'img', system.file('app/img', package = 'guinterp') )

  # get server volumes
  volumes <- c("Home"=path.expand("~"), shinyFiles::getVolumes()())

  # fix map_selvariable to "selvar"
  # to allow visualisation of more variables:
  # 1. add a radioGroupButtons of input$map_selvariable;
  # 2. replace map_selvariable with input$map_selvariable in all the code
  map_selvariable <- "selvar"

  # # open with hiden sidebarmenu
  # shinyjs::addClass(selector = "body", class = "sidebar-collapse")

  # option to mask intern tab
  rv$interp_onoff <- FALSE
  output$interp_onoff <- shiny::reactive(rv$interp_onoff)
  shiny::outputOptions(output, "interp_onoff", suspendWhenHidden = FALSE)

  # demo mode
  output$demo_mode <- renderText(getShinyOption("demo_mode") == TRUE)
  shiny::outputOptions(output, "demo_mode", suspendWhenHidden = FALSE)

  # Disable filter tab
  shinyjs::hide(selector = "#tabs li a[data-value=tab_filter]")


  #### Filter selectors ####
  source(
    system.file("app/server/interp_filters.R", package="guinterp"),
    local=TRUE
  )$value

  #### Interpolation options selectors ####
  source(
    system.file("app/server/interp_options.R", package="guinterp"),
    local=TRUE
  )$value

  #### Oputput format options ####
  source(
    system.file("app/server/interp_outformat.R", package="guinterp"),
    local=TRUE
  )$value

  #### Processing options selectors ####
  source(
    system.file("app/server/interp_proc.R", package="guinterp"),
    local=TRUE
  )$value

  #### Tab: Input ####
  source(
    system.file("app/server/tab_input.R", package="guinterp"),
    local=TRUE
  )$value

  #### Update histogram ####
  source(
    system.file("app/server/update_interp_histogram.R", package="guinterp"),
    local=TRUE
  )$value

  #### Update map ####
  source(
    system.file("app/server/update_interp_map.R", package="guinterp"),
    local=TRUE
  )$value

  #### Modal dialog for edit map ####
  source(
    system.file("app/server/modal_editmap.R", package="guinterp"),
    local=TRUE
  )$value

  #### Modal dialog for select points ####
  source(
    system.file("app/server/modal_selpts.R", package="guinterp"),
    local=TRUE
  )$value

  #### Modal dialog for variogram ####
  source(
    system.file("app/server/modal_variogram.R", package="guinterp"),
    local=TRUE
  )$value

  #### Launch interpolation ####
  source(
    system.file("app/server/launch_interpolation.R", package="guinterp"),
    local=TRUE
  )$value

  #### Reset app ####
  source(
    system.file("app/server/close_interp.R", package="guinterp"),
    local=TRUE
  )$value

  #### Help dialogs ####
  source(
    system.file("app/server/helpers.R", package="guinterp"),
    local=TRUE
  )$value

}
