
server_guinterp <- function(input, output, session) {

  rv <- reactiveValues()
  RV_yield <- reactiveValues() # FIXME remove alla fine!

  # get server volumes
  volumes <- c("Home"=path.expand("~"), shinyFiles::getVolumes()())

  # fix map_selvariable to "selvar"
  # to allow visualisation of more variables:
  # 1. add a radioGroupButtons of input$map_selvariable;
  # 2. replace rv$map_selvariable with input$map_selvariable in all the code
  rv$map_selvariable <- "selvar"

  # # open with hiden sidebarmenu
  # shinyjs::addClass(selector = "body", class = "sidebar-collapse")

  # option to mask intern tab
  rv$interp_onoff <- FALSE
  output$interp_onoff <- shiny::reactive(rv$interp_onoff)
  shiny::outputOptions(output, "interp_onoff", suspendWhenHidden = FALSE)

  # Disable filter tab
  shinyjs::hide(selector = "#tabs li a[data-value=tab_filter]")


  #### File selectors ####
  source(
    system.file("apps/guinterp/server/import_data_loadsave.R", package="guinterp"),
    local=TRUE
  )$value

  #### Filter selectors ####
  source(
    system.file("apps/guinterp/server/import_interp_filters.R", package="guinterp"),
    local=TRUE
  )$value

  #### Options selectors ####
  source(
    system.file("apps/guinterp/server/import_interp_options.R", package="guinterp"),
    local=TRUE
  )$value

  #### Tab: Input ####
  source(
    system.file("apps/guinterp/server/tab_input.R", package="guinterp"),
    local=TRUE
  )$value

  #### Update yield map ####
  source(
    system.file("apps/guinterp/server/update_interp_map.R", package="guinterp"),
    local=TRUE
  )$value

  #### Update yield histogram ####
  source(
    system.file("apps/guinterp/server/update_interp_histogram.R", package="guinterp"),
    local=TRUE
  )$value

  #### Launch interpolation ####
  source(
    system.file("apps/guinterp/server/launch_interpolation.R", package="guinterp"),
    local=TRUE
  )$value












  # if Exit is pressend, exit from GUI
  observeEvent(input$exit_gui, {
    shinyjs::js$closeWindow()
    # shinyjs::js$resetApp()
    stopApp()
  })
  observeEvent(input$finish_and_exit, {
    shinyjs::js$closeWindow()
    # shinyjs::js$resetApp()
    stopApp()
  })


} # end of yieldmaps_gui.server
