
server_guinterp <- function(input, output, session) {

  rv <- reactiveValues()

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

  # Disable filter tab
  shinyjs::hide(selector = "#tabs li a[data-value=tab_filter]")


  #### Filter selectors ####
  source(
    system.file("apps/guinterp/server/interp_filters.R", package="guinterp"),
    local=TRUE
  )$value

  #### Interpolation options selectors ####
  source(
    system.file("apps/guinterp/server/interp_options.R", package="guinterp"),
    local=TRUE
  )$value

  #### Oputput format options ####
  source(
    system.file("apps/guinterp/server/interp_outformat.R", package="guinterp"),
    local=TRUE
  )$value

  #### Processing options selectors ####
  source(
    system.file("apps/guinterp/server/interp_proc.R", package="guinterp"),
    local=TRUE
  )$value

  #### Tab: Input ####
  source(
    system.file("apps/guinterp/server/tab_input.R", package="guinterp"),
    local=TRUE
  )$value

  #### Update map ####
  source(
    system.file("apps/guinterp/server/update_interp_map.R", package="guinterp"),
    local=TRUE
  )$value

  #### Modal dialog for edit map ####
  source(
    system.file("apps/guinterp/server/modal_editmap.R", package="guinterp"),
    local=TRUE
  )$value

  #### Modal dialog for select points ####
  source(
    system.file("apps/guinterp/server/modal_selpts.R", package="guinterp"),
    local=TRUE
  )$value

  #### Modal dialog for variogram ####
  source(
    system.file("apps/guinterp/server/modal_variogram.R", package="guinterp"),
    local=TRUE
  )$value

  #### Update histogram ####
  source(
    system.file("apps/guinterp/server/update_interp_histogram.R", package="guinterp"),
    local=TRUE
  )$value

  #### Launch interpolation ####
  source(
    system.file("apps/guinterp/server/launch_interpolation.R", package="guinterp"),
    local=TRUE
  )$value

  #### Reset app ####
  source(
    system.file("apps/guinterp/server/close_interp.R", package="guinterp"),
    local=TRUE
  )$value

  #### Help dialogs ####
  source(
    system.file("apps/guinterp/server/helpers.R", package="guinterp"),
    local=TRUE
  )$value


} # end of server function
