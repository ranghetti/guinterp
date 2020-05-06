#   ______________________________________________________________________
#   Script used to build the GUI of the tab used to edit/view data
#   data within the app app_editdat
#
# shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
shiny::div(

  shiny::fluidRow(
    # Main map ----
    shiny::column(
      width = 8,
      shiny::conditionalPanel(
        condition = "output.interp_onoff",
        div(
          # Histograms ---
          shinydashboard::box(
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            title = ht("_hist_title", i18n),
            collapsible = TRUE,
            shiny::fluidRow(
              shiny::column(
                width = 5,
                shiny::uiOutput("whichfield_ui")
              ),
              shiny::column(
                width = 7,
                shiny::uiOutput("samplesize_stats_ui")
              )
            ),
            shiny::plotOutput("hist", height = "270px"),
            DT::dataTableOutput("summary")
          ),
          # Main map ---
          shinydashboard::box(
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = ht("_interp_map_title", i18n),
            leaflet::leafletOutput("interp_map", height = 650),
            shiny::div(
              style = "text-align:right;",
              tags$i(ht("_note1_interp_map", i18n))
            ),
            shiny::uiOutput("samplesize_map_ui")
          )
        )
      )
    ), # end map colums
    # right-side selectors ----
    shiny::column(
      width = 4,
      shiny::conditionalPanel(
        condition = "output.interp_onoff",
        shiny::div(
          shinydashboard::box(
            width = NULL,
            title = ht("_filter_ui_title", i18n),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_filters_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = ht("_outformat_ui_title", i18n),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_outformat_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = ht("_interp_options_ui_title", i18n),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_options_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = ht("_interp_proc_ui_title", i18n),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_proc_ui.R", local = TRUE)$value
          )
        )
      )
    )
  )

)
