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
          # Sample size ---
          shinydashboard::box(
            width = NULL,
            # status = "danger",
            solidHeader = FALSE,
            # title = ht("_samplesize_view_title", i18n),
            # collapsible = TRUE,
            shiny::uiOutput("samplesize_view_ui")
          ),
          # Histograms ---
          shinydashboard::box(
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            title = ht("_hist_title", i18n),
            collapsible = TRUE,
            plotOutput("hist", height = "270px")
          ),
          # Main map ---
          shinydashboard::box(
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = ht("_interp_map_title", i18n),
            leaflet::leafletOutput("interp_map", height = 650),
            shiny::div(tags$i(ht("_note1_interp_map", i18n)))
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
