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
            title = i18n$t("_hist_title"),
            collapsible = TRUE,
            plotOutput("hist", height = "270px")
          ),
          # Main map ---
          shinydashboard::box(
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = i18n$t("_interp_map_title"),
            leaflet::leafletOutput("interp_map", height = 650),
            shiny::div(tags$i(i18n$t("_note1_interp_map")))
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
            title = i18n$t("_filter_ui_title"),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_filters_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = i18n$t("_outformat_ui_title"),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_outformat_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = i18n$t("_interp_options_ui_title"),
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/interp_options_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = i18n$t("_interp_proc_ui_title"),
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
