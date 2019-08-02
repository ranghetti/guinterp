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
            title = "Istogramma",
            collapsible = TRUE,
            plotOutput("hist", height = "270px")
          ),
          # Main map ---
          shinydashboard::box(
            width = NULL,
            status = "danger",
            solidHeader = TRUE,
            collapsible = TRUE,
            title = "Mappa",
            leaflet::leafletOutput("interp_map", height = 650),
            shiny::div(tags$i("I punti filtrati sono mostrati in azzurro."))
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
            title = "Filtra dati",
            solidHeader = TRUE,
            collapsible = TRUE,
            status = "danger",
            source("ui/tab_interp/import_interp_filters_ui.R", local = TRUE)$value
          ),
          shinydashboard::box(
            width = NULL,
            title = "Opzioni di interpolazione",
            solidHeader = TRUE,
            status = "danger",
            source("ui/tab_interp/import_interp_options_ui.R", local = TRUE)$value
          )
        )
      )
    )
  )

)
