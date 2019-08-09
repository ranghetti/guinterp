# Content of the box "Filtra dati"

shiny::div(

  shiny::div(
    style="vertical-align:center;padding-bottom:10px;",
    # shinyjs::disabled(shiny::selectInput(
    shiny::selectInput(
      # shinyWidgets::radioGroupButtons(
      inputId = "filter_buttons",
      label = span(
        shiny::strong("Filtraggio dei punti\u2000"),
        actionLink("help_filter_buttons", icon("question-circle")),
        "\u00a0"
      ),
      choices = c(
        "Manuale"="manual", "Predefinito"="auto",
        "Solo valori estremi"="minimal", "Nessuno"="no"
      ),
      selected = "no"
    )
  ),

  shiny::conditionalPanel(
    condition = "input.filter_buttons == 'manual'",
    shiny::div(

      shiny::div(
        style="vertical-align:center;",
        shiny::strong("Valori consentiti\u2000")
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rangey", value = FALSE,
          size = "small", onLabel = "On", offLabel = "Off"
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::uiOutput("indata_rangey")
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

      shiny::div(
        style="vertical-align:center;",
        shiny::strong("Zscore\u2000")
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_zscorey", value = FALSE,
          size = "small", onLabel = "On", offLabel = "Off"
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::sliderInput(
          inputId="zscorey",
          label=NULL,
          min = -300,
          max = 300,
          value = c(-100,100),
          post = "%",
          step = 5
        )
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

      shiny::div(
        style="vertical-align:center;",
        shiny::strong("Rbias\u2000")
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rbiasy", value = FALSE,
          size = "small", onLabel = "On", offLabel = "Off"
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::sliderInput(
          inputId="rbiasy",
          label=NULL,
          min = -200,
          max = 200,
          value = c(-75,75),
          post = "%",
          step = 5
        )
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

      shiny::div(
        style="vertical-align:center;",
        shiny::strong("Intervallo dei quantili\u2000")
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rangeq", value = TRUE,
          size = "small", onLabel = "On", offLabel = "Off"
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::sliderInput(
          inputId="rangeq",
          label=NULL,
          min = 0,
          max = 100,
          value = c(2,98),
          post = "%",
          step = 1
        )
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

      shiny::div(
        style="vertical-align:center;",
        shiny::strong("Distanza minima dal bordo dei poligoni\u2000")
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_pos", value = TRUE,
          size = "small", onLabel = "On", offLabel = "Off"
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::sliderInput(
          inputId="pos",
          label=NULL,
          min = 0,
          max = 50,
          value = 5,
          post = " m",
          step = 0.1
        )
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 1em;"),


      # Import/export filters
      shiny::tags$head(shiny::tags$script(src = "message-handler.js")),
      shiny::div(
        shiny::strong("Importa / esporta i filtri:"),
        shiny::div(
          style = "padding-bottom:5px;",
          shiny::div(
            style="display:inline-block;vertical-align:middle;",
            shinyFiles::shinyFilesButton(
              "sfb_importFilters",
              "Importa", "Scegli il file dei parametri",
              multiple=FALSE
            )
          ),
          shiny::div(
            style="display:inline-block;vertical-align:middle;padding-left:10px;",
            shinyFiles::shinySaveButton(
              "downloadFilters",
              "Esporta", "Salva i parametri come...",
              filetype=list(json="json")
            )
          )
        ),
        shiny::div(
          style = "padding-bottom:5px;",
          shiny::actionButton(
            "setdefaultFilters", "Imposta come predefiniti"
          )
        )
      )

    )
  )

)
