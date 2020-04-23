# Content of the box "Filter data"

shiny::div(

  shiny::div(
    style="vertical-align:center;padding-bottom:10px;",
    # shinyjs::disabled(shiny::selectInput(
    shiny::selectInput(
      # shinyWidgets::radioGroupButtons(
      inputId = "filter_buttons",
      label = span(
        shiny::strong(ph(ht("_filter_buttons", i18n),"\u2000")),
        actionLink("help_filter_buttons", icon("question-circle")),
        "\u00a0"
      ),
      choices = setNames(
        c("manual", "auto", "minimal", "no"),
        ht(c("_filter_buttons_manual", "_filter_buttons_auto",
             "_filter_buttons_minimal", "_filter_buttons_no"), i18n)
      ),
      selected = "no"
    )
  ),

  shiny::conditionalPanel(
    condition = "input.filter_buttons == 'manual'",
    shiny::div(

      shiny::div(
        style="vertical-align:center;",
        shiny::strong(ph(ht("_filters_title", i18n),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rangey", value = FALSE,
          size = "small", onLabel = ht("_On", i18n), offLabel = ht("_Off", i18n)
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::uiOutput("indata_rangey")
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

      shiny::div(
        style="vertical-align:center;",
        shiny::strong(ph(ht("_zscorey", i18n),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_zscorey", value = FALSE,
          size = "small", onLabel = ht("_On", i18n), offLabel = ht("_Off", i18n)
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
        shiny::strong(ph(ht("_rbiasy", i18n),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rbiasy", value = FALSE,
          size = "small", onLabel = ht("_On", i18n), offLabel = ht("_Off", i18n)
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
        shiny::strong(ph(ht("_rangeq", i18n),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rangeq", value = TRUE,
          size = "small", onLabel = ht("_On", i18n), offLabel = ht("_Off", i18n)
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
        shiny::strong(ph(ht("_pos", i18n),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_pos", value = TRUE,
          size = "small", onLabel = ht("_On", i18n), offLabel = ht("_Off", i18n)
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
        shiny::strong(ht("_importFilters", i18n)),
        shiny::div(
          style = "padding-bottom:5px;",
          shiny::div(
            style="display:inline-block;vertical-align:middle;",
            shinyFiles::shinyFilesButton(
              "sfb_importFilters",
              ht("_Import", i18n), ht("_sfb_importFilters", i18n),
              multiple=FALSE
            )
          ),
          shiny::div(
            style="display:inline-block;vertical-align:middle;padding-left:10px;",
            shinyFiles::shinySaveButton(
              "downloadFilters",
              ht("_Export", i18n), ht("_downloadFilters", i18n),
              filetype=list(json="json")
            )
          )
        ),
        shiny::div(
          style = "padding-bottom:5px;",
          shiny::actionButton(
            "setdefaultFilters", ht("_setdefaultFilters", i18n)
          )
        )
      )

    )
  )

)
