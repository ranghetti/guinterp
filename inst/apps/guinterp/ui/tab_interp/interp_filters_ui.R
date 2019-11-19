# Content of the box "Filter data"

shiny::div(

  shiny::div(
    style="vertical-align:center;padding-bottom:10px;",
    # shinyjs::disabled(shiny::selectInput(
    shiny::selectInput(
      # shinyWidgets::radioGroupButtons(
      inputId = "filter_buttons",
      label = span(
        shiny::strong(paste0(i18n$t("_filter_buttons"),"\u2000")),
        actionLink("help_filter_buttons", icon("question-circle")),
        "\u00a0"
      ),
      choices = setNames(
        c("manual", "auto", "minimal", "no"),
        c(i18n$t("_filter_buttons_manual"), i18n$t("_filter_buttons_auto"),
          i18n$t("_filter_buttons_minimal"), i18n$t("_filter_buttons_no"))
      ),
      selected = "no"
    )
  ),

  shiny::conditionalPanel(
    condition = "input.filter_buttons == 'manual'",
    shiny::div(

      shiny::div(
        style="vertical-align:center;",
        shiny::strong(paste0(i18n$t("_filters_title"),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rangey", value = FALSE,
          size = "small", onLabel = i18n$t("_On"), offLabel = i18n$t("_Off")
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
        shiny::uiOutput("indata_rangey")
      ),
      shiny::hr(style = "margin-top: 0em; margin-bottom: 0.75em;"),

      shiny::div(
        style="vertical-align:center;",
        shiny::strong(paste0(i18n$t("_zscorey"),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_zscorey", value = FALSE,
          size = "small", onLabel = i18n$t("_On"), offLabel = i18n$t("_Off")
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
        shiny::strong(paste0(i18n$t("_rbiasy"),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rbiasy", value = FALSE,
          size = "small", onLabel = i18n$t("_On"), offLabel = i18n$t("_Off")
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
        shiny::strong(paste0(i18n$t("_rangeq"),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_rangeq", value = TRUE,
          size = "small", onLabel = i18n$t("_On"), offLabel = i18n$t("_Off")
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
        shiny::strong(paste0(i18n$t("_pos"),"\u2000"))
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        ""
      ),
      shiny::div(
        style="display:inline-block;vertical-align:middle;width:50pt;",
        shinyWidgets::switchInput(
          "check_pos", value = TRUE,
          size = "small", onLabel = i18n$t("_On"), offLabel = i18n$t("_Off")
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
        shiny::strong(i18n$t("_importFilters")),
        shiny::div(
          style = "padding-bottom:5px;",
          shiny::div(
            style="display:inline-block;vertical-align:middle;",
            shinyFiles::shinyFilesButton(
              "sfb_importFilters",
              i18n$t("_Import"), i18n$t("_sfb_importFilters"),
              multiple=FALSE
            )
          ),
          shiny::div(
            style="display:inline-block;vertical-align:middle;padding-left:10px;",
            shinyFiles::shinySaveButton(
              "downloadFilters",
              i18n$t("_Export"), i18n$t("_downloadFilters"),
              filetype=list(json="json")
            )
          )
        ),
        shiny::div(
          style = "padding-bottom:5px;",
          shiny::actionButton(
            "setdefaultFilters", i18n$t("_setdefaultFilters")
          )
        )
      )

    )
  )

)
