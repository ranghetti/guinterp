### Content of the sidebar ###
shiny::div(

  ## Input selection (visible when interp_onoff is OFF)
  shiny::conditionalPanel(
    condition = "!output.interp_onoff",
    shiny::div(
      style = "margin-top:20px;margin-bottom:50px;",
      shiny::div(
        shiny::div(
          style = "margin-top:10px;padding-left:15px;",
          shiny::span(
            style="display:inline-block",
            shiny::strong(i18n$t("_load_borders"))
          ),
          shiny::span(
            style="display:inline-block",
            shiny::actionLink("help_load_borders", icon("question-circle"))
          )
        ),
        actionButton(
          "button_load_borders",
          label = paste0("\u2000",i18n$t("_Load")),
          class = "darkbutton",
          icon=icon("upload")
        )
      ),
      shiny::div(
        shiny::div(
          style = "margin-top:10px;padding-left:15px;",
          shiny::span(
            style="display:inline-block",
            shiny::strong(i18n$t("_load_inputpts"))
          ),
          shiny::span(
            style="display:inline-block",
            shiny::actionLink("help_load_inputpts", icon("question-circle"))
          )
        ),
        shinyjs::disabled(actionButton(
          "button_load_inputpts",
          label = paste0("\u2000",i18n$t("_Load")),
          class = "darkbutton",
          icon=icon("upload")
        ))
      )
    )
  ), # end of input conditionalPanel

  shiny::conditionalPanel(
    condition = "output.interp_onoff",
    shiny::div(
      shiny::div(
        style = "margin-top:20px;margin-bottom:50px;",
        actionButton(
          "interp_button",
          label = paste0("\u2000",i18n$t("_interp_button")),
          class = "darkbutton",
          icon=icon("cogs")
        )
      ),
      shiny::div(
        style = "margin-top:20px;",
        actionButton(
          "close_interp_button",
          label = paste0("\u2000",i18n$t("_Close")),
          class = "darkbutton",
          icon=icon("window-close")
        )
      )
    )
  ),

  shiny::div(
    style = "margin-top:20px;",
    actionButton(
      "close_app_button",
      label = paste0("\u2000",i18n$t("_close_app_button")),
      class = "darkbutton",
      icon=icon("sign-out-alt")
    )
  )

)
