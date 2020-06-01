# Content of the box "Interpolation settings"

shiny::div(

  shiny::div(
    style="vertical-align:center;",
    shiny::span(
      shiny::strong(ph(ht("_maxptdist"),"\u2000")),
      actionLink("help_maxptdist", icon("question-circle")),
      "\u00a0"
    )
  ),
  shiny::div(
    style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
    ""
  ),
  shiny::div(
    style="display:inline-block;vertical-align:middle;width:50pt;",
    shinyWidgets::switchInput(
      "maxptdist_onoff", value = TRUE,
      size = "small", onLabel = ht("_Yes"), offLabel = ht("_No")
    )
  ),
  shiny::div(
    style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
    shiny::uiOutput("maxptdist_ui")
  ),

  shiny::radioButtons(
    'interp_method',
    shiny::span(
      ph(ht("_interp_method"),"\u2000"),
      actionLink("help_interp_method", icon("question-circle"))
    ),
    choiceNames = ht(c("_interp_method_krige", "_interp_method_idw")),
    choiceValues = c("krige", "idw"),
    selected = "krige"
  ),

  # uiOutput("interp_sampling"), # FIXME remove?

  shiny::conditionalPanel(
    condition = "input.interp_method == 'krige'",
    shiny::div(
      shiny::div(
        style = "display:inline-block;position:relative;",
        shiny::radioButtons(
          'auto_vgm',
          shiny::span(
            ph(ht("_auto_vgm"),"\u2000"),
            actionLink("help_auto_vgm", icon("question-circle"))
          ),
          choiceNames = ht(c("_auto_vgm_auto", "_auto_vgm_semiauto", "_auto_vgm_manual")),
          choiceValues = c("auto", "semiauto", "manual"),
          selected = "auto"
        )
      ),
      shiny::div(
        style = "display:inline-block;position:relative;vertical-align:top;margin-top:40px;margin-left:-20px;",
        actionButton("fit_vgm_button", ph("\u2004",ht("_fit_vgm_button")), icon = shiny::icon("cog"))
      )
    )
  ),

  shiny::checkboxInput(
    "focal_onoff",
    shiny::span(
      ph(ht("_focal_onoff"),"\u2000"),
      actionLink("help_focal", icon("question-circle"))
    ),
    value = TRUE
  )

)
