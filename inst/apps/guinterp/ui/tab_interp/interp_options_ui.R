# Content of the box "Interpolation settings"

shiny::div(

  shiny::div(
    style="vertical-align:center;",
    shiny::span(
      shiny::strong(paste0(i18n$t("_maxptdist"),"\u2000")),
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
      size = "small", onLabel = i18n$t("_Yes"), offLabel = i18n$t("_No")
    )
  ),
  shiny::div(
    style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
    shiny::sliderInput(
      inputId="maxptdist", label=NULL,
      min = 0, max = 100, value = 15,
      post = " m", step = 1
    )
  ),

  shiny::radioButtons(
    'interp_method',
    shiny::span(
      paste0(i18n$t("_interp_method"),"\u2000"),
      actionLink("help_interp_method", icon("question-circle"))
    ),
    setNames(
      c("krige", "idw"),
      c(i18n$t("_interp_method_krige"), i18n$t("_interp_method_idw"))
    ),
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
            paste0(i18n$t("_auto_vgm"),"\u2000"),
            actionLink("help_auto_vgm", icon("question-circle"))
          ),
          setNames(
            c("auto", "semiauto", "manual"),
            c(i18n$t("_auto_vgm_auto"), i18n$t("_auto_vgm_semiauto"), i18n$t("_auto_vgm_manual"))
          ),
          selected = "auto"
        )
      ),
      shiny::div(
        style = "display:inline-block;position:relative;vertical-align:top;margin-top:40px;margin-left:-20px;",
        actionButton("fit_vgm_button", paste0("\u2004",i18n$t("_fit_vgm_button")), icon = shiny::icon("cog"))
      )
    )
  ),

  shiny::checkboxInput(
    "focal_onoff",
    shiny::span(
      paste0(i18n$t("_focal_onoff"),"\u2000"),
      actionLink("help_focal", icon("question-circle"))
    ),
    value = TRUE
  )

)
