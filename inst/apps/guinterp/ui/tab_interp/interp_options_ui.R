# Content of the box "Opzioni di interpolazione"

shiny::div(

  shiny::div(
    style="vertical-align:center;",
    shiny::span(
      shiny::strong("Distanza massima dai punti:\u2000"),
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
      size = "small", onLabel = "S\u00EC", offLabel = "No"
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
      "Metodo di interpolazione\u2000",
      actionLink("help_interp_method", icon("question-circle"))
    ),
    c(
      # None='',
      'Kriging ordinario (pi\u00F9 affidabile)'='krige',
      'Distanza inversa ponderata (pi\u00F9 veloce)'='idw'
    ),
    selected = 'krige'
  ),

  uiOutput("interp_sampling"),

  shiny::conditionalPanel(
    condition = "input.interp_method == 'krige'",
    shiny::div(
      shiny::div(
        style = "display:inline-block;position:relative;",
        shiny::radioButtons(
          'auto_vgm',
          shiny::span(
            "Definizione del variogramma\u2000",
            actionLink("help_auto_vgm", icon("question-circle"))
          ),
          c('Automatica'="auto", 'Semiautomatica'="semiauto", 'Manuale'="manual"),
          selected = "auto"
        )
      ),
      shiny::div(
        style = "display:inline-block;position:relative;vertical-align:top;margin-top:40px;margin-left:-20px;",
        actionButton("fit_vgm_button", "Impostazioni")
        # bfsapps::bf_button("fit_vgm_button", type = "settings", action = "settings")
      )
    )
  )

)
