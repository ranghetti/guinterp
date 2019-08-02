# Content of the box "Opzioni di interpolazione"

shiny::div(

  shiny::span(
    shiny::strong("Sistema di riferimento:\u2000"),
    shiny::actionLink("help_out_proj", icon("question-circle")),
    "\u2000",
    shiny::actionLink("help_opts_disabled", shiny::span(style="color:orange;", shiny::icon("info-circle")))
  ),
  shiny::div(
    style="display:inline-block;position:relative;width:calc(100% - 10px - 10px);",
    shiny::uiOutput("out_proj_textinput")
  ),
  shiny::div(
    style="display:inline-block;position:relative;margin-left:5px;width:10px;",
    shiny::uiOutput("outproj_message")
  ),

  shiny::numericInput(
    'interp_res',
    "Risoluzione dei raster (in metri):",
    value = 5, min = 0.01, max = 100, step = 1
  ),

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
    'turbo',
    shiny::span(
      "Ottimizza l\'interpolazione per:\u2000",
      actionLink("help_turbo", icon("question-circle"))
    ),
    c(
      # None='',
      'Uso desktop'='low',
      'Uso server/workstation'='high'
    ),
    selected = 'low'
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
  ),

  shiny::div(
    shiny::div(
      style="display:inline-block;vertical-align:top;",
      shiny::strong("Cartella di output: \u00a0")),
    # shiny::div(style="display:inline-block;vertical-align:top;",
    #     htmlOutput("path_out_errormess")),
    shiny::div(
      shiny::div(
        style="display:inline-block;vertical-align:top;width:70pt;",
        shinyFiles::shinyDirButton(
          "path_out_sel", "Seleziona",
          "Seleziona la cartella di output delle mappe raster"
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
        shiny::textInput("path_out_textin", NULL, "", placeholder = "usa una cartella temporanea")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
        shiny::htmlOutput("path_out_errormess")
      )
    )
  ),

  shiny::radioButtons(
    'interp_overwrite',
    shiny::span(
      "Sovrascrivere i raster esistenti?\u2000"#,
      # actionLink("help_interp_overwrite", icon("question-circle"))
    ),
    c('S\u00EC'=TRUE, 'No'=FALSE),
    selected = TRUE,
    inline = TRUE
  )

)
