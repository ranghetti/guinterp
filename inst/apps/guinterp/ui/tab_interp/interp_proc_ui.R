# Content of the box "Opzioni di processamento"

shiny::div(

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

  shiny::div(
    shiny::div(
      style="display:inline-block;vertical-align:top;margin-bottom:5px;",
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
