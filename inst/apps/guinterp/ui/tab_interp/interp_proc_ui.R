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
      shiny::strong("Raster di output: \u00a0")
    ),
    shiny::div(
      shiny::div(
        style="display:inline-block;vertical-align:top;width:70pt;",
        shinyFiles::shinySaveButton(
          "path_outraster_sel", "Seleziona",
          "Salva il raster di output come...",
          filetype = list(GeoTIFF = "tif")
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
        shiny::textInput("path_outraster_textin", NULL, "", placeholder = "salva in un raster temporaneo")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
        shiny::htmlOutput("path_outraster_errormess")
      )
    )
  ),

  shiny::div(
    checkboxInput(
      "outraster_savesingles",
      "Salva anche i raster dei singoli poligoni",
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.outraster_savesingles",
      shiny::div(
        shiny::div(
          style="vertical-align:top;margin-bottom:5px;",
          shiny::strong("Cartella di output: \u00a0")
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:70pt;",
          shinyFiles::shinyDirButton(
            "path_outdir_sel", "Seleziona",
            "Seleziona la cartella di output delle mappe raster"
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
          shiny::textInput("path_outdir_textin", NULL, "", placeholder = "usa una cartella temporanea")
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
          shiny::htmlOutput("path_outdir_errormess")
        )
      )
    )
  )#,

  # shiny::radioButtons(
  #   'interp_overwrite',
  #   shiny::span(
  #     "Sovrascrivere i raster esistenti?\u2000"#,
  #     # actionLink("help_interp_overwrite", icon("question-circle"))
  #   ),
  #   c('S\u00EC'=TRUE, 'No'=FALSE),
  #   selected = TRUE,
  #   inline = TRUE
  # )

)
