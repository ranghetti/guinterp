# Content of the box "Formato di output"

shiny::div(

  shiny::radioButtons(
    "outgrid_type",
    "Griglia di output",
    c(
      "Definisci risoluzione e proiezione" = "custom",
      "Scegli da un raster esistente" = "ref"
    ),
    selected = "custom"
  ),
  shiny:::conditionalPanel(
    condition = "input.outgrid_type == 'custom'",
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::numericInput(
          'interp_res',
          "Risoluzione (m)",
          value = 5, min = 0.01, max = 100, step = 1
        )
      ),
      shiny::column(
        width = 7,
        shiny::div(
          style="display:inline-block;position:relative;width:calc(100% - 10px - 8px);",
          shiny::uiOutput("out_proj_textinput")
        ),
        shiny::div(
          style="display:inline-block;position:relative;margin-left:5px;width:10px;",
          shiny::uiOutput("outproj_message")
        )
      )
    )
  ),
  shiny:::conditionalPanel(
    condition = "input.outgrid_type == 'ref'",
    shiny::div(
      shiny::div(
        style="display:inline-block;vertical-align:top;margin-bottom:5px;",
        shiny::strong("Raster di riferimento: \u00a0")),
      shiny::div(
        shiny::div(
          style="display:inline-block;vertical-align:top;width:70pt;",
          shinyFiles::shinyFilesButton(
            "path_refraster_sel", "Seleziona",
            "Seleziona il raster da cui ottenere la griglia di riferimento",
            multiple = FALSE#,
            # filetype = list(
            #   GeoTIFF = c("tif", "tiff"),
            #   VRT = c("vrt"),
            #   `JPEG, PNG, JPEG2000` = c("jpg", "jp2", "png"),
            #   ENVI = c("envi", "dat"),
            #   `Tutti i file` =
            # )
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
          shiny::textInput("path_refraster_textin", NULL, "")
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
          shiny::htmlOutput("path_refraster_errormess")
        )
      )
    )
  )

)
