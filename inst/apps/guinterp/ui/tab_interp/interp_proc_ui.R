# Content of the box "Processing options"

shiny::div(

  shiny::radioButtons(
    'turbo',
    shiny::span(
      paste0(i18n$t("_turbo"),"\u2000"),
      actionLink("help_turbo", icon("question-circle"))
    ),
    setNames(
      c("low", "high"),
      c(i18n$t("_turbo_low"), i18n$t("_turbo_high"))
    ),
    selected = 'low'
  ),

  shiny::div(
    shiny::div(
      style="display:inline-block;vertical-align:top;margin-bottom:5px;",
      paste0(i18n$t("_outraster")," \u00a0")
    ),
    shiny::div(
      shiny::div(
        style="display:inline-block;vertical-align:top;width:70pt;",
        shinyFiles::shinySaveButton(
          "path_outraster_sel", i18n$t("_Select"),
          i18n$t("_path_outraster_sel"),
          filetype = list(GeoTIFF = "tif")
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
        shiny::textInput("path_outraster_textin", NULL, "", placeholder = i18n$t("_path_outraster_textin_placeholder"))
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
      i18n$t("_outraster_savesingles"),
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.outraster_savesingles",
      shiny::div(
        shiny::div(
          style="vertical-align:top;margin-bottom:5px;",
          paste0(i18n$t("_outfolder")," \u00a0")
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:70pt;",
          shinyFiles::shinyDirButton(
            "path_outdir_sel", i18n$t("_Select"),
            i18n$t("_path_outdir_sel")
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
          shiny::textInput("path_outdir_textin", NULL, "", placeholder = i18n$t("_path_outdir_textin_placeholder"))
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
          shiny::htmlOutput("path_outdir_errormess")
        )
      )
    )
  ),

  shiny::checkboxInput(
    "v_options_onoff",
    shiny::span(
      shiny::strong(paste0(i18n$t("_v_options_onoff"),"\u2000")),
      actionLink("help_v_options", icon("question-circle")),
      "\u00a0"
    ),
    value = FALSE
  ),
  shiny::conditionalPanel(
    condition = "input.v_options_onoff",
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::div(i18n$t("_v_maxdist")),
        shiny::div(
          style="display:inline-block;vertical-align:middle;height:60px;padding-bottom:10px;",
          ""
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:60pt;",
          shinyWidgets::switchInput(
            "v_maxdist_onoff", value = TRUE,
            size = "small", onLabel = i18n$t("_Auto"), offLabel = i18n$t("_Man")
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:calc(100% - 60pt - 3px - 2pt);",
          shiny::uiOutput("v_maxdist_ui")
        )
      ),
      shiny::column(
        width = 6,
        shiny::div(i18n$t("_v_nmax")),
        shiny::div(
          style="display:inline-block;vertical-align:middle;height:60px;padding-bottom:10px;",
          ""
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:60pt;",
          shinyWidgets::switchInput(
            "v_nmax_onoff", value = TRUE,
            size = "small", onLabel = i18n$t("_Auto"), offLabel = i18n$t("_Man")
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:calc(100% - 60pt - 3px - 2pt);",
          shiny::uiOutput("v_nmax_ui")
        )
      )
    )
  )

  # shiny::radioButtons(
  #   'interp_overwrite',
  #   shiny::span(
  #     paste0(i18n$t("_interp_overwrite"),"\u2000")#,
  #     # actionLink("help_interp_overwrite", icon("question-circle"))
  #   ),
  #   c(i18n$t("_Yes), i18n$t("_No)),
  #   selected = TRUE,
  #   inline = TRUE
  # )

)
