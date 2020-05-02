# Content of the box "Processing options"

shiny::div(

  shiny::radioButtons(
    'turbo',
    shiny::span(
      ph(ht("_turbo", i18n),"\u2000"),
      actionLink("help_turbo", icon("question-circle"))
    ),
    setNames(
      c("low", "high"),
      ht(c("_turbo_low", "_turbo_high"), i18n)
    ),
    selected = 'low'
  ),

  shiny::div(
    shiny::div(
      style="display:inline-block;vertical-align:top;margin-bottom:5px;",
      ph(ht("_outraster", i18n)," \u00a0")
    ),
    shiny::div(
      shiny::div(
        style="display:inline-block;vertical-align:top;width:70pt;",
        shinyFiles::shinySaveButton(
          "path_outraster_sel", ht("_Select", i18n),
          ht("_path_outraster_sel", i18n),
          filetype = list(GeoTIFF = "tif")
        )
      ),
      shiny::div(
        style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
        shiny::textInput(
          "path_outraster_textin", NULL, "",
          placeholder = ht("_path_outraster_textin_placeholder", i18n)
        )
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
      ht("_outraster_savesingles", i18n),
      value = FALSE
    ),
    shiny::conditionalPanel(
      condition = "input.outraster_savesingles",
      shiny::div(
        shiny::div(
          style="vertical-align:top;margin-bottom:5px;",
          ph(ht("_outfolder", i18n)," \u00a0")
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:70pt;",
          shinyFiles::shinyDirButton(
            "path_outdir_sel", ht("_Select", i18n),
            ht("_path_outdir_sel", i18n)
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 6px - 10pt - 15px);",
          shiny::textInput(
            "path_outdir_textin", NULL, "",
            placeholder = ht("_path_outdir_textin_placeholder", i18n)
          )
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
      shiny::strong(ph(ht("_v_options_onoff", i18n),"\u2000")),
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
        shiny::div(ht("_v_maxdist", i18n)),
        shiny::div(
          style="display:inline-block;vertical-align:middle;height:60px;padding-bottom:10px;",
          ""
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:60pt;",
          shinyWidgets::switchInput(
            "v_maxdist_onoff", value = TRUE,
            size = "small", onLabel = ht("_Auto", i18n), offLabel = ht("_Man", i18n)
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:calc(100% - 60pt - 3px - 2pt);",
          shiny::uiOutput("v_maxdist_ui")
        )
      ),
      shiny::column(
        width = 6,
        shiny::div(ht("_v_nmax", i18n)),
        shiny::div(
          style="display:inline-block;vertical-align:middle;height:60px;padding-bottom:10px;",
          ""
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:60pt;",
          shinyWidgets::switchInput(
            "v_nmax_onoff", value = TRUE,
            size = "small", onLabel = ht("_Auto", i18n), offLabel = ht("_Man", i18n)
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:calc(100% - 60pt - 3px - 2pt);",
          shiny::uiOutput("v_nmax_ui")
        )
      )
    ),

    shiny::div(
      style="vertical-align:center;",
      shiny::strong(ph(ht("_samplesize_proc_onoff", i18n),"\u2000"))
    ),
    shiny::div(
      style="display:inline-block;vertical-align:middle;height:65px;padding-bottom:10px;",
      ""
    ),
    shiny::div(
      style="display:inline-block;vertical-align:middle;width:50pt;",
      shinyWidgets::switchInput(
        "samplesize_proc_onoff", value = FALSE,
        size = "small", onLabel = ht("_Yes", i18n), offLabel = ht("_No", i18n)
      )
    ),
    shiny::div(
      style="display:inline-block;vertical-align:middle;width:calc(100% - 50pt - 3px - 2pt);",
      shiny::uiOutput("samplesize_proc_ui")
    ),
    shinyjs::disabled(shiny::radioButtons(
      "samplescheme",
      shiny::span(
        style = "font-weight:normal;",
        ph(ht("_samplescheme", i18n),"\u2000")#,
        # actionLink("help_interp_overwrite", icon("question-circle"))
      ),
      choiceNames = ht(c(
        "_samplescheme_random", "_samplescheme_strat_npts",
        "_samplescheme_strat_area", "_samplescheme_strat_prop"
      ), i18n),
      choiceValues = c("casual", "strat_npts", "strat_area", "strat_prop"),
      selected = "strat_area"
    ))

  )

  # shiny::radioButtons(
  #   'interp_overwrite',
  #   shiny::span(
  #     ph(ht("_interp_overwrite", i18n),"\u2000")#,
  #     # actionLink("help_interp_overwrite", icon("question-circle"))
  #   ),
  #   ht(c("_Yes", "_No"), i18n),
  #   selected = TRUE,
  #   inline = TRUE
  # )

)
