# Modal dialog for variogram definition and semiauto interpolation options

# Server functions for fit_vgm_gui
shiny::observeEvent(input$fit_vgm_button, ignoreInit = TRUE, ignoreNULL = TRUE, {
  rv$fit_vgm_launchgui <- paste0("manualopen_",sample(1E6,1)) # dummy var to open the modaldialog
})

shiny::observeEvent(rv$fit_vgm_launchgui, ignoreInit = TRUE, ignoreNULL = TRUE, {
  shiny::req(rv$inputpts_points, rv$borders_polygon)

  ## ModalDialog for interpolation options
  if (input$auto_vgm == "manual") {
    rv$inputpts_sf <- inputpts_to_sf(
      rv$inputpts_points,
      outcrs = st_crs_utm_from_lonlat(
        lon = rv$inputpts_points[,mean(lon,na.rm=TRUE)],
        lat = rv$inputpts_points[,mean(lat,na.rm=TRUE)]
      )
    )
    rv$max_diagonal <- sapply(unique(rv$inputpts_sf$idfield), function(i) {
      rv$inputpts_sf[rv$inputpts_sf$idfield==i,] %>%
        st_bbox() %>% as.list() %>%
        with(c((xmax-xmin)^2, (ymax-ymin)^2)) %>%
        sum() %>% sqrt()
    }) %>% max()
    fit_vgm_gui <- shiny::modalDialog(
      size = "l",
      shinydashboard::box(
        status = "primary", width = NULL, solidHeader = TRUE,
        title = ht("_modal_variogram_title", i18n),

        shiny::fluidRow(

          shiny::column(
            width=3,
            radioButtons(
              "model", label = ht("_modeltype", i18n),
              choiceValues = c("Exp", "Sph", "Gau"), #"Wav"
              choiceNames = ht(c("_modeltype_exp", "_modeltype_sph", "_modeltype_gau"), i18n),
              selected = if (is.null(shiny::isolate(input$model))) {"Exp"} else {shiny::isolate(input$model)}
            ),
            shiny::div(
              style = "margin-bottom:0.75em;",
              shiny::strong(ht("_variogram_param_label", i18n))
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:30pt;",
              ht("_variogram_sill", i18n)
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:calc(100% - 30pt - 13px);padding-left:10px;margin-bottom:-5px;",
              numericInput(
                "sill", label = NULL,
                value = if (is.null(shiny::isolate(input$sill))) {
                  # default value: data variance
                  var(rv$inputpts_points$selvar)
                } else {
                  shiny::isolate(input$sill)
                },
                min = 0
              )
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:30pt;",
              ht("_variogram_nugget", i18n)
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:calc(100% - 30pt - 13px);padding-left:10px;margin-bottom:-5px;",
              numericInput(
                "nugget", label = NULL,
                value = if (is.null(shiny::isolate(input$nugget))) {
                  # default value: 1/5 of the sill
                  var(rv$inputpts_points$selvar)/5
                } else {
                  shiny::isolate(input$nugget)
                },
                min = 0
              )
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:30pt;",
              ht("_variogram_range", i18n)
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:calc(100% - 30pt - 13px);padding-left:10px;margin-bottom:-5px;",
              numericInput(
                "range", label = NULL,
                value = if (is.null(shiny::isolate(input$range))) {
                  # default value: 1/5 of the cutoff (~ 1/5 of 1/3 of the bbox diagonal)
                  rv$max_diagonal/3/5
                } else {
                  shiny::isolate(input$range)
                },
                min = 0
              )
            ),
            shiny::div(
              shiny::div(
                style = "display:inline-block;position:relative;width:calc(100% - 15pt - 13px);",
                actionButton("autofit_vgm", ht("_autofit_vgm", i18n), width = "100%")
              ),
              shiny::div(
                style = "display:inline-block;position:relative;padding-left:10px;",
                uiOutput("err_autofit_vgm_1")
              )
            )
          ),

          shiny::column(
            width=9,
            shiny::fluidRow(
              shiny::column(
                width=6,
                sliderInput(
                  inputId = "interp_sampling",
                  label = ht("_interp_sampling", i18n),
                  min = 0,
                  max = ceiling(nrow(rv$inputpts_sf)),
                  value = if (is.null(shiny::isolate(input$interp_sampling))) {
                    min(1E4,ceiling(nrow(rv$inputpts_sf)))
                  } else {
                    shiny::isolate(input$interp_sampling)
                  },
                  step = 1
                )
              ),
              shiny::column(
                width=6,
                sliderInput(
                  inputId = "vgm_cutoff",
                  label = ht("_vgm_cutoff", i18n),
                  min = 10,
                  max = ceiling(rv$max_diagonal*2/3),
                  value = if (is.null(shiny::isolate(input$vgm_cutoff))) {
                    ceiling(rv$max_diagonal/3)
                  } else {
                    shiny::isolate(input$vgm_cutoff)
                  },
                  step = 1
                )
              )
            ),
            # shiny::fluidRow(
            plotOutput("v_plot",height="300px")
            # )
          )

        ),

        shiny::fluidRow(shiny::column(width = 12, uiOutput("err_autofit_vgm_2")))

      ),
      footer = shiny::div(
        actionButton(
          "save_vgm",
          ph("\u2004",ht("_Save", i18n)),
          icon = shiny::icon("check")
        )#,
        # modalButton(ph("\u2004",ht("_Exit", i18n)), icon = shiny::icon("ban")) # TODO re-activate,
        # restoring settings set before editing them
      )

    )
    shiny::showModal(fit_vgm_gui)
    shinyjs::delay(500, {
      rv$autofit_vgm <- sample(1E6, 1)
    })
  }

  ## ModalDialog for semiautomatic interpolation options
  if (input$auto_vgm == "semiauto") {
    fit_semiauto_gui <- shiny::modalDialog(
      size = "s",
      shinydashboard::box(
        status = "primary", width = NULL, solidHeader = TRUE,
        title = ht("_semiauto_title", i18n),
        radioButtons(
          "semiauto_model", label = ht("_modeltype", i18n),
          choiceValues = c("Exp", "Sph", "Gau"), #"Wav"
          choiceNames = ht(c("_modeltype_exp", "_modeltype_sph", "_modeltype_gau"), i18n),
          selected = if (is.null(shiny::isolate(input$semiauto_model))) {"Exp"} else {shiny::isolate(input$semiauto_model)}
        ),
        # numericInput("semiauto_range", label = shiny::span(shiny::em("Range"), "del variogramma"), value = 50)
        shiny::div(
          style="vertical-align:center;",
          shiny::strong(ht("_semiauto_autorange", i18n))
        ),
        # shiny::div(
        #   style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        #   ""
        # ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:55pt;",
          shinyWidgets::switchInput(
            "semiauto_autorange",
            value = if (is.null(shiny::isolate(input$semiauto_autorange))) {TRUE} else {shiny::isolate(input$semiauto_autorange)},
            size = "small", onLabel = ht("_Auto", i18n), offLabel = ht("_Man", i18n)
          )
        ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;padding-left:10px;width:calc(100% - 55pt - 3px - 2pt);",
          numericInput(
            "semiauto_range", label = NULL,
            value = if (is.null(shiny::isolate(input$semiauto_range))) {
              NA
            } else {
              shiny::isolate(input$semiauto_range)
            },
            min = 0
          )
        )
      ),
      footer = shinyjs::disabled(actionButton(
        "save_semiauto", ph("\u2004",ht("_Save", i18n)),
        icon = shiny::icon("check")
      ))
    )
    shiny::showModal(fit_semiauto_gui)
  }

})
