#   ____________________________________________________________________________
#   Observer used for widgets relatives to interpolation options            ####

## Max point dist
observeEvent(list(input$maxptdist_onoff), {
  shiny::req(rv$inputpts_points)
  if (input$maxptdist_onoff & !is.null(rv$inputpts_points)) {
    shinyjs::enable("maxptdist")
  } else {
    shinyjs::disable("maxptdist")
  }
})

# Default output projection
output$out_proj_textinput <- renderUI({
  shiny::req(rv$inputpts_points)
  textInput(
    "out_proj", NULL,
    value = st_crs_utm_from_lonlat(
      lon = rv$inputpts_points[,mean(lon,na.rm=TRUE)],
      lat = rv$inputpts_points[,mean(lat,na.rm=TRUE)]
    )$epsg
  )
})


# check CRS
output$outproj_message <- renderUI({
  req(input$out_proj)
  # if required, take from reference raster
  rv$outproj_validated <- tryCatch(
    st_crs2(input$out_proj),
    error = function(e) {sf::st_crs(NA)}
  )$proj4string
  if (input$out_proj=="") {
    ""
  }  else if (is.na(rv$outproj_validated)) {
    span(style="color:red", "\u2718")
  } else {
    span(style="color:darkgreen", "\u2714")
  }
})


# Change polygon extension if it was set as the bbox of the points
observeEvent(rv$outproj_validated, {
  req(rv$outproj_validated)
  if (input$border_type == "bbox") {
    rv$borders_polygon <- inputpts_to_sf(rv$inputpts_points, outcrs = rv$outproj_validated, all = TRUE) %>%
      st_bbox() %>%
      st_as_sfc() %>%
      st_buffer_m(input$bbox_buffer) %>%
      st_bbox() %>% st_as_sfc() %>% sf::st_sf() %>%
      dplyr::transmute(id_geom = 0) %>%
      dplyr::group_by(id_geom) %>% dplyr::summarise() %>%
      sf::st_transform(4326)
  }
})


# Deactivate output CRS and resolution 1) if some raster exists, or
# 2) if overwrite is TRUE and all the existing rasters would be overwritten
observeEvent(c(rv$on_interp, rv$new_interpolation, input$interp_overwrite), {
  shiny::req(rv$inputpts_points, rv$borders_polygon)
  ex_raster_list <- list.files(rv$interp_dir, "\\.tif$", full.names = TRUE)
  ex_idfield <- gsub("^.+\\_([a-zA-Z0-9]+)\\.tif$","\\1",ex_raster_list) # id_geom of existing raster
  req_idfield <- unique(rv$inputpts_points$idfield) # id_geom of points to be interpolated
  if (length(ex_raster_list) > 0 & (input$interp_overwrite == FALSE | any(!ex_idfield %in% req_idfield))) {
    ex_meta <- rgdal::GDALinfo(ex_raster_list[1])
    ex_res <- mean(ex_meta[c("res.x","res.y")]) # use mean (res.x and res.y should be equal)
    ex_crs <- st_crs2(attr(ex_meta,"projection"))
    ex_crs <- if (is.na(ex_crs$epsg)) {ex_crs$proj4string} else {ex_crs$epsg}
    shiny::updateTextInput(session, "out_proj", value = ex_crs)
    shiny::updateNumericInput(session, "interp_res", value = ex_res)
    shinyjs::disable("out_proj")
    shinyjs::disable("interp_res")
    shinyjs::show("help_opts_disabled")
  } else {
    shinyjs::enable("out_proj")
    shinyjs::enable("interp_res")
    shinyjs::hide("help_opts_disabled")
  }
})


# Deactivate fit_vgm_button if auto_vgm
observeEvent(input$auto_vgm, {
  if (input$auto_vgm == "auto") {
    shinyjs::disable("fit_vgm_button")
  } else {
    shinyjs::enable("fit_vgm_button")
  }
})


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
        title = "Impostazioni di interpolazione",

        shiny::fluidRow(

          shiny::column(
            width=3,
            radioButtons(
              "model", label = "Tipo di modello",
              choices = list(
                "Esponenziale"="Exp",
                "Sferoidale"="Sph",
                "Gaussiano"="Gau",
                "Wavelength"="Wav"
              ),
              selected = if (is.null(shiny::isolate(input$model))) {"Exp"} else {shiny::isolate(input$model)}
            ),
            shiny::div(
              style = "margin-bottom:0.75em;",
              shiny::strong("Parametri del variogramma")
            ),
            shiny::div(
              style = "display:inline-block;position:relative;width:30pt;",
              shiny::em("Sill:")
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
              shiny::em("Nugget:")
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
              shiny::em("Range:")
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
            # numericInput("sill", label = shiny::em("Sill"), value = 1),
            # numericInput("nugget", label = shiny::em("Nugget"), value = 0),
            # numericInput("range", label = shiny::em("Range"), value = 100),
            shiny::div(
              shiny::div(
                style = "display:inline-block;position:relative;width:calc(100% - 15pt - 13px);",
                actionButton("autofit_vgm", "Ottimizza", width = "100%")
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
                  inputId="interp_sampling",
                  label="Punti da utilizzare:",
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
                  inputId="vgm_cutoff",
                  label="Massima distanza:",
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
        actionButton("save_vgm", "\u2004Salva", icon = shiny::icon("check"))#,
        # modalButton("\u2004Esci", icon = shiny::icon("ban")) # TODO riattivalo,
        # andando a ripristinare le impsotazioni prima della modifica
      )

    )
    shiny::showModal(fit_vgm_gui)
  }

  ## ModalDialog for semiautomatic interpolation options
  if (input$auto_vgm == "semiauto") {
    fit_semiauto_gui <- shiny::modalDialog(
      size = "s",
      shinydashboard::box(
        status = "primary", width = NULL, solidHeader = TRUE,
        title = "Impostazioni",
        radioButtons(
          "semiauto_model", label = "Tipo di modello",
          choices = list(
            "Esponenziale"="Exp",
            "Sferoidale"="Sph",
            "Gaussiano"="Gau",
            "Wavelength"="Wav"
          ),
          selected = if (is.null(shiny::isolate(input$semiauto_model))) {"Exp"} else {shiny::isolate(input$semiauto_model)}
        ),
        # numericInput("semiauto_range", label = shiny::span(shiny::em("Range"), "del variogramma"), value = 50)
        shiny::div(
          style="vertical-align:center;",
          shiny::strong(shiny::em("Range"), "del variogramma")
        ),
        # shiny::div(
        #   style="display:inline-block;vertical-align:middle;height:80px;padding-bottom:10px;",
        #   ""
        # ),
        shiny::div(
          style="display:inline-block;vertical-align:middle;width:55pt;",
          shinyWidgets::switchInput(
            "semiauto_autorange",
            value = if (is.null(shiny::isolate(input$semiauto_autorange))) {FALSE} else {shiny::isolate(input$semiauto_autorange)},
            size = "small", onLabel = "Auto", offLabel = "Set"
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
      footer = shinyjs::disabled(actionButton("save_semiauto", "\u2004Salva", icon = shiny::icon("check")))
    )
    shiny::showModal(fit_semiauto_gui)
  }

})


#### Server ####

## Activate semiauto ok button
observeEvent(input$semiauto_range, {
  if (is.na(input$semiauto_range)) {
    shinyjs::disable("save_semiauto")
  } else {
    shinyjs::enable("save_semiauto")
  }
})


## Create (and optimise) manual variogram
observe({
  shiny::req(rv$inputpts_sf, input$interp_sampling)
  indata_sel <- rv$inputpts_sf[rv$inputpts_sf$sid<=input$interp_sampling,]
  rv$v.man <- gstat::vgm(psill=input$sill, model=input$model, range=input$range, nugget=input$nugget)
  v_formula <- if (length(unique(indata_sel$idfield)) > 1) {selvar ~ idfield} else {selvar ~ 1}
  rv$v <- gstat::variogram(v_formula, indata_sel, cutoff=input$vgm_cutoff)
  rv$autofit_vgm <- sample(1E6, 1)
})

## Optimise manual variogram
observeEvent(input$autofit_vgm, {
  rv$autofit_vgm <- sample(1E6, 1)
})


#output$err_autofit_vgm <- renderUI({" "})

observeEvent(rv$autofit_vgm, {
  shiny::req(rv$inputpts_sf, rv$v, rv$v.man)
  # fit_vgm_warn <- tryCatch(v.auto <- fit.variogram(rv$v, rv$v.man), warning=function(w){"warning"})
  v.auto <- suppressWarnings(gstat::fit.variogram(rv$v, rv$v.man))
  if (is(tryCatch(gstat::fit.variogram(rv$v, rv$v.man),warning=function(w){"warning"}), "variogramModel")) {
    output$err_autofit_vgm_1 <- renderUI({shiny::span(style="color:darkgreen;",shiny::icon("check"))})
    output$err_autofit_vgm_2 <- renderUI({""})
  } else {
    output$err_autofit_vgm_1 <- renderUI({shiny::span(
      style="color:red;",
      shiny::icon("exclamation-triangle")
    )})
    output$err_autofit_vgm_2 <- renderUI({shiny::div(
      style="color:red;",
      "I punti non sono modellabili univocamente: controllare manualmente i parametri generati."
    )})
  }
  updateNumericInput(session, "sill", value=signif(v.auto[,"psill"],3)[2])
  updateNumericInput(session, "nugget", value=signif(v.auto[,"psill"],3)[1])
  updateNumericInput(session, "range", value=signif(v.auto[2,3],3))
})

output$vgm_sill <- renderUI({
  shiny::req(rv$inputpts_sf)
  sliderInput(
    inputId="interp_sampling",
    label="Number of points to use:",
    min = 0,
    max = ceiling(nrow(rv$inputpts_sf)),
    value = min(1E4,ceiling(nrow(rv$inputpts_sf))),
    step = 1
  )
})

# Histogram of selvar
output$v_plot <- renderPlot({
  shiny::req(rv$v, rv$v.man)
  plot(rv$v,rv$v.man)
})

observeEvent(list(input$semiauto_autorange, input$fit_vgm_button, rv$fit_vgm_launchgui), {
  shiny::req(input$semiauto_range)
  if (input$semiauto_autorange == FALSE) {
    shinyjs::enable("semiauto_range")
  } else {
    shinyjs::disable("semiauto_range")
  }
})

observeEvent(input$save_vgm, ignoreInit = TRUE, {
  shiny::req(rv$inputpts_sf, rv$v, rv$v.man)
  rv$vgm.fit <- rv$v.man
  attr(rv$vgm.fit,"cutoff") <- input$vgm_cutoff
  attr(rv$vgm.fit,"n_points") <- input$interop_sampling
  if (any(grepl("^autoopen",rv$fit_vgm_launchgui))) {
    rv$interp_canbelaunched <- sample(1E6,1) # dummy var to continue the interpolation
  }
  removeModal()
})

observeEvent(input$save_semiauto, ignoreInit = TRUE, {
  shiny::req(rv$inputpts_points, rv$borders_polygon)
  rv$vgm.semiauto <- NA
  if (input$semiauto_autorange == FALSE) {
    attr(rv$vgm.semiauto, "range") <- input$semiauto_range
  }
  attr(rv$vgm.semiauto, "model") <- input$semiauto_model
  if (any(grepl("^autoopen",rv$fit_vgm_launchgui))) {
    rv$interp_canbelaunched <- sample(1E6,1) # dummy var to continue the interpolation
  }
  removeModal()
})


## Path checks

# Error messages
shiny::observeEvent(input$path_out_textin, {
  output$path_out_errormess <- path_check(input$path_out_textin)
})

