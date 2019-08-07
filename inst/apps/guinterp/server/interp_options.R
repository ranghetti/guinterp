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

# Deactivate fit_vgm_button if auto_vgm
observeEvent(input$auto_vgm, {
  if (input$auto_vgm == "auto") {
    shinyjs::disable("fit_vgm_button")
  } else {
    shinyjs::enable("fit_vgm_button")
  }
})

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
