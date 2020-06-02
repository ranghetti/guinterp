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
observeEvent(c(input$semiauto_range, input$semiauto_autorange), {
  if (is.na(input$semiauto_range) & !input$semiauto_autorange) {
    shinyjs::disable("save_semiauto")
  } else {
    shinyjs::enable("save_semiauto")
  }
})

## Create (and optimise) manual variogram
observe({
  shiny::req(rv$inputpts_sf, input$interp_sampling)
  indata_sel <- rv$inputpts_sf[rv$inputpts_sf$sid<=input$interp_sampling,]
  rv$v.man <- gstat::vgm(psill=input$sill-input$nugget, model=input$model, range=input$range, nugget=input$nugget)
  v_formula <- if (length(unique(indata_sel$idfield)) > 1) {selvar ~ idfield} else {selvar ~ 1}
  rv$v <- gstat::variogram(v_formula, indata_sel, cutoff=input$vgm_cutoff)
  # rv$autofit_vgm <- sample(1E6, 1)
})

# Histogram of selvar
output$v_plot <- renderPlot({
  shiny::req(rv$v, rv$v.man)
  plot(rv$v,rv$v.man)
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
      ht("_err_autofit_vgm_2", i18n)
    )})
  }
  updateNumericInput(session, "sill", value=sum(signif(v.auto[,"psill"],3)))
  updateNumericInput(session, "nugget", value=signif(v.auto[,"psill"],3)[1])
  updateNumericInput(session, "range", value=signif(v.auto[2,3],3))
})

observeEvent(list(input$semiauto_autorange, input$fit_vgm_button, rv$fit_vgm_launchgui), {
  shiny::req(!is.null(input$semiauto_autorange))
  if (input$semiauto_autorange) {
    shinyjs::disable("semiauto_range")
  } else {
    shinyjs::enable("semiauto_range")
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


## Dynamic widgets
output$maxptdist_ui <- shiny::renderUI({
  req(rv$inputpts_points, rv$borders_polygon)
  maxdist <- max(sapply(
    sf::st_geometry(st_transform_utm(rv$borders_polygon)),
    function(x) {with(as.list(sf::st_bbox(x)), sqrt((xmax-xmin)^2+(ymax-ymin)^2))}
  ))
  shiny::sliderInput(
    inputId="maxptdist", label=NULL,
    min = 0,
    max = ceiling(maxdist/10^floor(log10(maxdist)))*10^floor(log10(maxdist)),
    value = maxdist,
    sep = "",
    post = " m",
    step = 10^(floor(log10(maxdist))-2)
  )
})

