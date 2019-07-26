#   ____________________________________________________________________________
#   Observers used to update the map during yield interpolation             ####

# check if the manual vgm were defined (if required)
observeEvent(input$interp_button, ignoreInit = TRUE, ignoreNULL = TRUE, {

  # Check if manual VGM still has to be defined
  if (input$interp_method == "krige" & input$auto_vgm == "manual" & is.null(rv$vgm.fit)) {
    rv$fit_vgm_launchgui <- paste0("autoopen_",sample(1E6,1)) # dummy var to open the modaldialog
  } else if (input$interp_method == "krige" & input$auto_vgm == "semiauto" & is.null(rv$vgm.semiauto)) {
    rv$fit_vgm_launchgui <- paste0("autoopen_",sample(1E6,1)) # dummy var to open the modaldialog
  } else {
    rv$interp_canbelaunched <- sample(1E6,1) # dummy var to continue the interpolation
  }
})

# If so, launch the processing
observeEvent(rv$interp_canbelaunched, ignoreInit = TRUE, ignoreNULL = TRUE, {

  # Open waiting message
  show_modal_message(
    shiny::div(
      shiny::p(
        "L'interpolazione dei dati \u00E8 in corso,",tags$br(),
        "attendere prego..."
      ),
      shinyWidgets::progressBar(id = "pb_interp", value = 0, striped = TRUE)
    ),
    title = "Caricamento dati di resa"
  )


  # Create the file name according to naming conventions ----
  dir.create(rv$interp_dir, showWarnings=FALSE)

  rv$out_raster_paths <- guinterp_process(
    rv$inputpts_points, rv$borders_polygon,
    grid_path = NA, filtered = TRUE,
    id_fieldname="id_geom", # TODO add GUI input for it
    interp_dir = rv$interp_dir,
    # samplesize = if (input$interp_method=="krige") {1E4} else {Inf},
    samplesize = Inf,
    parallel = (input$turbo == "high"),
    interp_method = input$interp_method,
    interp_res = input$interp_res,
    out_crs = rv$outproj_validated,
    buffer_radius = if (input$maxptdist_onoff) {input$maxptdist} else {Inf},
    vgm = switch(
      input$auto_vgm,
      auto = NA,
      semiauto = rv$vgm.semiauto,
      manual = rv$vgm.fit
    ),
    merge = FALSE,
    overwrite = input$interp_overwrite,
    .shiny_session = session,
    .shiny_pbar_id = "pb_interp"
  )


  # Create a merged raster to be shown
  rv$interp_merged <- if (length(rv$out_raster_paths) > 1) {
    do.call(raster::merge, lapply(rv$out_raster_paths, raster::raster))
  } else {
    lapply(rv$out_raster_paths, raster::raster)[[1]]
  }
  # Change the variable shown
  shinyWidgets::updateRadioGroupButtons(
    session,
    "map_selvariable",
    selected = "selvar"
  )

  # Remove dummy variables used to launch the processing / open a modal
  rv$vgm.fit <- rv$vgm.auto <- NULL
  rv$fit_vgm_launchgui <- NULL
  rv$interp_canbelaunched <- NULL

  # update dummy variable (used to update the map)
  rv$new_interpolation <- if (!is.null(rv$new_interpolation)) {
    rv$new_interpolation + 1
  } else {
    1
  }

  removeModal()

  # shinyWidgets::sendSweetAlert(
  #   session, title = "Interpolazione completata",
  #   text = "I dati di resa sono stati correttamente interpolati.",
  #   type = "success", btn_labels = "Ok"
  # )

})

