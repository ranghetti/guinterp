observeEvent(input$editmap, ignoreInit = TRUE, {

  # use a copy of points (so not to show previous editmap filters)
  editmap_inputpts <- copy(rv$inputpts_points)
  filter_pts_reset(editmap_inputpts, "editmap")

  editmap_map <- rv$base_interp_map
  if (nrow(editmap_inputpts[sid3 <= input$samplesize_map & filter==TRUE,]) > 0) {
    editmap_map <- leaflet::addCircleMarkers(
      editmap_map,
      ~lon, ~lat,
      data         = editmap_inputpts[sid3 <= input$samplesize_map & filter==TRUE,],
      layerId      = paste0("pts_",editmap_inputpts[sid3 <= input$samplesize_map & filter == TRUE, ]$uid),
      radius       = 3, stroke = FALSE, fillOpacity = 0.4, fillColor = "cyan",
      label        = ~format(selvar, digits = 0,nsmall = 1),
      group        = i18n$t("_mapgroup_points"),
      labelOptions = labelOptions(style = list("background-color" = "#FFCCCC"))
    )
  }
  if (nrow(editmap_inputpts[sid3 <= input$samplesize_map & filter==FALSE,]) > 0) {
    editmap_map <- leaflet::addCircleMarkers(
      editmap_map,
      ~lon, ~lat,
      data      = editmap_inputpts[sid3 <= input$samplesize_map & filter == FALSE,],
      layerId   = paste0("pts_", editmap_inputpts[sid3 <= input$samplesize_map & filter == FALSE,]$uid),
      radius    = 5, stroke = FALSE, fillOpacity = 0.65,
      fillColor = ~rv$pal(editmap_inputpts[sid3 <= input$samplesize_map & filter == FALSE,][[map_selvariable]]),
      label     = ~format(selvar, digits = 0, nsmall = 1),
      group     = i18n$t("_mapgroup_points"),
      labelOptions = labelOptions(style = list("background-color" = "#CCFFCC"))
    )
  }
  editmap_map <- leaflet::addLegend(
    editmap_map,
    "bottomright", pal = rv$pal,
    values = editmap_inputpts[sid3 <= input$samplesize_map & filter == FALSE,],
    layerId = "colorLegend",
    title = switch(
      map_selvariable,
      selvar = NULL # it can be removed
    )
  )

  editmap_ns_name <- paste0("editmap_",sample(1E9,1))
  editmap_ns <- NS(editmap_ns_name)
  rv$editmap_interp <- shiny::callModule(
    mapedit::editMod, editmap_ns_name, editmap_map,
    editor = "leafpm",
    record = TRUE,
    editorOptions = list(
      toolbarOptions = leafpm::pmToolbarOptions(drawMarker = FALSE, drawPolyline = FALSE),
      drawOptions = leafpm::pmDrawOptions(snappable = FALSE)
    )
  )

  shiny::showModal(shiny::modalDialog(
    size = "l",
    mapedit::editModUI(editmap_ns_name, height = "650px"),
    shiny::div(
      style = "margin-top:15px;text-align:center;",
      shiny::radioButtons(
        "editmap_reverse", NULL,
        choiceNames = ht(c("_editmap_reverse_t", "_editmap_reverse_f"), i18n),
        choiceValues = c(TRUE, FALSE),
        selected = TRUE,
        inline = TRUE
      )
    ),
    footer = shiny::tagList(
      actionButton(
        "save_editmap", ph("\u2004",ht("_Save", i18n)),
        icon = shiny::icon("check")
      ),
      modalButton(ph("\u2004",ht("_Exit", i18n)), icon = shiny::icon("ban"))
    )
  ))
})

observeEvent(input$save_editmap, ignoreInit = TRUE, {

  # convert points+radius to circles
  rv$editmap_geoms <- rv$editmap_interp()$finished
  if (is.null(rv$editmap_geoms)) {rv$editmap_geoms <- list()} # so to apply filter
  if (!is.null(rv$editmap_geoms$radius)) {
    rv$editmap_geoms$radius[is.na(rv$editmap_geoms$radius)] <- 0
    rv$editmap_geoms <- st_buffer_m(rv$editmap_geoms, 1.001 * rv$editmap_geoms$radius)
  }

  shiny::removeModal()

})
