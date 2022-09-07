observeEvent(input$selpts, ignoreInit = TRUE, {

  # use a copy of points (so not to show previous selpts filters)
  selpts_inputpts <- copy(rv$inputpts_points)
  filter_pts_reset(selpts_inputpts, "selpts")

  selpts_pal <- colourPal(
    selpts_inputpts[sid3 < Inf & filter == FALSE,],
    map_selvariable,
    na.colour = NA
  )

  selpts_map <- rv$base_interp_map
  if (nrow(selpts_inputpts[sid3 < Inf & filter==TRUE,]) > 0) {
    selpts_map <- leaflet::addCircleMarkers(
      selpts_map,
      ~lon, ~lat,
      data         = selpts_inputpts[sid3 < Inf & filter==TRUE,],
      layerId      = paste0("pts_",selpts_inputpts[sid3 < Inf & filter == TRUE, ]$uid),
      radius       = 3, stroke = TRUE, weight = 1, opacity = 1, color = "black", fillOpacity = 1,
      fillColor    = "cyan",
      label        = ~format(selvar, digits = 1, nsmall = 1),
      group        = i18n$t("_mapgroup_points"),
      labelOptions = labelOptions(style = list("background-color" = "#FFCCCC"))
    )
  }
  if (nrow(selpts_inputpts[sid3 < Inf & filter==FALSE,]) > 0) {
    selpts_map <- leaflet::addCircleMarkers(
      selpts_map,
      ~lon, ~lat,
      data      = selpts_inputpts[sid3 < Inf & filter == FALSE,],
      layerId   = paste0("pts_", selpts_inputpts[sid3 < Inf & filter == FALSE,]$uid),
      radius    = 5, stroke = TRUE, weight = 1, opacity = 1, color = "black", fillOpacity = 1,
      fillColor = ~selpts_pal(selpts_inputpts[sid3 < Inf & filter == FALSE,][[map_selvariable]]),
      label     = ~format(selvar, digits = 1, nsmall = 1),
      group     = i18n$t("_mapgroup_points"),
      labelOptions = labelOptions(style = list("background-color" = "#CCFFCC"))
    )
  }
  selpts_map <- leaflet::addLegend(
    selpts_map,
    "bottomright", pal = selpts_pal,
    values = selpts_inputpts[sid3 < Inf & filter == FALSE,],
    layerId = "colorLegend",
    title = switch(
      map_selvariable,
      selvar = NULL # it can be removed
    )
  )

  selpts_ns_name <- paste0("selpts_",sample(1E9,1))
  selpts_ns <- NS(selpts_ns_name)
  rv$selpts_interp <- shiny::callModule(
    mapedit::selectMod, selpts_ns_name, selpts_map,
    # styleFalse = list(stroke = TRUE, weight = 2, color = "red"),
    styleFalse = list(weight = 1, fillOpacity = 1),
    styleTrue = list(weight = 5, fillOpacity = 0)
  )

  shiny::showModal(shiny::modalDialog(
    size = "l",
    mapedit::selectModUI(selpts_ns_name, height = "650px"),
    shiny::div(
      style = "margin-top:15px;text-align:center;",
      shiny::radioButtons(
        "selpts_reverse", NULL,
        choiceNames = ht(c("_editmap_reverse_t", "_editmap_reverse_f"), i18n),
        choiceValues = c(TRUE, FALSE),
        selected = TRUE,
        inline = TRUE
      )
    ),
    footer = shiny::tagList(
      actionButton(
        "save_selpts", ph("\u2004",ht("_Save", i18n)),
        icon = shiny::icon("check")
      ),
      modalButton(ph("\u2004",ht("_Exit", i18n)), icon = shiny::icon("ban"))
    )
  ))

})

# Necessary to catch all the selected points
observe({
  req(rv$selpts_interp)
  invisible(rv$selpts_interp())
})

observeEvent(input$save_selpts, ignoreInit = TRUE, {
  rv$selpts_uids <- rv$inputpts_points[uid %in% as.integer(
    gsub("^pts_","",rv$selpts_interp()$id[rv$selpts_interp()$selected=="TRUE"])
  ), uid]
  shiny::removeModal()
})
