#   ____________________________________________________________________________
#   Observers used to update the map during interpolation             ####

# Update the colour palette
colourPal <- function(colourData, selvariable, na.colour = NA) {
  if (nrow(colourData) == 0) {
    colorFactor("RdYlGn", levels = 1)
  } else {
  colorBin(
    "RdYlGn",
    range(colourData[[selvariable]], na.rm = TRUE),
    # bins: if 3*95% percentile > max, use a custom scale
    # (7 linear bins 0-95% percentiles, one from 95% to max)
    bins = if (3*quantile(colourData[[selvariable]], .95, na.rm = TRUE) >
               max(colourData[[selvariable]], na.rm = TRUE)) {
      8
    } else {
      c(seq(
        min(colourData[[selvariable]],na.rm = TRUE),
        quantile(colourData[[selvariable]], .95, na.rm = TRUE),
        length.out = 8
      )[-8], max(colourData[[selvariable]], na.rm = TRUE))
    },
    na.color = na.colour,
    # bins = round(quantile(colourData[[selvariable]], seq(0,1,.1), na.rm=TRUE), 1),
    pretty = TRUE
  )
  }
}


observeEvent(c(rv$inputpts_points, rv$borders_polygon), {
  req(rv$inputpts_points, rv$borders_polygon)
  rv$base_interp_map <- leaflet::leaflet() %>%
    addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
             group = i18n$t("_mapgroup_ortophoto")) %>%
    addTiles("https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_only_labels/{z}/{x}/{y}.png",
             group = i18n$t("_mapgroup_ortophoto")) %>%
    # addTiles(group = i18n$t("_mapgroup_map")) %>%
    addTiles("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
             group = i18n$t("_mapgroup_map")) %>%
    leaflet::addPolygons(
      data         = rv$borders_polygon,
      color        = "#bfbfbf", fill = NA, weight = 2.5,
      label        = ~id_geom,
      labelOptions = labelOptions(
        textOnly  = TRUE, direction = 'top', #permanent  = TRUE,
        style      = list("color" = "white", "font-weight" = "bold")
      )
    ) %>%
    leaflet::fitBounds(
      lng1 = min(rv$inputpts_points$lon),
      lat1 = min(rv$inputpts_points$lat),
      lng2 = max(rv$inputpts_points$lon),
      lat2 = max(rv$inputpts_points$lat)
    ) %>%
    leaflet::addLayersControl(
      baseGroups    = c(i18n$t("_mapgroup_ortophoto"),i18n$t("_mapgroup_map")),
      overlayGroups = c(i18n$t("_mapgroup_points"),i18n$t("_mapgroup_raster")),
      options       = layersControlOptions(collapsed = TRUE)
    ) %>%
    leaflet::showGroup(i18n$t("_mapgroup_points")) %>%
    leaflet::hideGroup(i18n$t("_mapgroup_raster"))
})
output$interp_map <- leaflet::renderLeaflet(rv$base_interp_map)


# Update the map when filters are changed
observeEvent(c(rv$change_interp, map_selvariable, rv$borders_polygon), { # LEAVE OBSERVEEVENT!
  # observe is not reactive for rv$change_interp, so it does not work properly.
  # To make this code reactive for other variables, add these ones to
  # observeevent's vecto, or change rv$change_interp where needed.
  # samplesize <- 1E3 #nrow(rv$inputpts_points) # TODO use a limit
  req(rv$inputpts_points, rv$borders_polygon)
  # Change the colour scale when filters or variable are changed
  rv$pal <- colourPal(
    rv$inputpts_points[sid < samplesize & filter == FALSE,],
    map_selvariable,
    na.colour = NA
  )

  leaflet::leafletProxy("interp_map") %>%
    # leaflet::clearShapes() %>%
    leaflet::removeShape(paste0("pts_", rv$inputpts_points$sid)) %>%
    leaflet::removeShape(paste0("brd_buf_", rv$borders_polygon$id_geom))
  if (nrow(rv$inputpts_points[sid < samplesize & filter==TRUE,]) > 0) {
    leaflet::addCircleMarkers(
      leaflet::leafletProxy("interp_map"),
        ~lon, ~lat,
        data         = rv$inputpts_points[sid < samplesize & filter==TRUE,],
        layerId      = paste0("pts_",rv$inputpts_points[sid < samplesize & filter == TRUE, ]$sid),
        radius       = 3, stroke = FALSE, fillOpacity = 0.4, fillColor = "cyan",
        label        = ~format(selvar, digits = 0,nsmall = 1),
        group        = i18n$t("_mapgroup_points"),
        labelOptions = labelOptions(style = list("background-color" = "#FFCCCC"))
      )
  }
  if (nrow(rv$inputpts_points[sid < samplesize & filter==FALSE,]) > 0) {
    leaflet::addCircleMarkers(
      leaflet::leafletProxy("interp_map"),
      ~lon, ~lat,
      data      = rv$inputpts_points[sid < samplesize & filter == FALSE,],
      layerId   = paste0("pts_", rv$inputpts_points[sid < samplesize & filter == FALSE,]$sid),
      radius    = 5, stroke = FALSE, fillOpacity = 0.65,
      fillColor = ~rv$pal(rv$inputpts_points[sid < samplesize & filter == FALSE,][[map_selvariable]]),
      label     = ~format(selvar, digits = 0, nsmall = 1),
      group     = i18n$t("_mapgroup_points"),
      labelOptions = labelOptions(style = list("background-color" = "#CCFFCC"))
    ) %>%
      leaflet::addLegend(
        "bottomleft", pal = rv$pal,
        values = rv$inputpts_points[sid < samplesize & filter == FALSE,],
        layerId = "colorLegend",
        title = switch(
          map_selvariable,
          selvar = NULL # it can be removed
        )
      )
  }

  if (input$check_pos & input$filter_buttons == "manual") {
    req(input$pos)
    leaflet::leafletProxy("interp_map") %>%
      leaflet::addPolygons(
        layerId = paste0("brd_buf_", rv$borders_polygon$id_geom),
        data = st_buffer_m(rv$borders_polygon, -input$pos),
        color = "#bfbfbf", fill = NA, weight = 2, dashArray = "8,6"
      )
  }

  if (!is.null(rv$interp_merged)) {
    leaflet::leafletProxy("interp_map") %>%
      # leaflet::removeImage("interp_raster")
      leaflet::clearImages()
    if (map_selvariable == "selvar") {
      leaflet::leafletProxy("interp_map") %>%
        leaflet::addRasterImage(
          # layerId = "interp_raster",
          x      = rv$interp_merged,
          colors = rv$pal, opacity = 1,
          group  = i18n$t("_mapgroup_raster"),
        )
    }
  }

})

# Update the map when raster are generated
observeEvent(rv$new_interpolation, {
  req(rv$interp_merged)

  leaflet::leafletProxy("interp_map") %>%
    leaflet::clearImages() %>%
    # leaflet::removeImage("interp_raster") %>%
    leaflet::addRasterImage(
      # layerId = "interp_raster",
      x      = rv$interp_merged,
      colors = rv$pal, opacity = 1,
      group  = i18n$t("_mapgroup_raster"),
    ) %>%
    leaflet::hideGroup(i18n$t("_mapgroup_points")) %>%
    leaflet::showGroup(i18n$t("_mapgroup_raster"))
})

observeEvent(rv$new_inputs, {
  req(rv$inputpts_points)
  # workaround to show points
  shiny::updateSelectInput(session, 'filter_buttons', selected = "no")
  delay(500, shiny::updateSelectInput(session, 'filter_buttons', selected = "auto"))
})

