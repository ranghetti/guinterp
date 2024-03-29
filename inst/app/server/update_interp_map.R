#   ____________________________________________________________________________
#   Observers used to update the map during interpolation             ####

# Samplesize
output$samplesize_map_ui <- shiny::renderUI({
  req(rv$inputpts_points)
  shiny::sliderInput(
    inputId = "samplesize_map", label = ht("_samplesize_map", i18n),
    min = 0, max = nrow(rv$inputpts_points),
    value = min(nrow(rv$inputpts_points), 1E4)
  )
})
# observeEvent(input$samplesize_map, {
#   req(rv$inputpts_points)
#   rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
# })


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
      data = rv$borders_polygon,
      color = "#bfbfbf", fill = NA, weight = 2.5,
      label = ~id_geom,
      labelOptions = if (length(unique(rv$borders_polygon$id_geom))>1) {
        labelOptions(
          textOnly = TRUE, direction = 'center',
          permanent  = TRUE, textsize = "12px",
          style = list(
            "color" = "white", "font-weight" = "bold", "border" = "1px",
            "border-style" = "solid", "border-color" = "white",
            "background-color" = "rgba(0,0,0,.5)", "border-radius" = "5px"
          )
        )
      } else {
        labelOptions(
          textOnly = TRUE, direction = 'center',
          permanent  = FALSE, textsize = "6px",
          style = list("color" = "white", "font-weight" = "bold")
        )
      }
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
observeEvent(c(rv$change_interp, map_selvariable, rv$borders_polygon, input$samplesize_map), { # LEAVE OBSERVEEVENT!
  # observe is not reactive for rv$change_interp, so it does not work properly.
  # To make this code reactive for other variables, add these ones to
  # observeevent's vecto, or change rv$change_interp where needed.
  req(rv$inputpts_points, rv$borders_polygon, input$samplesize_map)
  # Change the colour scale when filters or variable are changed
  rv$pal <- colourPal(
    rv$inputpts_points[sid3 <= input$samplesize_map & filter == FALSE,],
    map_selvariable,
    na.colour = NA
  )

  leaflet::leafletProxy("interp_map") %>%
    # leaflet::clearShapes() %>%
    leaflet::removeMarker(paste0("pts_", rv$inputpts_points$uid)) %>%
    leaflet::removeMarker(paste0("brd_buf_", rv$borders_polygon$id_geom))
  if (nrow(rv$inputpts_points[sid3 <= input$samplesize_map & filter==TRUE,]) > 0) {
    leaflet::addCircleMarkers(
      leaflet::leafletProxy("interp_map"),
        ~lon, ~lat,
        data         = rv$inputpts_points[sid3 <= input$samplesize_map & filter==TRUE,],
        layerId      = paste0("pts_",rv$inputpts_points[sid3 <= input$samplesize_map & filter == TRUE,]$uid),
        radius       = 3, stroke = FALSE, fillOpacity = 0.4, fillColor = "cyan",
        label        = ~format(selvar, digits = 1, nsmall = 1),
        group        = i18n$t("_mapgroup_points"),
        labelOptions = labelOptions(style = list("background-color" = "#FFCCCC"))
      )
  }
  if (nrow(rv$inputpts_points[sid3 <= input$samplesize_map & filter==FALSE,]) > 0) {
    leaflet::addCircleMarkers(
      leaflet::leafletProxy("interp_map"),
      ~lon, ~lat,
      data      = rv$inputpts_points[sid3 <= input$samplesize_map & filter == FALSE,],
      layerId   = paste0("pts_", rv$inputpts_points[sid3 <= input$samplesize_map & filter == FALSE,]$uid),
      radius    = 5, stroke = FALSE, fillOpacity = 0.65,
      fillColor = ~rv$pal(rv$inputpts_points[sid3 <= input$samplesize_map & filter == FALSE,][[map_selvariable]]),
      label     = ~format(selvar, digits = 1, nsmall = 1),
      group     = i18n$t("_mapgroup_points"),
      labelOptions = labelOptions(style = list("background-color" = "#CCFFCC"))
    ) %>%
      leaflet::addLegend(
        "bottomleft", pal = rv$pal,
        values = rv$inputpts_points[sid3 <= input$samplesize_map & filter == FALSE,],
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

