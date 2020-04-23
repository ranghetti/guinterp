#   ____________________________________________________________________________
#   Observer used for widgets relatives to output format options            ####

# Default output projection
output$out_proj_textinput <- renderUI({
  shiny::req(rv$inputpts_points)
  textInput(
    "out_proj",
    span(
      ph(ht("_out_proj", i18n),"\u2000"),
      shiny::actionLink("help_out_proj", icon("question-circle"))
    ),
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


## Define output grid basing on manual definition of res. and CR, or on the ref raster
observeEvent(c(input$outgrid_type, input$interp_res, rv$outproj_validated, input$path_refraster_textin), {
  if (input$outgrid_type == "custom") {
    req(input$interp_res, rv$outproj_validated)
    rv$interp_res <- input$interp_res
    rv$outproj <- rv$outproj_validated
    rv$grid_offset <- c("xmin" = 0, "ymin" = 0)
  } else if (input$outgrid_type == "ref") {
    req(input$path_refraster_textin)
    # Error messages
    path_refraster_errormess <- raster_check(input$path_refraster_textin)
    output$path_refraster_errormess <- path_refraster_errormess
    rv$path_refraster_isvalid <- attr(path_refraster_errormess, "isvalid")
    req(rv$path_refraster_isvalid)
    outgrid_raster <- stars::read_stars(input$path_refraster_textin, proxy = TRUE, quiet = TRUE)
    rv$interp_res <- stars::st_dimensions(outgrid_raster)[[1]]$delta # assuming same resolution in x and y
    rv$outproj <- sf::st_crs(outgrid_raster)$proj4string
    rv$grid_offset <- sf::st_bbox(outgrid_raster)[c("xmin","ymin")] %% rv$interp_res
  }
})
output$path_refraster_isvalid <- shiny::renderText({
  attr(rv$path_refraster_isvalid, "isvalid")
})
shiny::outputOptions(output, "path_refraster_isvalid", suspendWhenHidden = FALSE)


# # Deactivate output CRS and resolution 1) if some raster exists, or
# # 2) if overwrite is TRUE and all the existing rasters would be overwritten
# observeEvent(c(rv$on_interp, rv$new_interpolation, input$interp_overwrite), {
#   shiny::req(rv$inputpts_points, rv$borders_polygon)
#   ex_raster_list <- list.files(rv$interp_dir, "\\.tif$", full.names = TRUE)
#   ex_idfield <- gsub("^.+\\_([a-zA-Z0-9]+)\\.tif$","\\1",ex_raster_list) # id_geom of existing raster
#   req_idfield <- unique(rv$inputpts_points$idfield) # id_geom of points to be interpolated
#   if (length(ex_raster_list) > 0 & (input$interp_overwrite == FALSE | any(!ex_idfield %in% req_idfield))) {
#     ex_meta <- read_stars(ex_raster_list[1], proxy = TRUE)
#     ex_res <- st_dimensions(ex_meta)[[1]]$delta # assuming res. x is equal to res.y
#     ex_crs <- with(
#       st_crs(ex_meta),
#       if (is.na(epsg)) {proj4string} else {epsg}
#     )
#     # grid_offset: offset from (0,0) in the raster CRS (used to coregistrate grids)
#     rv$grid_offset <- st_bbox(ex_meta)[c("xmin","ymin")] %% ex_res
#     shiny::updateTextInput(session, "out_proj", value = ex_crs)
#     shiny::updateNumericInput(session, "interp_res", value = ex_res)
#     shinyjs::disable("out_proj")
#     shinyjs::disable("interp_res")
#     shinyjs::show("help_opts_disabled")
#   } else {
#     shinyjs::enable("out_proj")
#     shinyjs::enable("interp_res")
#     shinyjs::hide("help_opts_disabled")
#   }
# })

## Reference raster ####
shinyFiles::shinyFileChoose(input, "path_refraster_sel", roots = volumes, session = session)
# if paths change after using the shinyDirButton, update the values and the textInput
observeEvent(input$path_refraster_sel, {
  path_refraster_string <- shinyFiles::parseFilePaths(volumes, input$path_refraster_sel)$datapath
  updateTextInput(session, "path_refraster_textin", value = path_refraster_string)
})

