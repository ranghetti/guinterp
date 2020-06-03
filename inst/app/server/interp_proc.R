#   ____________________________________________________________________________
#   Observer used for widgets relatives to processing options            ####

## Output raster path ####
shinyFiles::shinyFileSave(input, "path_outraster_sel", roots = volumes, session = session)
# if paths change after using the shinyDirButton, update the values and the textInput
observeEvent(input$path_outraster_sel, {
  path_outraster_string <- as.character(
    shinyFiles::parseSavePath(volumes, input$path_outraster_sel)$datapath
  )
  updateTextInput(session, "path_outraster_textin", value = path_outraster_string)
})
# get interp_dir
# TODO add here checks on interp_dir
observeEvent(input$path_outraster_textin, {
  outraster_path <- if (input$path_outraster_textin == "") {
    file.path(tempdir(), strftime(Sys.time(), "interp_%Y%m%d%H%M%S.tif"))
  } else {
    input$path_outraster_textin
  }
  rv$outraster_path <- if (grepl("\\.tif$",outraster_path)) {
    outraster_path
  } else {
    paste0(outraster_path,".tif")
  }
})

## Demo mode
if (getShinyOption("demo_mode") == TRUE) {
  shinyjs::disable("path_outraster_textin")
  shinyjs::disable("path_outraster_sel")
}


# Error messages
shiny::observeEvent(input$path_outraster_textin, {
  output$path_outraster_errormess <- path_check(dirname(input$path_outraster_textin))
})


## Output dir ####
shinyFiles::shinyDirChoose(input, "path_outdir_sel", roots = volumes)
# if paths change after using the shinyDirButton, update the values and the textInput
observeEvent(input$path_outdir_sel, {
  path_outdir_string <- shinyFiles::parseDirPath(volumes, input$path_outdir_sel)
  updateTextInput(session, "path_outdir_textin", value = path_outdir_string)
})
# get interp_dir
# TODO add here checks on interp_dir
observeEvent(c(input$path_outdir_textin, input$outraster_savesingles), {
  rv$interp_dir <- if (input$path_outdir_textin == "" | !input$outraster_savesingles) {
    file.path(tempdir(), strftime(Sys.time(), "interp_%Y%m%d%H%M%S"))
  } else {
    input$path_outdir_textin
  }
})

# Error messages
shiny::observeEvent(input$path_outdir_textin, {
  output$path_outdir_errormess <- path_check(input$path_outdir_textin)
})

# Hide the whole option of only a single polygon is interpolated
observeEvent(rv$inputpts_points, {
  if (length(unique(rv$inputpts_points$idfield)) <=1 ) {
    updateCheckboxInput(session, "outraster_savesingles", value = FALSE)
    shinyjs::hide("outraster_savesingles")
  } else {
    shinyjs::show("outraster_savesingles")
  }
})


## Advanced controls ####

## v_nmax
output$v_nmax_ui <- renderUI({
  req(rv$inputpts_points)
  numericInput(
    inputId="v_nmax",
    label=NULL,
    min = min(10, nrow(rv$inputpts_points)),
    max = nrow(rv$inputpts_points),
    value = min(500, nrow(rv$inputpts_points))
  )
})

# v_nmax_onoff
observeEvent(c(input$v_nmax_onoff, input$v_nmax), ignoreInit = FALSE, ignoreNULL = FALSE, {
  req(rv$inputpts_points)
  if (!input$v_nmax_onoff & !is.null(rv$inputpts_points)) {
    shinyjs::enable("v_nmax")
  } else {
    shinyjs::disable("v_nmax")
    updateNumericInput(session, "v_nmax", value = 500)
  }
})

## v_maxdist
output$v_maxdist_ui <- renderUI({
  req(rv$borders_polygon)
  max_border_diagonal <- sapply(unique(rv$borders_polygon$id_geom), function(i) {
    rv$borders_polygon[rv$borders_polygon$id_geom==i,] %>%
      st_transform_utm() %>%
      st_bbox() %>% as.list() %>%
      with(c((xmax-xmin)^2, (ymax-ymin)^2)) %>%
      sum() %>% sqrt()
  }) %>% max()
  numericInput(
    inputId="v_maxdist",
    label=NULL,
    min = 0,
    max = ceiling(max_border_diagonal),
    value = ceiling(max_border_diagonal/5),
    step = 1
  )
})

# v_maxdist_onoff
observeEvent(c(input$v_maxdist_onoff, input$v_maxdist), ignoreInit = FALSE, ignoreNULL = FALSE, {
  req(rv$borders_polygon)
  if (!input$v_maxdist_onoff & !is.null(rv$inputpts_points)) {
    shinyjs::show("v_maxdist")
  } else {
    shinyjs::hide("v_maxdist")
    # updateNumericInput(session, "v_maxdist", value = "auto")
  }
})

# samplesize_proc
observeEvent(c(rv$borders_polygon, input$samplesize_proc_onoff), {
  req(rv$borders_polygon, input$v_options_onoff)
  if (input$samplesize_proc_onoff) {
    shinyjs::enable("samplesize_proc")
    shinyjs::enable("samplescheme")
  } else {
    shinyjs::disable("samplesize_proc")
    shinyjs::disable("samplescheme")
  }
})
observeEvent(rv$new_inputs, {
  req(rv$inputpts_points)
  if (length(unique(rv$inputpts_points$idfield)) > 1) {
    shinyjs::show("samplescheme")
  } else {
    shinyjs::hide("samplescheme")
  }
})
output$samplesize_proc_ui <- shiny::renderUI({
  req(rv$inputpts_points)
  shinyjs::disabled(shiny::sliderInput(
    inputId = "samplesize_proc", label = NULL,
    min = 1, max = nrow(rv$inputpts_points),
    value = min(nrow(rv$inputpts_points), 1E4)
  ))
})


