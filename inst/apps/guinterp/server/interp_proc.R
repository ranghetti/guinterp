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
