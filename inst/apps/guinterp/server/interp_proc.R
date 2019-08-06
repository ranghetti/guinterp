#   ____________________________________________________________________________
#   Observer used for widgets relatives to processing options            ####

## Box: output ####
shinyFiles::shinyDirChoose(input, "path_out_sel", roots = volumes)
# if paths change after using the shinyDirButton, update the values and the textInput
observeEvent(input$path_out_sel, {
  path_out_string <- shinyFiles::parseDirPath(volumes, input$path_out_sel)
  updateTextInput(session, "path_out_textin", value = path_out_string)
})
# get interp_dir
# TODO add here checks on interp_dir
observeEvent(input$path_out_textin, {
  rv$interp_dir <- if (input$path_out_textin == "") {
    tempfile(pattern = strftime(Sys.time(), "interp_%Y%m%d%H%M%S_"))
  } else {
    input$path_out_textin
  }
})

# Error messages
shiny::observeEvent(input$path_out_textin, {
  output$path_out_errormess <- path_check(input$path_out_textin)
})

