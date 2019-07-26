# File selector are managed here,
# so to be easily switchable between desktop and clientserver modes (not yet implemented)

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

# Read path
shinyFiles::shinyFileChoose(
  input, "sfb_importFilters", roots=volumes, session=session,
  filetypes = c("JSON"="json")
)
shiny::observeEvent(input$sfb_importFilters, {
  rv$importFilters <- shinyFiles::parseFilePaths(volumes,input$sfb_importFilters)
})

# Export filtering parameters
observe({
  shinyFiles::shinyFileSave(input, "downloadFilters", roots=volumes, session=session)
  export_param_path <- shinyFiles::parseSavePath(volumes, input$downloadFilters)
  if (nrow(export_param_path)>0) {
    filter_list <- list()
    if (input$check_rangey) {filter_list$rangey <- c(input$miny, input$maxy)}
    if (input$check_zscorey) {filter_list$zscorey <- input$zscorey}
    if (input$check_rbiasy) {filter_list$rbiasy <- input$rbiasy}
    if (input$check_rangeq) {filter_list$rangeq <- input$rangeq/100}
    if (input$check_pos) {filter_list$pos <- input$pos}
    write(
      jsonlite::toJSON(filter_list, pretty = TRUE),
      as.character(export_param_path$datapath)
    )
  }
})





