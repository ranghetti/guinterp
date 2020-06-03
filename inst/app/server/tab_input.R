# Server functions for tab_input

observeEvent(c(rv$vectfile_path, rv$vectfields_path, input$rawformat), {
  if (all(sapply(list(rv$vectfile_path, rv$vectfields_path), is.data.frame)) |
      is.data.frame(rv$vectfile_path) & input$rawformat!="custom_ibf") {
    shinyjs::enable("load_button")
  } else {
    shinyjs::disable("load_button")
  }
})


#### Load borders and points ####
source(
  system.file("app/server/modal_loadinputpts.R", package="guinterp"),
  local=TRUE
)$value
source(
  system.file("app/server/modal_loadborders.R", package="guinterp"),
  local=TRUE
)$value
