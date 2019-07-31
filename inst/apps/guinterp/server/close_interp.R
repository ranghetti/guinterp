# #### Observer used to close the interp tab and clean up the environment
observeEvent(input$close_interp_button, {
  # shinyjs::js$closeWindow()
  shinyjs::js$resetApp()
})

observeEvent(input$close_app_button, {
  shinyjs::js$closeWindow()
  # shinyjs::js$resetApp()
  stopApp()
})
