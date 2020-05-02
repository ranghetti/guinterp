#   ____________________________________________________________________________
#   Observer used for widgets relatives to sample size to visualise         ####

output$samplesize_view_ui <- shiny::renderUI({
  req(rv$inputpts_points)
  shiny::sliderInput(
    inputId = "samplesize_view", label = ht("_samplesize_view", i18n),
    min = 0, max = nrow(rv$inputpts_points),
    value = min(nrow(rv$inputpts_points), 1E4)
  )
})

observeEvent(input$samplesize_view, {
  req(rv$inputpts_points)
  rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
})
