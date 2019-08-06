#   ____________________________________________________________________________
#   Observer used for widgets relatives to interpolation options            ####







#### Server ####





#output$err_autofit_vgm <- renderUI({" "})


# Histogram of selvar
output$v_plot <- renderPlot({
  shiny::req(rv$v, rv$v.man)
  plot(rv$v,rv$v.man)
})



## Path checks




