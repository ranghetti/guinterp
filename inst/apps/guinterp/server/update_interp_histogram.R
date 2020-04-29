#   ____________________________________________________________________________
#   Observers used to update the histograms during interpolation      ####

# Histogram of selvar
observeEvent(rv$change_interp, {
  output$hist <- renderPlot({
    req(rv$inputpts_points, rv$borders_polygon)
    rv$hist_range  <- quantile(rv$inputpts_points$selvar, c(.005,.995), type=1)
    rv$hist_breaks <- hist(plot = FALSE, rv$inputpts_points[sid<=samplesize & selvar>=rv$hist_range[1] & selvar<=rv$hist_range[2],]$selvar, breaks=50)$breaks
    rv$hist_ylim   <- c(0,max(hist(plot = FALSE, rv$inputpts_points[sid<=samplesize & selvar>=rv$hist_range[1] & selvar<=rv$hist_range[2],]$selvar, breaks=50)$counts)) /
      nrow(rv$inputpts_points[sid<=samplesize & selvar>=rv$hist_range[1] & selvar<=rv$hist_range[2],])

    p <- ggplot2::ggplot(rv$inputpts_points, ggplot2::aes(x=selvar)) +
      ggplot2::geom_histogram(data=rv$inputpts_points[sid<=samplesize,], colour="white", aes(y = stat(count) / sum(count), fill=filter), breaks=rv$hist_breaks) +
      ggplot2::scale_fill_manual(values = c("FALSE"="darkgreen", "TRUE"="red")) +
      ggplot2::scale_y_continuous(labels = scales::percent, limits = rv$hist_ylim) +
      ggplot2::xlab(gsub("%v",input$select_inputvar,i18n$t("_hist_xlab"))) + ggplot2::ylab(i18n$t("_hist_ylab"))
    if (!is.null(input$miny) & !is.null(input$maxy) & input$check_rangey & input$filter_buttons == "manual") {
      p <- p + ggplot2::geom_vline(xintercept = c(input$miny, input$maxy), colour="red")
    }
    p
  })
})

