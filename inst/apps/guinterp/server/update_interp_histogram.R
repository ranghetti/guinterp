#   ____________________________________________________________________________
#   Observers used to update the statistics during interpolation            ####


# Select polygons
output$whichfield_ui <- shiny::renderUI({
  req(rv$inputpts_points, rv$borders_polygon)
  shinyWidgets::pickerInput(
    "whichfield", ht("_whichfield", i18n),
    choices = sort(unique(rv$inputpts_points$idfield)),
    selected = unique(rv$inputpts_points$idfield),
    options = list(
      `selected-text-format` = "count > 3",
      `live-search` = TRUE,
      `actions-box` = TRUE,
      title = ht("_whichfield_nothing", i18n)
    ),
    multiple = TRUE
  )
})


# Samplesize
output$samplesize_stats_ui <- shiny::renderUI({
  req(rv$inputpts_points)
  shiny::sliderInput(
    inputId = "samplesize_stats", label = ht("_samplesize_stats", i18n),
    min = 0, max = nrow(rv$inputpts_points[idfield %in% input$whichfield,]),
    value = min(nrow(rv$inputpts_points[idfield %in% input$whichfield,]), 1E4)
  )
})


# Histogram of selvar
observeEvent(c(rv$change_interp, input$whichfield, input$samplesize_stats), {

  req(rv$inputpts_points, rv$borders_polygon, input$samplesize_stats)
  inputpts_points_toview <- rv$inputpts_points[idfield %in% input$whichfield,]
  inputpts_points_toview <- inputpts_points_toview[frank(sid3)<=input$samplesize_stats,]

  output$hist <- shiny::renderPlot({
    req(nrow(inputpts_points_toview) > 0)
    rv$hist_range  <- quantile(inputpts_points_toview$selvar, c(.005,.995), type=1)
    rv$hist_breaks <- hist(plot = FALSE, inputpts_points_toview[selvar>=rv$hist_range[1] & selvar<=rv$hist_range[2],]$selvar, breaks=50)$breaks
    rv$hist_ylim   <- c(0,max(hist(plot = FALSE, inputpts_points_toview[selvar>=rv$hist_range[1] & selvar<=rv$hist_range[2],]$selvar, breaks=50)$counts)) /
      nrow(inputpts_points_toview[selvar>=rv$hist_range[1] & selvar<=rv$hist_range[2],])
    p <- ggplot2::ggplot(inputpts_points_toview, ggplot2::aes(x=selvar)) +
      ggplot2::geom_histogram(colour="white", aes(y = stat(count) / sum(count), fill=filter), breaks=rv$hist_breaks) +
      ggplot2::scale_fill_manual(values = c("FALSE"="darkgreen", "TRUE"="red")) +
      ggplot2::scale_y_continuous(labels = scales::percent, limits = rv$hist_ylim) +
      ggplot2::xlab(gsub("%v",input$select_inputvar,i18n$t("_hist_xlab"))) + ggplot2::ylab(i18n$t("_hist_ylab"))
    if (!is.null(input$miny) & !is.null(input$maxy) & input$check_rangey & input$filter_buttons == "manual") {
      p <- p + ggplot2::geom_vline(xintercept = c(input$miny, input$maxy), colour="red")
    }
    p
  })

  output$summary <- DT::renderDT({
    req(nrow(inputpts_points_toview) > 0)

    stats_summary <- as.data.frame(rbind(
      c(sum(!inputpts_points_toview$filter),
        summary(inputpts_points_toview[filter == FALSE,]$selvar),
        sd(inputpts_points_toview[filter == FALSE,]$selvar, na.rm = TRUE)),
      c(nrow(inputpts_points_toview),
        summary(inputpts_points_toview$selvar),
        sd(inputpts_points_toview$selvar, na.rm = TRUE))
    ))[,c(1,5,8,2:4,6:7)]
    if (sum(!inputpts_points_toview$filter) == 0) {
      stats_summary <- stats_summary[2,]
    }
    stats_summary[,2:3] <- format(stats_summary[,2:3], digits = 3)
    stats_summary[,4:8] <- format(round(stats_summary[,4:8], 5), drop0trailing = TRUE)
    names(stats_summary) <- ht(c("_number","_avg","_std","_min","_1q","_median","_3q","_max"), i18n)
    rownames(stats_summary) <- ht(c("_stats_nonfiltered","_stats_all"), i18n)

    DT::datatable(
      stats_summary,
      options = list(
        dom = 't',
        columnDefs = list(list(className = 'dt-right', targets = 0:8))
      ),
      escape = FALSE,
      selection = "none",
      rownames = TRUE,
      class = "compact",
      style = "default",
      autoHideNavigation = TRUE
    )

  })

})


