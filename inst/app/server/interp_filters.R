#   ____________________________________________________________________________
#   Observer used to subset points when filter values are changed           ####

## Main filter ----

observeEvent(c(input$filter_buttons), {
  req(rv$inputpts_points)
  req(input$filter_buttons)

  if (input$filter_buttons=="manual") {
    # Manual filter
    # filter data basing on widgets (done in the specific widget's observers)

  } else if (input$filter_buttons=="auto") {
    # Default filter
    show_modal_message(shiny::p("Filtraggio dei punti in corso..."))
    # Load json with default filters
    if (system.file("app/ancillary/default_filters.json", package="guinterp")=="") {
      file.copy(
        system.file("app/ancillary/default_pkg_filters.json", package="guinterp"),
        file.path(system.file("app/ancillary", package="guinterp"), "default_filters.json")
      )
    }
    def_filters <- jsonlite::fromJSON(
      system.file("app/ancillary/default_filters.json", package="guinterp")
    )
    # Apply filters
    filtered_ptsdata <- tryCatch({
      rv$inputpts_points <- filter_pts_reset(rv$inputpts_points)
      for (filter_name in names(def_filters)) {
        rv$inputpts_points <- filter_pts(
          indata = rv$inputpts_points,
          metric = filter_name,
          value = def_filters[[filter_name]],
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE,
          samplesize = NA,
          par1 = NA
        )
      }
      rv$inputpts_points
    }, error = function(e) {
      paste(i18n$t("_filtered_ptsdata_error"), e$message)
    })
    shiny::removeModal()

  } else if (input$filter_buttons=="minimal") {
    # Minimal filter
    show_modal_message(ht("_filtered_ptsdata", i18n))
    filtered_ptsdata <- tryCatch({
      rv$inputpts_points <- filter_pts_reset(rv$inputpts_points) %>%
        filter_pts(
          "rangeq", c(.02,.98), # filter points < 2° and > 98° percentiles
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA
        )
    }, error = function(e) {
      paste(i18n$t("_filtered_ptsdata_error"), e$message)
    })
    shiny::removeModal()

  } else if (input$filter_buttons=="no") {
    # No filter
    show_modal_message(ht("_filtered_ptsdata_restore", i18n))
    filtered_ptsdata <- tryCatch({
      rv$inputpts_points <- filter_pts_reset(rv$inputpts_points)
    })
    shiny::removeModal()

  } # end of filter_buttons IF cycle
  rv$change_interp <- sample(1E6,1) # dummy var for map/hist update

})



## Manual filters ----

# Define UI objects dinamically for filtering
output$indata_rangey <- renderUI({
  req(rv$inputpts_points, rv$borders_polygon)
  shiny::div(
    shiny::div(
      style="display:inline-block;position:relative;",
      shiny::numericInput(
        inputId = "miny",
        label = shiny::span(style="font-weight:normal;", ht("_miny", i18n)),
        min = 0,
        max = ceiling(max(rv$inputpts_points$selvar)),
        value = round(quantile(rv$inputpts_points$selvar, .02), 2),
        step = 1
      )
    ),
    shiny::div(
      style="display:inline-block;position:relative;padding-left:10px;",
      shiny::numericInput(
        inputId = "maxy",
        label = shiny::span(style="font-weight:normal;", ht("_maxy", i18n)),
        min = 0,
        max = ceiling(max(rv$inputpts_points$selvar)),
        value = round(quantile(rv$inputpts_points$selvar, .98), 2),
        step = 1
      )
    )
  )
})

output$pos_ui <- shiny::renderUI({
  req(rv$inputpts_points, rv$borders_polygon)
  maxdist <- max(sapply(
    sf::st_geometry(st_transform_utm(rv$borders_polygon)),
    function(x) {with(as.list(sf::st_bbox(x)), sqrt((xmax-xmin)^2+(ymax-ymin)^2))}
  ))
  shiny::sliderInput(
    inputId="pos",
    label=NULL,
    min = 0,
    max = ceiling(maxdist/10^floor(log10(maxdist))/2)*10^floor(log10(maxdist)),
    value = maxdist/50,
    post = " m",
    step = 10^(floor(log10(maxdist))-2)
  )
})



# Filters
# observe({
#   req(rv$inputpts_points, rv$borders_polygon)

  observeEvent(c(input$check_rangey, input$miny, input$maxy, input$filter_buttons), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_rangey) {
        shinyjs::enable("miny")
        shinyjs::enable("maxy")
        req(input$miny, input$maxy)
        rv$inputpts_points <- filter_pts(
          rv$inputpts_points, "rangey", c(input$miny,input$maxy),
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA
        )
      } else {
        shinyjs::disable("miny")
        shinyjs::disable("maxy")
        filter_pts_reset(rv$inputpts_points, "rangey")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })

  observeEvent(c(input$check_zscorey, input$zscorey, input$filter_buttons), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_zscorey) {
        shinyjs::enable("zscorey")
        rv$inputpts_points <- filter_pts(
          rv$inputpts_points, "zscorey", input$zscorey/100,
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA
        )
      } else {
        shinyjs::disable("zscorey")
        filter_pts_reset(rv$inputpts_points, "zscorey")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })

  observeEvent(c(input$check_rbiasy, input$rbiasy, input$filter_buttons), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_rbiasy) {
        shinyjs::enable("rbiasy")
        rv$inputpts_points <- filter_pts(
          rv$inputpts_points, "rbiasy", input$rbiasy/100,
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA
        )
      } else {
        shinyjs::disable("rbiasy")
        filter_pts_reset(rv$inputpts_points, "rbiasy")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })

  observeEvent(c(input$check_rangeq, input$rangeq, input$filter_buttons), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_rangeq) {
        shinyjs::enable("rangeq")
        rv$inputpts_points <- filter_pts(
          rv$inputpts_points, "rangeq", input$rangeq/100,
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA
        )
      } else {
        shinyjs::disable("rangeq")
        filter_pts_reset(rv$inputpts_points, "rangeq")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })

  observeEvent(c(input$check_pos, input$pos, input$filter_buttons, rv$borders_polygon), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_pos) {
        shinyjs::enable("pos")
        req(input$pos)
        rv$inputpts_points <- filter_pts(
          rv$inputpts_points, "pos", input$pos,
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA
        )
      } else {
        shinyjs::disable("pos")
        filter_pts_reset(rv$inputpts_points, "pos")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })

  observeEvent(c(input$check_editmap, input$pos, input$filter_buttons, rv$editmap_geoms), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_editmap) {
        shinyjs::enable("editmap")
        req(rv$editmap_geoms)
        rv$inputpts_points <- filter_pts(
          rv$inputpts_points, "editmap", rv$editmap_geoms,
          inlayer = rv$borders_polygon,
          id_fieldname="id_geom",
          byfield = TRUE, samplesize = NA,
          reverse = as.logical(input$editmap_reverse)
        )
        if (length(rv$editmap_geoms) == 0) {rv$editmap_geoms <- NULL} # list() to NULL
      } else {
        shinyjs::disable("editmap")
        filter_pts_reset(rv$inputpts_points, "editmap")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })

  observeEvent(c(input$check_selpts, input$pos, input$filter_buttons, rv$selpts_uids), ignoreNULL = FALSE, ignoreInit = FALSE, {
    req(rv$inputpts_points, rv$borders_polygon)
    if (input$filter_buttons == "manual") {
      if (input$check_selpts) {
        shinyjs::enable("selpts")
        if (!is.null(rv$selpts_uids) > 0) {
          rv$inputpts_points <- filter_pts(
            rv$inputpts_points, "selpts", rv$selpts_uids,
            inlayer = rv$borders_polygon,
            id_fieldname="id_geom",
            byfield = TRUE, samplesize = NA,
            reverse = as.logical(input$selpts_reverse)
          )
        }
      } else {
        shinyjs::disable("selpts")
        filter_pts_reset(rv$inputpts_points, "selpts")
      }
      rv$change_interp <- sample(1E6,1) # dummy var for map/hist update
    }
  })



  # Set initial value
  # (it is done in this way, so that points are filtered
  # also when the app is opened)
#   shiny::updateSelectInput(session, "filter_buttons", selected = "auto")
#
# }) # end of observe on filters


## Import filters ----

# Read path
shinyFiles::shinyFileChoose(
  input, "sfb_importFilters", roots=volumes, session=session,
  filetypes = c("JSON"="json")
)
shiny::observeEvent(input$sfb_importFilters, {
  rv$importFilters <- shinyFiles::parseFilePaths(volumes,input$sfb_importFilters)
})

# Export filtering parameters
observeEvent(input$downloadFilters, {
  # @Lorenzo, modifica tu i volumi e la cartella predefinita come nel resto dell'app
  shinyFiles::shinyFileSave(input, "downloadFilters", roots=volumes, session=session)
  export_param_path <- shinyFiles::parseSavePath(volumes, input$downloadFilters)
  if (nrow(export_param_path)>0) {
    filter_list <- list()
    if (input$check_rangey) {filter_list$rangey <- c(input$miny, input$maxy)}
    if (input$check_zscorey) {filter_list$zscorey <- input$zscorey/100}
    if (input$check_rbiasy) {filter_list$rbiasy <- input$rbiasy/100}
    if (input$check_rangeq) {filter_list$rangeq <- input$rangeq/100}
    if (input$check_pos) {filter_list$pos <- input$pos}
    write(
      jsonlite::toJSON(filter_list, pretty = TRUE),
      as.character(export_param_path$datapath)
    )

    shinyWidgets::sendSweetAlert(
      session,
      title = i18n$t("_downloadFilters_success"),
      text = shiny::span(ht("_downloadFilters_message", i18n)),
      type = "success", btn_labels = "Ok"
    )
  }
})

# # if Import is pressed, read a json object (using server-side button)
observeEvent(rv$importFilters, {
  # server-side button
  req(all(nrow(rv$importFilters)>0, !is.null(rv$importFilters)))
  imported_filters <- jsonlite::fromJSON(rv$importFilters$datapath)
  if (!is.null(imported_filters[["rangey"]])) {
    updateNumericInput(session, "miny", value=imported_filters[["rangey"]][1])
    updateNumericInput(session, "maxy", value=imported_filters[["rangey"]][2])
    updateCheckboxInput(session, "check_rangey", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_rangey", value=FALSE)
  }
  if (!is.null(imported_filters[["zscorey"]])) {
    updateSliderInput(session, "zscorey", value=imported_filters[["zscorey"]]*100)
    updateCheckboxInput(session, "check_zscorey", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_zscorey", value=FALSE)
  }
  if (!is.null(imported_filters[["rbiasy"]])) {
    updateSliderInput(session, "rbiasy", value=imported_filters[["rbiasy"]]*100)
    updateCheckboxInput(session, "check_rbiasy", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_rbiasy", value=FALSE)
  }
  if (!is.null(imported_filters[["rangeq"]])) {
    updateSliderInput(session, "rangeq", value=imported_filters[["rangeq"]]*100)
    updateCheckboxInput(session, "check_rangeq", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_rangeq", value=FALSE)
  }
  if (!is.null(imported_filters[["pos"]])) {
    updateSliderInput(session, "pos", value=imported_filters[["pos"]])
    updateCheckboxInput(session, "check_pos", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_pos", value=FALSE)
  }

  shinyWidgets::sendSweetAlert(
    session,
    title = i18n$t("_importFilters_success"),
    text = shiny::span(ht("_importFilters_message", i18n)),
    type = "success", btn_labels = "Ok"
  )
})

# # Load default values from json at startup
observeEvent(rv$interp_onoff, {
  req(rv$interp_onoff)
  # server-side button
  if (system.file("app/ancillary/default_filters.json", package="guinterp")=="") {
    file.copy(
      system.file("app/ancillary/default_pkg_filters.json", package="guinterp"),
      file.path(system.file("app/ancillary", package="guinterp"), "default_filters.json")
    )
  }
  def_filters <- jsonlite::fromJSON(
    system.file("app/ancillary/default_filters.json", package="guinterp")
  )

  rv$inputpts_points <- filter_pts_reset(rv$inputpts_points)

  if (!is.null(def_filters[["rangey"]])) {
    updateNumericInput(session, "miny", value=def_filters[["rangey"]][1])
    updateNumericInput(session, "maxy", value=def_filters[["rangey"]][2])
    updateCheckboxInput(session, "check_rangey", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_rangey", value=FALSE)
  }
  if (!is.null(def_filters[["zscorey"]])) {
    updateSliderInput(session, "zscorey", value=def_filters[["zscorey"]]*100)
    updateCheckboxInput(session, "check_zscorey", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_zscorey", value=FALSE)
  }
  if (!is.null(def_filters[["rbiasy"]])) {
    updateSliderInput(session, "rbiasy", value=def_filters[["rbiasy"]]*100)
    updateCheckboxInput(session, "check_rbiasy", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_rbiasy", value=FALSE)
  }
  if (!is.null(def_filters[["rangeq"]])) {
    updateSliderInput(session, "rangeq", value=def_filters[["rangeq"]]*100)
    updateCheckboxInput(session, "check_rangeq", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_rangeq", value=FALSE)
  }
  if (!is.null(def_filters[["pos"]])) {
    updateSliderInput(session, "pos", value=def_filters[["pos"]])
    updateCheckboxInput(session, "check_pos", value=TRUE)
  } else {
    updateCheckboxInput(session, "check_pos", value=FALSE)
  }
})

# Export default filters
observeEvent(input$setdefaultFilters, {
  filterdef_list <- list()
  if (input$check_rangey) {filterdef_list$rangey <- c(input$miny, input$maxy)}
  if (input$check_zscorey) {filterdef_list$zscorey <- input$zscorey/100}
  if (input$check_rbiasy) {filterdef_list$rbiasy <- input$rbiasy/100}
  if (input$check_rangeq) {filterdef_list$rangeq <- input$rangeq/100}
  if (input$check_pos) {filterdef_list$pos <- input$pos}
  write(
    jsonlite::toJSON(filterdef_list, pretty = TRUE),
    file.path(
      system.file("app/ancillary", package="guinterp"),
      "default_filters.json"
    )
  )

  shinyWidgets::sendSweetAlert(
    session, title = NULL,
    text = shiny::span(ht("_setdefaultFilters_success", i18n)),
    type = "success", btn_labels = "Ok"
  )
})
