# Open modal dialog to load the point file of inputpts

observeEvent(input$button_load_inputpts, {
  showModal(modalDialog(
    title = "Seleziona i file dei punti da interpolare",
    size = "m",

    div(
      style="vertical-align:top;",
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:85pt;padding-top:8px;",
        shiny::strong("Cartella di input")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:calc(100% - 85pt - 50pt - 15px - 10pt - 10px);",
        shiny::textInput("inputptspath_textin", NULL, "", width = "100%")
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:50pt;",
        shinyFiles::shinyDirButton(
          "inputptspath", "Cambia",
          "Seleziona la cartella contenente i files con i punti da interpolare"
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
        shiny::htmlOutput("inputptspath_errormess")
      )
    ),
    fluidRow(
      column(
        width = 8,
        div(
          div(
            style = "display:inline-block;padding-right:10pt;",
            shiny::strong("Tipo di file")
          ),
          div(
            style = "display:inline-block;padding-right:20pt;",
            radioButtons(
              "inputptsfiletype",
              NULL, #"Tipo di file",
              list("Vettoriale" = "vect", "File di testo" = "txt"), #"Tutti i files" = "all"),
              selected = "vect",
              inline = TRUE
            )
          )
        )
      ),
      column(
        width = 4,
        div(
          style="margin-top:-10px;",
          checkboxInput(
            "inputpts_showall",
            "Mostra tutti i file",
            value = FALSE
          )
        )
      )
    ),
    div(
      style = "margin-bottom:10px;",
      shiny::wellPanel(
        shinycssloaders::withSpinner(DT::dataTableOutput("inputptsfiles_tbl"), type = 6)
      )
    ),

    actionButton("load_inputpts", strong("\u2000Carica"), icon=icon("upload")),

    fluidRow(
      column(
        width = 4,
        shiny::div(style = "margin-top:15px;", uiOutput("selector_inputvar"))
      ),
      column(
        width = 4,
        shiny::div(style = "margin-top:15px;", uiOutput("selector_xvar"))
      ),
      column(
        width = 4,
        shiny::div(style = "margin-top:15px;", uiOutput("selector_yvar"))
      )
    ),
    # leafletOutput("view_map_inputpts", height=400, width="100%"),
    easyClose = FALSE,
    footer = tagList(
      shinyjs::disabled(actionButton("save_extent_inputpts", strong("\u2000Ok"), icon=icon("check"))),
      modalButton("\u2000Annulla", icon = icon("ban"))
    )
  ))
})


# Observer for shinyfiles ----
shinyFiles::shinyDirChoose(input, "inputptspath", roots = volumes)
shiny::observeEvent(input$inputptspath, ignoreNULL = TRUE, ignoreInit = TRUE, {
  if (!is.integer(input$inputptspath) & !is.null(input$inputptspath)) {
    inputptspath_string <- shinyFiles::parseDirPath(roots = volumes, input$inputptspath)
  } else {
    inputptspath_string <- ""
  }
  shiny::updateTextInput(session, "inputptspath_textin", value = inputptspath_string)
})


# disable elements until vectors are loaded
observeEvent(rv$inputpts_points_raw, ignoreInit = FALSE, ignoreNULL = FALSE, {
  if (length(rv$inputpts_points_raw)==0) {
    shinyjs::disable("save_extent_inputpts")
  } else {
    shinyjs::enable("save_extent_inputpts")
  }
})


# Variable selector
observeEvent(rv$inputpts_points_raw, {
  rv$inputpts_names <- names(rv$inputpts_points_raw)[names(rv$inputpts_points_raw)!="geometry"]
  if (input$inputptsfiletype == "txt") {
    rv$def_xvar <- rv$inputpts_names[c(
      grep("^x$", tolower(rv$inputpts_names)),         # search first var. x (or X)
      grep("^longitude$", tolower(rv$inputpts_names)), # then var. longitude
      grep("^lo?ng?$", tolower(rv$inputpts_names)),    # then var. lon, long, lng
      grep("^lo?ng?", tolower(rv$inputpts_names)),     # then, starting with lon[g]/lng
      grep("^x", tolower(rv$inputpts_names)),          # then, starting with x
      1                                                # first name
    )[1]]
    rv$def_yvar <- rv$inputpts_names[c(
      grep("^y$", tolower(rv$inputpts_names)),        # search first var. y (or Y)
      grep("^latitude$", tolower(rv$inputpts_names)), # then var. latitude
      grep("^lat$", tolower(rv$inputpts_names)),      # then var. lat
      grep("^lat", tolower(rv$inputpts_names)),       # then, starting with lat
      grep("^y", tolower(rv$inputpts_names)),         # then, starting with y
      min(length(rv$inputpts_names),2)                # second name (if exists)
    )[1]]
    rv$def_inputvar <- c(
      rv$inputpts_names[!rv$inputpts_names %in% c(rv$def_xvar, rv$def_yvar)],
      rv$inputpts_names
    )[1]
  } else {
    rv$def_xvar <- rv$def_yvar <- NULL
    rv$def_inputvar <- rv$inputpts_names[1]
  }
})
output$selector_inputvar <- renderUI({
  req(rv$inputpts_points_raw)
  selectInput(
    "select_inputvar",
    label = "Variabile da interpolare",
    choices = rv$inputpts_names,
    selected = rv$def_inputvar
  )
})
output$selector_xvar <- renderUI({
  req(rv$inputpts_points_raw)
  if (input$inputptsfiletype == "txt") {
    selectInput(
      "select_xvar",
      label = "Coordinate X",
      choices = rv$inputpts_names,
      selected = rv$def_xvar
    )
  }
})
output$selector_yvar <- renderUI({
  req(rv$inputpts_points_raw)
  if (input$inputptsfiletype == "txt") {
    selectInput(
      "select_yvar",
      label = "Coordinate Y",
      choices = rv$inputpts_names,
      selected = rv$def_yvar
    )
  }
})


# Error messages
shiny::observeEvent(input$inputptspath_textin, {
  output$inputptspath_errormess <- path_check(input$inputptspath_textin)
})


# Observer used to automatically filter the shps available in the selected ----
# folder based on spatial extent, conformity with standards and selections
# in coltura/varietaand render the files table
#
observeEvent(
  c(input$inputptspath_textin),
  ignoreInit = TRUE, ignoreNULL = TRUE, {

    output$inputptsfiles_tbl <- DT::renderDT({

      vect_tbl  <- data.frame(`Nome File` = "Nessun file vettoriale trovato")

      if (dir.exists(input$inputptspath_textin)) {

        # Get the list of files which intersect pcolt data.
        #  In case it was already retrieved, do not compute it again
        #  TODO link the list of available files to a specific folder name!!!!
        vect_list_all <- list.files(input$inputptspath_textin, full.names = TRUE)
        vect_ext <- gsub("^.+\\.([^\\.]+)$","\\1",vect_list_all)
        vect_list <- if (!input$inputpts_showall) {
          switch(
            input$inputptsfiletype,
            "vect" = vect_list_all[vect_ext %in% c("shp","gpkg","geojson","kml","gml","sqlite","tab")],
            "txt" = vect_list_all[vect_ext %in% c("txt", "csv")]
          )
        } else {vect_list_all}

        # TODO check that it contains multipolygons

        if (length(vect_list > 0)) {

          vect_tbl <- data.frame(
            `Nome File` = basename(vect_list),
            stringsAsFactors = FALSE
          )

          dt_tbl <- DT::datatable(
            vect_tbl,
            options = list(
              searching = FALSE,
              paging = ifelse(nrow(vect_tbl) > 5, TRUE, FALSE),
              lengthMenu = c(5, 15, 35), pageLength = 5),
            escape = FALSE,
            selection = "single", # TODO manage "multiple"
            rownames = FALSE,
            class = "compact",
            style = "default",
            autoHideNavigation = TRUE
          )
          rv$inputptsfiles_tbl <- vect_tbl

          dt_tbl
        } else {
          rv$inputptsfiles_tbl <- NULL
          vect_tbl
        }
      } else {
        rv$inputptsfiles_tbl <- NULL
        vect_tbl
      }
    })
  })

# reset loaded file in filetype is changed
observeEvent(input$inputptsfiletype, {
  rv$inputpts_points_raw <- sf::st_polygon()
  attr(rv$inputpts_points_raw, "valid") <- FALSE
})

# load inputpts
observeEvent(input$load_inputpts, {

  # file paths
  rv$inputpts_path <- file.path(input$inputptspath_textin,rv$inputptsfiles_tbl[input$inputptsfiles_tbl_rows_selected,])

  # FIXME multiple paths

  # if not null, load it and Check that the vector is valid
  req(all(nrow(rv$inputpts_path)>0, !is.null(rv$inputpts_path)))
  rv$inputpts_points_raw <- if (input$inputptsfiletype == "vect") {
    tryCatch(
      {
        x <- sf::st_read(rv$inputpts_path, quiet=TRUE) %>%
          sf::st_transform(4326) %>%
          st_cast("POINT")
        x <- x[, which(unlist(lapply(x, is.numeric)))]
        req(ncol(x) > 1)
        names(sf::st_geometry(x)) <- NULL
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      },
      error = function(e) {
        shinyWidgets::sendSweetAlert(
          session, title = "File non valido",
          text = paste(
            "Il file", basename(rv$inputpts_path),
            "non è un vettoriale puntiforme valido,",
            "oppure non contiene campi numerici."
          ),
          type = "error", btn_labels = "Ok"
        )
        x <- sf::st_polygon(); attr(x, "valid") <- FALSE; x
      }
    )
  } else if (input$inputptsfiletype == "txt") {
    tryCatch(
      {
        x <- data.table::fread(rv$inputpts_path)
        x <- x[, which(unlist(lapply(x, is.numeric))), with=FALSE]
        req(nrow(x) > 0)
        attr(x, "valid") <- TRUE
        attr(x, "new") <- TRUE
        x
      },
      error = function(e) {
        shinyWidgets::sendSweetAlert(
          session, title = "File non valido",
          text = paste(
            "Il file",
            basename(rv$inputpts_path),
            "non è un tabellare riconosciuto,",
            "oppure non contiene campi numerici."
          ),
          type = "error", btn_labels = "Ok"
        )
        x <- data.table::data.table(); attr(x, "valid") <- FALSE; x
      }
    )
  }


})


# confirm inputpts and activate filtering
observeEvent(input$save_extent_inputpts, {

  if (input$inputptsfiletype == "txt") {
    rv$inputpts_points_raw <- st_as_sf(
      rv$inputpts_points_raw,
      coords = c(input$select_xvar, input$select_yvar),
      crs = 4326 # TODO allow different CRS
    )
  }

  rv$inputpts_points <- read_inputpts(
    rv$inputpts_points_raw,
    borders = rv$borders_polygon,
    varname = input$select_inputvar
  )
  shiny::removeModal()
  updateTabItems(session, "tabs", selected = "tab_interp") # go to interp tab
  rv$inputpts_points_raw <- rv$inputptsfiles_tbl <- NULL
  rv$interp_onoff <- TRUE # activate interp tab
  rv$new_inputs <- sample(1E6, 1) # dummy variable to activate observers

})
