


# TODO è una copia di loadinputptss, sistemare!


# Open modal dialog to load the point file of inputpts

observeEvent(input$button_load_inputpts, {
  showModal(modalDialog(
    title = "Seleziona il poligonale dei punti da interpolare",
    size = "m",

    div(
      style="display:inline-block;vertical-align:top;margin-bottom:10px;",
      shiny::strong("Seleziona i files da caricare"),
      shiny::div(
        style = "vertical-align:top;width:100%",
        shiny::div(
          style = "display:inline-block;vertical-align:top;width:95pt;",
          "Cartella di input:"
        ),
        shiny::div(
          style = "display:inline-block;vertical-align:top;padding-left:10px;width:calc(100% - 95pt - 10px - 50pt);",
          shiny::textInput(
            "inputptsdir_textin", NULL, ""
          )
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:50pt;",
        shinyFiles::shinyDirButton(
          "inputptspath", "Cambia",
          "Seleziona la cartella contenente i poligonali dei punti da interpolare"
        )
      ),
      shiny::div(style = "display:inline-block;vertical-align:top;width:10px", ""),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:100px",
        shiny::htmlOutput("inputptspath_errormess")
      )
    ),
    shiny::wellPanel(
      shinycssloaders::withSpinner(DT::dataTableOutput("inputptsfiles_tbl"), type = 6)
    ),

    actionButton("load_inputpts", strong("\u2000Carica"), icon=icon("check")),

    uiOutput("selector_inputvar"),
    # leafletOutput("view_map_inputpts", height=400, width="100%"),
    easyClose = FALSE,
    footer = tagList(
      shinyjs::disabled(actionButton("save_extent_inputpts", strong("\u2000Ok"), icon=icon("check"))),
      modalButton("\u2000Cancel", icon = icon("ban"))
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
  shiny::updateTextInput(session, "inputptsdir_textin", value = inputptspath_string)
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
output$selector_inputvar <- renderUI({
  req(rv$inputpts_points_raw)
  inputpts_names <- names(rv$inputpts_points_raw)[names(rv$inputpts_points_raw)!="geometry"]
    selectInput(
      "select_inputvar",
      label = "Seleziona la variabile da interpolare",
      choices = inputpts_names,
      selected = inputpts_names[1]
    )
})


# Observer used to automatically filter the shps available in the selected ----
# folder based on spatial extent, conformity with standards and selections
# in coltura/varietaand render the files table
#
observeEvent(
  c(input$inputptsdir_textin),
  ignoreInit = TRUE, ignoreNULL = TRUE, {

    output$inputptsfiles_tbl <- DT::renderDT({

      vect_tbl  <- data.frame(`Nome File` = "Nessun file vettoriale trovato")

      if (dir.exists(input$inputptsdir_textin)) {

        # Get the list of files which intersect pcolt data.
        #  In case it was already retrieved, do not compute it again
        #  TODO link the list of available files to a specific folder name!!!!
        vect_list <- list.files(input$inputptsdir_textin, "\\.shp$", full.names = TRUE)

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
            escape = FALSE, selection = 'multiple', rownames = FALSE,
            style = "bootstrap",
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


# load inputpts
observeEvent(input$load_inputpts, {

  # file paths
  rv$inputpts_path <- file.path(input$inputptsdir_textin,rv$inputptsfiles_tbl[input$inputptsfiles_tbl_rows_selected,])

  # FIXME multiple paths

  # if not null, load it and Check that the vector is valid
  req(all(nrow(rv$inputpts_path)>0, !is.null(rv$inputpts_path)))
  rv$inputpts_points_raw <- tryCatch(
    {
      x <- sf::st_read(rv$inputpts_path, quiet=TRUE) %>%
        sf::st_transform(4326)
      names(sf::st_geometry(x)) <- NULL
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    },
    error = function(e) {x <- sf::st_polygon(); attr(x, "valid") <- FALSE; x}
  )

})


# confirm inputpts and activate filtering
observeEvent(input$save_extent_inputpts, {
  rv$inputpts_points <- read_inputpts(
    rv$inputpts_points_raw,
    borders = rv$borders_polygon,
    varname = input$select_inputvar
  )
  shiny::removeModal()
  updateTabItems(session, "tabs", selected = "tab_interp") # go to interp tab
  rv$interp_onoff <- TRUE # activate interp tab
  rv$new_inputs <- sample(1E6, 1) # dummy variable to activate observers

})
