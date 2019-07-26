# Open modal dialog to load the polygon file of borders

observeEvent(input$button_load_borders, {
  showModal(modalDialog(
    title = "Seleziona il poligonale dei bordi",
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
            "borderdir_textin", NULL, ""
          )
        )
      ),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:50pt;",
        shinyFiles::shinyDirButton(
          "borderpath", "Cambia",
          "Seleziona la cartella contenente i poligonali dei bordi"
        )
      ),
      shiny::div(style = "display:inline-block;vertical-align:top;width:10px", ""),
      shiny::div(
        style = "display:inline-block;vertical-align:top;width:100px",
        shiny::htmlOutput("borderpath_errormess")
      )
    ),
    shiny::wellPanel(
      shinycssloaders::withSpinner(DT::dataTableOutput("borderfiles_tbl"), type = 6)
    ),

    actionButton("load_extent_borders", strong("\u2000Carica"), icon=icon("check")),

    shinyjs::disabled(radioButtons(
      "select_uid_which", label = "ID univoco da utilizzare",
      choices = list(
        "Dissolvi tutto" = "no",
        "Numero di riga" = "record",
        "Scegli un attributo" = "attr"
      ),
      selected = "record"
    )),
    uiOutput("selector_uid"),
    # leafletOutput("view_map_borders", height=400, width="100%"),
    easyClose = FALSE,
    footer = tagList(
      shinyjs::disabled(actionButton("save_extent_borders", strong("\u2000Ok"), icon=icon("check"))),
      modalButton("\u2000Cancel", icon = icon("ban"))
    )
  ))
})


# Observer for shinyfiles ----
shinyFiles::shinyDirChoose(input, "borderpath", roots = volumes)
shiny::observeEvent(input$borderpath, ignoreNULL = TRUE, ignoreInit = TRUE, {
  if (!is.integer(input$borderpath) & !is.null(input$borderpath)) {
    borderpath_string <- shinyFiles::parseDirPath(roots = volumes, input$borderpath)
  } else {
    borderpath_string <- ""
  }
  shiny::updateTextInput(session, "borderdir_textin", value = borderpath_string)
})


# disable elements until vectors are loaded
observeEvent(rv$borders_polygon_raw, ignoreInit = FALSE, ignoreNULL = FALSE, {
  if (length(rv$borders_polygon_raw)==0) {
    shinyjs::disable("select_uid_which")
    shinyjs::disable("save_extent_borders")
    updateRadioButtons(session, "select_uid_which", selected = "record")
  } else {
    shinyjs::enable("select_uid_which")
    shinyjs::enable("save_extent_borders")
  }
})


# UID selector
output$selector_uid <- renderUI({
  req(rv$borders_polygon_raw)
  borders_names <- names(rv$borders_polygon_raw)[names(rv$borders_polygon_raw)!="geometry"]
  conditionalPanel(
    condition = "input.select_uid_which == 'attr'",
    selectInput(
      "select_uid",
      label = NULL, #label = "Seleziona l'attributo",
      choices = borders_names,
      selected = borders_names[1]
    )
  )
})


# Observer used to automatically filter the shps available in the selected ----
# folder based on spatial extent, conformity with standards and selections
# in coltura/varietaand render the files table
#
observeEvent(
  c(input$borderdir_textin),
  ignoreInit = TRUE, ignoreNULL = TRUE, {

    output$borderfiles_tbl <- DT::renderDT({

      vect_tbl  <- data.frame(`Nome File` = "Nessun file vettoriale trovato")

      if (dir.exists(input$borderdir_textin)) {

        # Get the list of files which intersect pcolt data.
        #  In case it was already retrieved, do not compute it again
        #  TODO link the list of available files to a specific folder name!!!!
        vect_list <- list.files(input$borderdir_textin, "\\.shp$", full.names = TRUE)

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
          rv$borderfiles_tbl <- vect_tbl

          dt_tbl
        } else {
          rv$borderfiles_tbl <- NULL
          vect_tbl
        }
      } else {
        rv$borderfiles_tbl <- NULL
        vect_tbl
      }
    })
  })


# load borders
observeEvent(input$load_extent_borders, {

  # file paths
  rv$borders_path <- file.path(input$borderdir_textin,rv$borderfiles_tbl[input$borderfiles_tbl_rows_selected,])

  # FIXME multiple paths

  # if not null, load it and Check that the vector is valid
  req(all(nrow(rv$borders_path)>0, !is.null(rv$borders_path)))
  rv$borders_polygon_raw <- tryCatch(
    {
      x <- sf::st_read(rv$borders_path, quiet=TRUE) %>%
        st_transform(4326)
      names(sf::st_geometry(x)) <- NULL
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    },
    error = function(e) {x <- sf::st_polygon(); attr(x, "valid") <- FALSE; x}
  )

})


# confirm inputpts and activate filtering
observeEvent(input$save_extent_borders, {

  # assign id_geom and group by it
  rv$borders_polygon <- if (input$select_uid_which == "attr") {
    rv$borders_polygon_raw %>%
      dplyr::select(input$select_uid) %>%
      dplyr::rename(id_geom = input$select_uid)
  } else if (input$select_uid_which == "record") {
    rv$borders_polygon_raw %>%
      dplyr::transmute(id_geom = seq_len(nrow(rv$borders_polygon_raw)))
  } else if (input$select_uid_which == "no") {
    rv$borders_polygon_raw %>%
      dplyr::transmute(id_geom = 0)
  } %>%
    dplyr::group_by(id_geom) %>% dplyr::summarise() %>%
    sf::st:tranform(4326)

  shiny::removeModal()

})


# # update the map
# observeEvent(input$save_extent_borders, {
#
#   if(attr(rv$borders_polygon_raw, "valid")) {
#     # if the vector is valid, update the map
#     leafletProxy("view_map_vectfile") %>%
#       # clearShapes() %>%
#       fitBounds(
#         lng1 = min(st_coordinates(rv$vectfile_polygon)[,"X"]),
#         lat1 = min(st_coordinates(rv$vectfile_polygon)[,"Y"]),
#         lng2 = max(st_coordinates(rv$vectfile_polygon)[,"X"]),
#         lat2 = max(st_coordinates(rv$vectfile_polygon)[,"Y"])
#       ) %>%
#       addPolygons(data = rv$vectfile_polygon,
#                   group = "Extent",
#                   # label = ~tile_id,
#                   # labelOptions = labelOptions(noHide = TRUE, direction = "auto"),
#                   fill = TRUE,
#                   fillColor = "green",
#                   fillOpacity = .3,
#                   stroke = TRUE,
#                   weight = 3,
#                   color = "darkgreen")
#   } else {
#     # if the vector is not valid, reset the map
#     # react_map_vectfile(base_map())
#   }
#
# })
