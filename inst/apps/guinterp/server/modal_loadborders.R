# Open modal dialog to load the polygon file of borders

observeEvent(input$button_load_borders, {
  showModal(modalDialog(
    title = ht("_modal_loadborders_title", i18n),
    size = "m",

    radioButtons(
      "border_type", label = ht("_border_type", i18n),
      choiceValues = c("files", "bbox"),
      choiceNames = ht(c("_border_type_files", "_border_type_bbox"), i18n),
      selected = "record"
    ),

    conditionalPanel(
      condition = "input.border_type == 'files'",
      div(
        div(
          style="vertical-align:top;",
            shiny::div(
              style = "display:inline-block;vertical-align:top;width:85pt;padding-top:8px;",
              shiny::strong(ht("_borderpath_label", i18n))
            ),
            shiny::div(
              style = "display:inline-block;vertical-align:top;width:calc(100% - 85pt - 50pt - 15px - 10pt - 10px);",
              shiny::textInput("borderpath_textin", NULL, "", width = "100%")
            ),
            shiny::div(
              style = "display:inline-block;vertical-align:top;width:50pt;",
              shinyFiles::shinyDirButton(
                "borderpath", ht("_borderpath_button", i18n),
                ht("_borderpath_sfb", i18n)
              )
            ),
            shiny::div(
              style = "display:inline-block;vertical-align:top;width:15px;margin-left:10pt;padding-top:8px;",
              shiny::htmlOutput("borderpath_errormess")
            )
        ),
        fluidRow(
          column(
            width = 8,
            div()
          ),
          column(
            width = 4,
            div(
              style="margin-top:-10px;",
              checkboxInput(
                "borders_showall",
                ht("_borders_showall", i18n),
                value = FALSE
              )
            )
          )
        ),
        div(
          style = "vertical-align:top;margin-bottom:10px;",
          shiny::wellPanel(
            shinycssloaders::withSpinner(DT::dataTableOutput("borderfiles_tbl"), type = 6)
          )
        ),

        actionButton(
          "load_extent_borders",
          strong(ph("\u2000",ht("_Load", i18n))),
          icon=icon("upload")
        ),

        shiny::div(
          style = "margin-top:15px;",
          shinyjs::disabled(radioButtons(
            "select_uid_which", label = ht("_select_uid_which", i18n),
            choiceValues = c("no", "record", "attr"),
            choiceNames = ht(c(
              "_select_uid_which_no",
              "_select_uid_which_record",
              "_select_uid_which_attr"
            ), i18n),
            selected = "record"
          ))
        ),
        uiOutput("selector_uid")
      )
    ),

    conditionalPanel(
      condition = "input.border_type == 'bbox'",
      numericInput(
        "bbox_buffer", ht("_bbox_buffer", i18n),
        value = 15, min = 0, step = 1
      )
    ),

    # leafletOutput("view_map_borders", height=400, width="100%"),
    easyClose = FALSE,
    footer = shiny::tagList(
      shinyjs::disabled(actionButton(
        "save_extent_borders",
        strong(ph("\u2000",ht("_Ok", i18n))),
        icon = icon("check")
      )),
      modalButton(ph("\u2000",ht("_Cancel", i18n)), icon = icon("ban"))
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
  shiny::updateTextInput(session, "borderpath_textin", value = borderpath_string)
})


# disable elements until vectors are loaded
observeEvent(c(input$border_type, rv$borders_polygon_raw), ignoreInit = TRUE, ignoreNULL = FALSE, {
  if (input$border_type == "files" & length(rv$borders_polygon_raw)==0) {
    shinyjs::disable("select_uid_which")
    shinyjs::disable("save_extent_borders")
    updateRadioButtons(session, "select_uid_which", selected = "record")
  } else {
    shinyjs::enable("select_uid_which")
    shinyjs::enable("save_extent_borders")
  }
})
observeEvent(input$borderfiles_tbl_rows_selected, ignoreNULL = FALSE, {
  if (length(input$borderfiles_tbl_rows_selected) > 0) {
    shinyjs::enable("load_extent_borders")
  } else {
    shinyjs::disable("load_extent_borders")
  }
})


# UID selector
output$selector_uid <- renderUI({
  req(rv$borders_polygon_raw)
  borders_names <- names(rv$borders_polygon_raw)[
    sapply(names(rv$borders_polygon_raw), function(x){
      !inherits(rv$borders_polygon_raw[[x]], "sfc")
    })
    ]
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


# Error messages
shiny::observe({
  output$borderpath_errormess <- path_check(input$borderpath_textin)
})


# Observer used to automatically filter the shps available in the selected ----
#
observeEvent(
  c(input$borderpath_textin),
  ignoreInit = TRUE, ignoreNULL = TRUE, {

    output$borderfiles_tbl <- DT::renderDT({

      vect_tbl  <- data.frame(i18n$t("_tbl_empty"), stringsAsFactors = FALSE)
      names(vect_tbl) <- i18n$t("_Filename")

      if (dir.exists(input$borderpath_textin)) {

        # Get the list of files which intersect pcolt data.
        #  In case it was already retrieved, do not compute it again
        #  TODO link the list of available files to a specific folder name
        vect_list_all <- list.files(input$borderpath_textin, full.names = TRUE)
        vect_ext <- gsub("^.+\\.([^\\.]+)$","\\1",vect_list_all)
        vect_list <-  if (!input$borders_showall) {
          vect_list_all[
            vect_ext %in% c("shp","gpkg","geojson","kml","gml","sqlite","tab")
            ]
        } else {vect_list_all}

        # TODO check that it contains multipolygons

        if (length(vect_list > 0)) {

          vect_tbl  <- data.frame(basename(vect_list), stringsAsFactors = FALSE)
          names(vect_tbl) <- i18n$t("_Filename")

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
  rv$borders_path <- file.path(input$borderpath_textin,rv$borderfiles_tbl[input$borderfiles_tbl_rows_selected,])

  # FIXME multiple paths

  # if not null, load it and Check that the vector is valid
  req(all(nrow(rv$borders_path)>0, !is.null(rv$borders_path)))
  rv$borders_polygon_raw <- tryCatch(
    {
      x <- sf::st_read(rv$borders_path, quiet=TRUE) %>%
        st_transform(4326) %>%
        st_cast("POLYGON") %>%
        st_zm()
      names(sf::st_geometry(x)) <- NULL
      attr(x, "valid") <- TRUE
      attr(x, "new") <- TRUE
      x
    },
    error = function(e) {
      shinyWidgets::sendSweetAlert(
        session, title = ht("_invalid_file", i18n),
        text = shiny::span(gsub(
          "\\%f", basename(rv$borders_path),
          ht("_borders_polygon_raw_invalid_message", i18n)
        )),
        type = "error", btn_labels = "Ok"
      )
      x <- sf::st_polygon(); attr(x, "valid") <- FALSE; x
    }
  )

})


# confirm inputpts and activate filtering
observeEvent(input$save_extent_borders, {

  # assign id_geom and group by it
  if (input$border_type == "files") {
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
      sf::st_transform(4326)
  }
  shinyjs::enable("button_load_inputpts")
  rv$borders_polygon_raw <- NULL
  shiny::removeModal()

})
