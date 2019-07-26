#' @title Load shapefiles from UI
#' @description from bfsapps
#' @slot input_paths TODO
#' @slot multiple TODO
#' @importFrom foreach foreach "%do%"
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

shiny_input_borders <- function(input_paths,
                               multiple = TRUE) {

  uploaded_exts <- gsub("(^.+)\\.(.+)$","\\2",input_paths$name)
  uploaded_basenames <- gsub("(^.+)\\.(.+)$","\\1",input_paths$name)

  # check if there are shapefiles among input files:
  # in this case, load all the files and group by file name,
  # else check that all the files are in the same format
  input_type_shp <- any(c("shp","shx","dbf","prj") %in% uploaded_exts)

  # group by basename
  if (input_type_shp) {

    if (multiple == FALSE & length(unique(uploaded_basenames)) > 1) {
      stop("Only one input file is allowed.")
    }

    foreach(sel_basename = unique(uploaded_basenames), .combine=rbind) %do% {

      sel_names <- input_paths$name[uploaded_basenames == sel_basename]
      sel_exts <- gsub("(^.+)\\.(.+)$","\\2",sel_names)
      sel_meta <- input_paths[match(sel_names, input_paths$name),]

      # checks
      if (anyNA(match(c("shp","shx","dbf","prj"),uploaded_exts))) {
        # if a shapefile was chosen but some files are missing, do not use it
        stop(
          paste("The shapefile", sel_basename, "is not complete.",
                "Please select all the files of the shapefile",
                "(at most .shp, .shx, .prj, .dbf)."
          )
        )
      } else {
        # if a shapefile was chosen and all the files are present,
        # rename the uploaded files in order to have the same filename and use them
        new_datapath <- file.path(
          dirname(sel_meta$datapath), sel_meta$name
        )
        for(i in seq_len(nrow(sel_meta))) {
          file.rename(sel_meta$datapath[i], new_datapath[i])
        }
        sel_meta$datapath <- new_datapath
        sel_meta[gsub("^.+\\.([^\\.]+)$","\\1",sel_meta$name)=="shp",]
      }

    }

  } else if (length(unique(uploaded_exts)) != 1) {
    stop(
      if (multiple == TRUE) {
        "The loaded files are not in the same data type."
      } else {
        "Only one input file is allowed."
      }
    )
  } else {
    input_paths
  }

}
