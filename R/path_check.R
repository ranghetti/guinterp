#' @title path_check
#' @description accessory functions to check that a directory exists and
#'  is writeable
#' @param path `string` full path to a folder
#' @rdname path_check
#' @author Luigi Ranghetti, phD (2019)
#' @importFrom shiny renderUI span renderText
#'
path_check <- function(path) {
  if (all(!is.null(path), length(path) > 0, path[1] != "")) {
    if (!dir.exists(path)) {
      return(renderUI(span(
        style = "color:red",
        "\u2718"
      )))
    } else if (file.access(path, mode = 2) < 0) {
      return(renderUI(span(
        style = "color:red",
        "\u2718"
      )))
    } else {
      return(renderUI(span(
        style = "color:darkgreen",
        "\u2714"
      )))
    }
  } else {
    return(renderText(""))
  }
}

#' @title raster_check
#' @description accessory functions to check that a raster exists and
#'  has a correct grid
#' @param path `string` full path to a raster
#' @rdname raster_check
#' @author Luigi Ranghetti, phD (2019)
#' @importFrom shiny renderUI span renderText
#' @importFrom stars read_stars
#'
raster_check <- function(path) {
  if (all(!is.null(path), length(path) > 0, path[1] != "")) {
    selraster <- tryCatch(
      suppressWarnings(suppressMessages(
        read_stars(path, proxy = TRUE, quiet = TRUE)
      )),
      error = function(e) e
    )
    if (is(selraster, "stars")) {
      out <- renderUI(span(
        style = "color:darkgreen",
        "\u2714"
      ))
      attr(out, "isvalid") <- TRUE
    } else {
      out <- renderUI(span(
        style = "color:red",
        "\u2718"
      ))
      attr(out, "isvalid") <- FALSE
    }
  } else {
    out <-renderText("")
    attr(out, "isvalid") <- NA
  }
  return(out)
}
