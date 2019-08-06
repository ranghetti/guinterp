#' @title path_check
#' @description accessory functions to check that a directory exists and
#'  is writable
#' @param path `string` fullpath to a folder
#' @rdname path_check
#' @export
#' @author Luigi Ranghetti, phD (2019)
#' @importFrom shiny renderUI span renderText
#'
path_check <- function(path) {
  if (all(!is.null(path), length(path) > 0, path[1] != "")) {
    if (!dir.exists(path)) {
      return(shiny::renderUI(shiny::span(
        style = "color:red",
        "\u2718"
      )))
    } else if (file.access(path, mode = 2) < 0) {
      return(shiny::renderUI(shiny::span(
        style = "color:red",
        "\u2718"
      )))
    } else {
      return(shiny::renderUI(shiny::span(
        style = "color:darkgreen",
        "\u2714"
      )))
    }
  } else {
    return(shiny::renderText(""))
  }
}

#' @title raster_check
#' @description accessory functions to check that a raster exists and
#'  has a correct grid
#' @param path `string` fullpath to a raster
#' @rdname raster_check
#' @export
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
      return(shiny::renderUI(shiny::span(
        style = "color:darkgreen",
        "\u2714"
      )))
    } else {
      return(shiny::renderUI(shiny::span(
        style = "color:red",
        "\u2718"
      )))
    }
  } else {
    return(shiny::renderText(""))
  }
}
