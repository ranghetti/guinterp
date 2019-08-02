#' @title Main GUI for app gestione dati
#' @description FUNCTION_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname guinterp
#' @export
#' @author Luigi Ranghetti, phD (2019) <ranghetti.l@irea.cnr.it>
#' @importFrom shiny shinyOptions runApp
guinterp <- function()  {

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(shiny::runApp(
      system.file("apps/guinterp", package = "guinterp"),
      display.mode = "normal",
      launch.browser = TRUE
    ))
  } else {
    stop("The function must be run from an interactive R session.")
  }

}
