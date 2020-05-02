#' @title Main GUI for app gestione dati
#' @description FUNCTION_DESCRIPTION
#' @param language "en" or "it" currently implemented
#'  (default is to retrieve it from system settings)
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname guinterp
#' @export
#' @author Luigi Ranghetti, phD (2019) <ranghetti.l@irea.cnr.it>
#' @importFrom shiny shinyOptions runApp
guinterp <- function(language = "en")  {

  # set language
  if (all(
    missing(language),
    grepl("^[Ii][Tt]", Sys.getlocale("LC_CTYPE"))
  )) {
    language <- "it"
  }

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    shinyOptions(ui_lang = language)
    return(runApp(
      system.file("apps/guinterp", package = "guinterp"),
      display.mode = "normal",
      launch.browser = TRUE
    ))
  } else {
    stop("The function must be run from an interactive R session.")
  }

}
