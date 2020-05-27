#' @title Launch GUInterp
#' @description Open the GUInterp graphical interface
#' @param language Interface language (currently only English -- `"en"` --
#'  and Italian -- `"it"` -- were implemented).
#'  Default is to retrieve it from system settings.
#' @param demo If TRUE, only internal example data can be used
#'  (default is FALSE).
#' @return NULL (the function is called for its side effect)
#' @export
#' @author Luigi Ranghetti, phD (2020) \email{luigi@@ranghetti.info}
#' @importFrom shiny shinyOptions runApp
#' @examples \dontrun{
#' guinterp()
#' }
guinterp <- function(
  language = "en",
  demo = FALSE
)  {

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
    shinyOptions(ui_lang = language, demo_mode = demo)
    return(runApp(
      system.file("apps/guinterp", package = "guinterp"),
      display.mode = "normal",
      launch.browser = TRUE
    ))
  } else {
    stop("The function must be run from an interactive R session.")
  }

}
