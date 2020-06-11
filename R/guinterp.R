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
#' @importFrom shiny shinyOptions runApp onStop shinyApp
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

  jscode <- "shinyjs.closeWindow = function() { open(location, '_self').close(); }"

  # clean up on exit
  shiny::onStop(function() {
    cat("Session stopped\n")
  })

  # run
  options(device.ask.default = FALSE)
  require(shinyBS) # required to show popup messages
  shinyOptions(
    ui_lang = language,
    demo_mode = demo
  )
  return(shinyApp(
    ui = guinterp_ui,
    server = guinterp_server,
    options = list(
      display.mode = "normal",
      launch.browser = TRUE
    )
  ))

}
