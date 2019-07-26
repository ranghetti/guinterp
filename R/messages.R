#' @title bf_error
#' @description FUNCTION_DESCRIPTION
#' @param message PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' @rdname bf_error
#' @export
#' @author Lorenzo Busetto, 2018
#' @importFrom shinyalert shinyalert
#'
bf_error <- function(message) {
  shinyalert::shinyalert(
    title = "",
    text = message,
    type = "error",
    confirmButtonCol = "#AEDEF4",
    timer = 0, imageHeight = 5
  )
}

#' @title bf_message
#' @description FUNCTION_DESCRIPTION
#' @param message PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' @rdname bf_message
#' @export
#' @author Lorenzo Busetto, 2018
#' @importFrom shinyalert shinyalert
#'
bf_message <- function(message) {
  shinyalert::shinyalert(
    title = "",
    text = message,
    type = "success",
    imageHeight = 5,
    closeOnClickOutside = TRUE,
    closeOnEsc = TRUE,
    showConfirmButton = FALSE,
    timer = 2000
  )
}

#' @title bf_message_confirm
#' @description FUNCTION_DESCRIPTION
#' @param message PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' @rdname bf_message_confirm
#' @export
#' @author Lorenzo Busetto, 2018
#' @importFrom shinyalert shinyalert
#'
bf_message_confirm <- function(message) {
  shinyalert::shinyalert(
    title = "",
    text = message,
    type = "success",
    imageHeight = 5,
    closeOnClickOutside = TRUE,
    closeOnEsc = TRUE,
    showConfirmButton = TRUE,
    timer = 10000
  )
}
