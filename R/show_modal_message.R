#' @title Show modal message
#' @description TODO
#' @param message TODO
#' @param title TODO
#' @param icon TODO
#' @param icon_colour TODO
#' @param size TODO
#' @param easyClose TODO
#' @param footer TODO
#' @importFrom shiny div hr icon modalDialog p showModal
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

show_modal_message <- function(message,
                               title = NULL,
                               icon = "cog",
                               icon_colour = "default",
                               size = "s",
                               easyClose = FALSE,
                               footer = NULL) {
  if (icon_colour == "default") {
    icon_colour <- switch(
      icon,
      check = "darkgreen",
      ban = "red",
      "darkgrey"
    )
  }
  modalDialog(
    div(
      align = "center",
      if (!is.null(icon)) {
        p(
          style = paste0("text-align:center;font-size:500%;color:",icon_colour,";"),
          icon(
            icon,
            class = if (icon %in% c("refresh","circle-o-notch","cog")) {
              "fa-spin"
            } else if (icon == "spinner") {
              "fa-pulse"
            } else {
              NULL
            }
          )
        )
      },
      div(message),
      if (!is.null(title) & is.null(footer)) {
        hr(style = "margin-top: 0.75em; margin-bottom: 0.75em;")
      }
    ),
    title = if (!is.null(title)) {div(align = "center", title)},
    size = size,
    easyClose = easyClose,
    footer = footer
  ) %>%
    showModal()
}
