#' @title Pad a string.
#' @description Forked from `sen2r::str_pad2()`.
#'  Vectorised over `string`, `width` and `pad`.
#'  This is an internal function doing the same thing of [stringr::str_pad]
#'  (except for parameters `'width'` and `'length'` which must be of length 1),
#'  but without depending on package `stringi`.
#' @param string A character vector.
#' @param width Minimum width of padded strings.
#' @param side Side on which padding character is added (left, right or both).
#' @param pad Single padding character (default is a space).
#' @return A character vector.
#' @author Luigi Ranghetti, phD (2019) \email{luigi@@ranghetti.info}
#' @references L. Ranghetti, M. Boschetti, F. Nutini, L. Busetto (2020).
#'  "sen2r": An R toolbox for automatically downloading and preprocessing
#'  Sentinel-2 satellite data. _Computers & Geosciences_, 139, 104473. DOI:
#'  \url{https://doi.org/10.1016/j.cageo.2020.104473},
#'  URL: \url{http://sen2r.ranghetti.info/}.
#' @note License: GPL 3.0

str_pad2 <- function (string, width, side = c("left", "right", "both"), pad = " ") {
  side <- match.arg(side)
  if (length(width)>1) {
    warning("'width' must be of length 1 (first element is taken).")
    width <- width[1]
  }
  if (length(pad)>1) {
    warning("'pad' must be of length 1 (first element is taken).")
    pad <- pad[1]
  }
  sapply(string, function(sel_string) {
    s_length <- nchar(sel_string)
    pad_length <- max(0, width - s_length)
    paste(
      if (side == "left") {
        c(rep(pad, pad_length), sel_string)
      } else if (side == "right") {
        c(sel_string, rep(pad, pad_length))
      } else if (side == "both") {
        c(rep(pad, floor(pad_length/2)), sel_string, rep(pad, ceiling(pad_length/2)))
      },
      collapse = ""
    )
  })
}
