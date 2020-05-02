#' @name h
#' @rdname ht
#' @title Convert to HTML entities
#' @description Internal functions used to convert  elements to `HTML`.
#'
#'  `t()` is converts [list of] character vectors to `HTML`.
#'
#'  `ht()` converts a vector of translation labels to a list of `HTML`
#'  translated entities.
#'
#'  `ph()` pastes `HTML` entities.
#' @param x Character vector or list
#' @importFrom shiny HTML
#' @return
#'  `t()` and `ht()`: if `x` is a 1-length character, an `HTML` object;
#'  if `x` is a vector with length > 1, a list of `HTML` objects;
#'  if `x` is a list of vectors, a list of objects.
#'
#'  `ph()`: an `HTML` element.
#' @author Luigi Ranghetti, phD (2020) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

h <- function(t) {
  vector_to_html <- function(t) {
    if (length(t) > 1) {
      lapply(as.character(t), HTML)
    } else {
      HTML(as.character(t))
    }
  }
  if (inherits(t, "list")) {
    lapply(t, vector_to_html)
  } else {
    vector_to_html(t)
  }
}

#' @name ht
#' @rdname ht
#' @param i18n Object containing translations
#' @export
ht <- function(x, i18n) {
  if (length(x) == 1) {
    h(i18n$t(x))
  } else {
    lapply(x, function(y) {h(i18n$t(y))})
  }
}


#' @name ph
#' @rdname ht
#' @importFrom shiny HTML
#' @param ... Elements to be pasted.
#' @export
ph <- function(...) {HTML(paste0(...))}
