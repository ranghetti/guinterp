#' @title Buffer longitude-latitude polygons in metres
#' @description TODO
#' @param x TODO
#' @param dist TODO
#' @importFrom sf st_as_sf st_buffer st_crs st_transform st_is_longlat
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note Modified from \url{https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world}
#' @note License: GPL 3.0

st_buffer_m <- function(x, dist) {
  # from sp to sf (TODO improve checks)
  if (all(!is(x, "sf"), !is(x, "sfc"))) {
    x <- st_as_sf(x)
  }
  projected <- if (st_is_longlat(x)) {
    st_transform_utm(x)
  } else {
    x
  }
  buffered <- st_buffer(projected, dist)
  st_transform(buffered, st_crs(x))
}
