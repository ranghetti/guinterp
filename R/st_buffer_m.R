#' @title Buffer latlon polygons in metres
#' @description TODO
#' @param poly TODO
#' @param width TODO
#' @importFrom rgeos gBuffer gCentroid
#' @importFrom sp CRS is.projected spTransform
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note Modified from https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world
#' @note License: GPL 3.0

st_buffer_m <- function(x, dist) {
  # from sp to sf (TODO improve checks)
  if (!is(x, "sf")) {
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
