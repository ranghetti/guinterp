#' @title Create global output grid
#' @description TODO
#' @param indata_sf TODO all input points
#' @param outres `numeric (1)` TODO spatial resolution of output raster (in CRS unit)
#' @param outcrs `crs` output crs
#' @param offset `numeric (2)` offset from (0,0) in `outcrs` projection
#' @param border `numeric (1)` TODO extent of border (in CRS units)
#' @importFrom sf st_crs st_transform
#' @importFrom stars st_as_stars st_set_dimensions
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

# FIXME border should be retrieve automatically from the filters (i.e. to prevent that a Gaussian would cut too much border)
make_interp_grid <- function(
  indata_sf,
  outres = 5,
  outcrs = st_crs(indata_sf),
  offset = c(0,0),
  border = 15
) {

  # convert to crs
  if (!is(outcrs, "crs")) {outcrs <- st_crs2(outcrs)}

  # reproj if necessary
  if (st_crs(indata_sf) != outcrs) {
    indata_sf <- st_transform(indata_sf, outcrs)
  }

  # check offset to be a numeric vector of length 2
  if (!is.numeric(offset)) {stop("'offset' must be numeric.")}
  if (length(offset) == 1) {offset <- rep(offset, 2)}
  if (length(offset) > 2) {stop("'offset' must be of length 2.")}

  # create global grid
  indata_bb <- (floor((st_bbox(indata_sf) - offset) / outres) + c(0,0,1,1)) *
    outres + offset + border*c(-1,-1,1,1)
  dx <- seq(indata_bb[1], indata_bb[3], outres)
  dy <- seq(indata_bb[4], indata_bb[2], -outres)
  indata_grd <- st_as_stars(matrix(0, length(dx), length(dy))) %>%
    st_set_dimensions(1, dx) %>%
    st_set_dimensions(2, dy) %>%
    st_set_dimensions(names = c("x", "y")) %>%
    st_set_crs(st_crs(indata_sf))

  return(indata_grd)

}
