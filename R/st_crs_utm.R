# (see https://stackoverflow.com/questions/40132542/get-a-cartesian-projection-accurate-around-a-lat-lng-pair)

#' @title Functions to manage UTM projections
#' @name st_crs_utm
#' @rdname st_crs_utm
#' @description `st_crs_utm` gives the UTM zone of an sf object
#' @param x TODO
#' @importFrom sf st_union st_centroid st_transform st_coordinates
#' @export
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0
st_crs_utm <- function(x) {
  centre <- suppressWarnings(
    st_union(x) %>% st_centroid()  # compute centroid
  ) %>% st_transform(4326)         # convert to lonlat
  st_crs_utm_from_lonlat(
    st_coordinates(centre)[,"X"],
    st_coordinates(centre)[,"Y"]
  )
}

#' @name st_crs_utm_from_lonlat
#' @rdname st_crs_utm
#' @description `st_crs_utm_from_lonlat` gives the UTM zone of a couple of geographic coordinates
#' @param lon TODO
#' @param lat TODO
#' @export
st_crs_utm_from_lonlat <- function(lon, lat = 45) {
  st_crs2(paste0(
    floor((lon + 180) / 6) + 1,     # UTM band
    if (lat >= 0) {"N"} else {"S"}  # North-South
  ))
}

# transform an sf object in the proper UTM zone
#' @name st_transform_utm
#' @rdname st_crs_utm
#' @description `st_transform_utm` transforms an sf object in the proper UTM zone
#' @importFrom sf st_transform
#' @export
st_transform_utm <- function(x) {
  st_transform(x, st_crs_utm(x))
}
