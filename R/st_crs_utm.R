# (see https://stackoverflow.com/questions/40132542/get-a-cartesian-projection-accurate-around-a-lat-lng-pair)

# give the UTM zone of a couple of geographic coordinates
st_crs_utm_from_lonlat <- function(lon, lat = 45) {
  st_crs2(paste0(
    floor((lon + 180) / 6) + 1,     # UTM band
    if (lat >= 0) {"N"} else {"S"}  # North-South
  ))
}

# give the UTM zone of an sf object
st_crs_utm <- function(x) {
  centre <- st_union(x) %>% st_centroid() %>%  # compute centroid
    st_transform(4326)                         # convert to lonlat
  out_crs <- st_crs_utm_from_lonlat(
    st_coordinates(centre)[,"X"],
    st_coordinates(centre)[,"Y"]
  )
}

# transform an sf object in the proper UTM zone
st_transform_utm <- function(x) {
  st_transform(x, st_crs_utm(x))
}
