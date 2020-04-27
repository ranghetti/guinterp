#' @title Update filter values
#' @description `filter_pts` updates filter values (altering the original dataframe).
#' @param indata TODO
#' @param metric one between f_rangev, f_rangey, f_zscorey, f_rbiasy, f_rangeq, f_pos, f_editmap
#' @param value value or values to apply (depending on metric)
#' @param inlayer optional: sf of fields (see also `id_fieldname`)
#' @param id_fieldname optional: name of the `inlayer` field containing unique ID of fields (default: "idfield")
#' @param byfield if FALSE (default), consider data as a unique field; if TRUE, iterate each filter on field `id_fieldname`
#' @param reverse if TRUE, the filter is applied reversed (this makes sense only for `f_editmap`). default is FALSE.
#' @param samplesize maximum size of the sample of the original data to work with (default: 100000; if NA: all the points)
#' @param par1 additional parameter for filtering (now used only for area to filter / not to filter NA values)
#' @import data.table
#' @importFrom data.table setkey
#' @importFrom methods is
#' @importFrom stats sd
#' @importFrom sf st_crs st_transform st_contains
#' @export
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

filter_pts <- function(
  indata,
  metric,
  value,
  inlayer = NULL,
  id_fieldname = "idfield",
  byfield = FALSE,
  reverse = FALSE,
  samplesize = 1E5,
  par1 = NA
) {

  # Check samplesize
  if (is.na(samplesize)) {samplesize <- Inf}

  # Check input data
  if (any(
    !is(indata, "data.table"),
    !all(c("uid","sid","lat","lon","idfield","selvar","f_rangev","f_rangey","f_zscorey","f_rbiasy","f_rangeq","f_pos","f_editmap", "filter") %in% names(indata))
  )) {
    stop("The input object is not a valid data.table.")
  }
  if (!is.null(inlayer) & !is(inlayer, "sf")) {
    stop("The inlayer object is not a valid sf object")
  }

  if (byfield) {
    if (is.null(inlayer[[id_fieldname]])) {
      stop("The inlayer shapefile must contain a field with field IDs (argument \"id_fieldname\").")
    }
  }

  # Convert to sf
  if (metric %in% c("pos", "editmap")) {
    outdata_sf <- st_as_sf(
      indata[sid <= samplesize,list(sid,lon,lat,idfield)],
      coords = c("lon","lat"),
      crs = 4326
    )
  }

  # Duplicate indata (so to can use ":=")
  # outdata <- copy(indata)
  outdata <- indata # FIXME this alter original dataset!
  # Apply filter
  if (metric == "rangev") {
    outdata[sid <= samplesize & (speed < value[1] | speed > value[2]), f_rangev := !reverse]
    outdata[sid <= samplesize & !(speed < value[1] | speed > value[2]), f_rangev := reverse]

  } else if (metric == "rangey") {
    outdata[sid <= samplesize & (selvar < value[1] | selvar > value[2]), f_rangey := !reverse]
    outdata[sid <= samplesize & !(selvar < value[1] | selvar > value[2]), f_rangey := reverse]

  } else if (metric == "zscorey") {
    if (byfield) {
      setkey(outdata,"idfield")
      for (sel_field in unique(outdata$idfield)) {
        outdata_avg <- outdata[idfield == sel_field & sid <= samplesize, mean(selvar,na.rm = TRUE)]
        outdata_sd <- outdata[idfield == sel_field & sid <= samplesize, sd(selvar,na.rm = TRUE)]
        outdata[idfield == sel_field & sid <= samplesize & ((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := !reverse]
        outdata[idfield == sel_field & sid <= samplesize & !((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := reverse]
      }
    } else {
      outdata_avg <- outdata[sid <= samplesize, mean(selvar,na.rm = TRUE)]
      outdata_sd <- outdata[sid <= samplesize, sd(selvar,na.rm = TRUE)]
      outdata[sid <= samplesize & ((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := !reverse]
      outdata[sid <= samplesize & !((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := reverse]
    }

  } else if (metric == "rbiasy") {
    if (byfield) {
      setkey(outdata,idfield)
      for (sel_field in unique(outdata$idfield)) {
        outdata_avg <- outdata[idfield == sel_field & sid <= samplesize, mean(selvar,na.rm = TRUE)]
        outdata[idfield == sel_field & sid <= samplesize & ((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := !reverse]
        outdata[idfield == sel_field & sid <= samplesize & !((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := reverse]
      }
    } else {
      outdata_avg <- outdata[sid <= samplesize, mean(selvar,na.rm = TRUE)]
      outdata[sid <= samplesize & ((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := !reverse]
      outdata[sid <= samplesize & !((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := reverse]
    }

  } else if (metric == "rangeq") {

    if (byfield) {
      setkey(outdata,"idfield")
      for (sel_field in unique(outdata$idfield)) {
        outdata_rangeq <- outdata[idfield == sel_field & sid <= samplesize, quantile(selvar,value,na.rm = TRUE)]
        outdata[idfield == sel_field & sid <= samplesize & (selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := !reverse]
        outdata[idfield == sel_field & sid <= samplesize & !(selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := reverse]
      }
    } else {
      outdata_rangeq <- outdata[sid <= samplesize, quantile(selvar,value,na.rm = TRUE)]
      outdata[sid <= samplesize & (selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := !reverse]
      outdata[sid <= samplesize & !(selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := reverse]
    }

  } else if (metric == "pos") {
    inlayer_buffer <- st_buffer_m(inlayer, -value)
    if (st_crs(inlayer_buffer) != st_crs(outdata_sf)) {
      outdata_sf <- st_transform(outdata_sf, st_crs(inlayer_buffer))
    }

    if (byfield) {
      setkey(outdata,"idfield")
      for (sel_field in unique(outdata$idfield)) {
        sel_inlayer <- inlayer_buffer[inlayer_buffer[[id_fieldname]] == sel_field,]
        if (nrow(sel_inlayer) > 0) {
          outdata_pos <- !colSums(suppressMessages(
            st_contains(sel_inlayer, outdata_sf, sparse = FALSE)
          ))
          outdata_pos_allpts <- outdata_pos[match(outdata$sid,outdata_sf$sid)]
          outdata[
            sid <= samplesize & idfield == sel_field &
              outdata_pos_allpts & !is.na(outdata_pos_allpts),
            f_pos := !reverse
            ]
          outdata[
            sid <= samplesize & idfield == sel_field &
              !outdata_pos_allpts & !is.na(outdata_pos_allpts),
            f_pos := reverse
            ]
        } else {
          outdata[idfield == sel_field & sid <= samplesize, f_pos := !reverse]
        }
      }
    } else {
      outdata_pos <- !colSums(suppressMessages(
        st_contains(inlayer_buffer, outdata_sf, sparse = FALSE)
      ))
      outdata_pos_allpts <- outdata_pos[match(outdata$sid,outdata_sf$sid)]
      outdata[
        sid <= samplesize &
          outdata_pos_allpts & !is.na(outdata_pos_allpts),
        f_pos := !reverse
        ]
      outdata[
        sid <= samplesize &
          !outdata_pos_allpts & !is.na(outdata_pos_allpts),
        f_pos := reverse
        ]
    }

  } else if (metric == "editmap") {
    if (is.null(value)) {
      value <- sf::st_sfc(sf::st_polygon(), crs = 4326)
    }
    if (st_crs(value) != st_crs(outdata_sf)) {
      outdata_sf <- st_transform(outdata_sf, st_crs(value))
    }
    outdata_pos <- !colSums(suppressMessages(
      st_contains(value, outdata_sf, sparse = FALSE)
    ))
    outdata_pos_allpts <- outdata_pos[match(outdata$sid,outdata_sf$sid)]
    outdata[
      sid <= samplesize &
        outdata_pos_allpts & !is.na(outdata_pos_allpts),
      f_editmap := !reverse
      ]
    outdata[
      sid <= samplesize &
        !outdata_pos_allpts & !is.na(outdata_pos_allpts),
      f_editmap := reverse
      ]

  } else stop("Metric is not recognised.")

  # Update global filter
  outdata[sid <= samplesize, filter := f_rangev|f_rangey|f_zscorey|f_rbiasy|f_rangeq|f_pos|f_editmap]

  return(outdata)

}


#' @name filter_pts_reset
#' @rdname filter_pts
#' @description `filter_pts_reset` resets all filter values to FALSE (altering the original dataframe).
#' @param filters TODO
#' @import data.table
#' @export

filter_pts_reset <- function(indata, filters = NA) {
  outdata <- indata # no effect
  if (is.na(filters)) { # if NA, reset all; otherwise, only specified filters
    outdata[,c("f_rangev","f_rangey","f_zscorey","f_rbiasy","f_rangeq","f_pos","f_editmap","filter") := as.list(rep(FALSE,8))]
  } else {
    # TODO check that "filters" contains only allowed values
    outdata[,paste0("f_",filters) := as.list(FALSE)]
    outdata[,filter := f_rangev|f_rangey|f_zscorey|f_rbiasy|f_rangeq|f_pos|f_editmap]
  }
  return(outdata)
}


#' @name filter_pts_resample
#' @rdname filter_pts
#' @description `filter_pts_resample` resets sampling order (altering the original dataframe).
#' @import data.table
#' @importFrom data.table setkey
#' @export

filter_pts_resample <- function(indata) {
  outdata <- indata # no effect
  outdata[,sid := sample(sid)]
  setkey(outdata,sid)
  return(outdata)
}
