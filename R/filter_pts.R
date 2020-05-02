#' @title Update filter values
#' @description `filter_pts` updates filter values (altering the original dataframe).
#' @param indata TODO
#' @param metric one between f_rangev, f_rangey, f_zscorey, f_rbiasy, f_rangeq, f_pos, f_editmap, f_selpts
#' @param value value or values to apply (depending on metric)
#' @param inlayer optional: sf of fields (see also `id_fieldname`)
#' @param id_fieldname optional: name of the `inlayer` field containing unique ID of fields (default: "idfield")
#' @param byfield if FALSE (default), consider data as a unique field; if TRUE, iterate each filter on field `id_fieldname`
#' @param reverse if TRUE, the filter is applied reversed (this makes sense only for `f_editmap` and `f_selpts`). default is FALSE.
#' @param samplesize maximum size of the sample of the original data to work with (default: 100000; if NA: all the points)
#' @param sid character: name of the ID field to be used (`"uid"`, `"sid1"`, `"sid2"`, `"sid3"` or `"sid4"`)
#' @param par1 additional parameter for filtering (now used only for area to filter / not to filter NA values)
#' @import data.table
#' @importFrom data.table setkey
#' @importFrom methods is
#' @importFrom stats sd
#' @importFrom sf st_crs st_transform st_contains st_sfc st_polygon
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
  sid = "sid3",
  par1 = NA
) {

  # Check samplesize
  if (is.na(samplesize)) {samplesize <- Inf}

  # Check input data
  if (any(
    !is(indata, "data.table"),
    !all(c("uid","sid1","sid2","sid3","sid4","lat","lon","idfield","selvar","f_rangev","f_rangey","f_zscorey","f_rbiasy","f_rangeq","f_pos","f_editmap", "f_selpts","filter") %in% names(indata))
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
      indata[get(sid) <= samplesize,list(sid=get(sid),lon,lat,idfield)],
      coords = c("lon","lat"),
      crs = 4326
    )
  }

  # Duplicate indata (so to can use ":=")
  # outdata <- copy(indata)
  outdata <- indata # FIXME this alter original dataset!
  # Apply filter
  if (metric == "rangev") {
    outdata[get(sid) <= samplesize & (speed < value[1] | speed > value[2]), f_rangev := !reverse]
    outdata[get(sid) <= samplesize & !(speed < value[1] | speed > value[2]), f_rangev := reverse]

  } else if (metric == "rangey") {
    outdata[get(sid) <= samplesize & (selvar < value[1] | selvar > value[2]), f_rangey := !reverse]
    outdata[get(sid) <= samplesize & !(selvar < value[1] | selvar > value[2]), f_rangey := reverse]

  } else if (metric == "zscorey") {
    if (byfield) {
      setkey(outdata,"idfield")
      for (sel_field in unique(outdata$idfield)) {
        outdata_avg <- outdata[idfield == sel_field & get(sid) <= samplesize, mean(selvar,na.rm = TRUE)]
        outdata_sd <- outdata[idfield == sel_field & get(sid) <= samplesize, sd(selvar,na.rm = TRUE)]
        outdata[idfield == sel_field & get(sid) <= samplesize & ((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := !reverse]
        outdata[idfield == sel_field & get(sid) <= samplesize & !((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := reverse]
      }
    } else {
      outdata_avg <- outdata[get(sid) <= samplesize, mean(selvar,na.rm = TRUE)]
      outdata_sd <- outdata[get(sid) <= samplesize, sd(selvar,na.rm = TRUE)]
      outdata[get(sid) <= samplesize & ((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := !reverse]
      outdata[get(sid) <= samplesize & !((selvar - outdata_avg)/outdata_sd < value[1] | (selvar - outdata_avg)/outdata_sd > value[2]), f_zscorey := reverse]
    }

  } else if (metric == "rbiasy") {
    if (byfield) {
      setkey(outdata,idfield)
      for (sel_field in unique(outdata$idfield)) {
        outdata_avg <- outdata[idfield == sel_field & get(sid) <= samplesize, mean(selvar,na.rm = TRUE)]
        outdata[idfield == sel_field & get(sid) <= samplesize & ((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := !reverse]
        outdata[idfield == sel_field & get(sid) <= samplesize & !((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := reverse]
      }
    } else {
      outdata_avg <- outdata[get(sid) <= samplesize, mean(selvar,na.rm = TRUE)]
      outdata[get(sid) <= samplesize & ((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := !reverse]
      outdata[get(sid) <= samplesize & !((selvar - outdata_avg)/outdata_avg < value[1] | (selvar - outdata_avg)/outdata_avg > value[2]), f_rbiasy := reverse]
    }

  } else if (metric == "rangeq") {

    if (byfield) {
      setkey(outdata,"idfield")
      for (sel_field in unique(outdata$idfield)) {
        outdata_rangeq <- outdata[idfield == sel_field & get(sid) <= samplesize, quantile(selvar,value,na.rm = TRUE)]
        outdata[idfield == sel_field & get(sid) <= samplesize & (selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := !reverse]
        outdata[idfield == sel_field & get(sid) <= samplesize & !(selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := reverse]
      }
    } else {
      outdata_rangeq <- outdata[get(sid) <= samplesize, quantile(selvar,value,na.rm = TRUE)]
      outdata[get(sid) <= samplesize & (selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := !reverse]
      outdata[get(sid) <= samplesize & !(selvar < outdata_rangeq[1] | selvar > outdata_rangeq[2]), f_rangeq := reverse]
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
          outdata_pos_allpts <- outdata_pos[match(outdata[[sid]],outdata_sf$sid)]
          outdata[
            get(sid) <= samplesize & idfield == sel_field &
              outdata_pos_allpts & !is.na(outdata_pos_allpts),
            f_pos := !reverse
            ]
          outdata[
            get(sid) <= samplesize & idfield == sel_field &
              !outdata_pos_allpts & !is.na(outdata_pos_allpts),
            f_pos := reverse
            ]
        } else {
          outdata[idfield == sel_field & get(sid) <= samplesize, f_pos := !reverse]
        }
      }
    } else {
      outdata_pos <- !colSums(suppressMessages(
        st_contains(inlayer_buffer, outdata_sf, sparse = FALSE)
      ))
      outdata_pos_allpts <- outdata_pos[match(outdata[[sid]],outdata_sf$sid)]
      outdata[
        get(sid) <= samplesize &
          outdata_pos_allpts & !is.na(outdata_pos_allpts),
        f_pos := !reverse
        ]
      outdata[
        get(sid) <= samplesize &
          !outdata_pos_allpts & !is.na(outdata_pos_allpts),
        f_pos := reverse
        ]
    }

  } else if (metric == "editmap") {
    if (length(value) == 0) {
      value <- st_sfc(st_polygon(), crs = 4326)
    }
    if (st_crs(value) != st_crs(outdata_sf)) {
      outdata_sf <- st_transform(outdata_sf, st_crs(value))
    }
    outdata_pos <- !colSums(suppressMessages(
      st_contains(value, outdata_sf, sparse = FALSE)
    ))
    outdata_pos_allpts <- outdata_pos[match(outdata[[sid]],outdata_sf$sid)]
    outdata[
      get(sid) <= samplesize &
        outdata_pos_allpts & !is.na(outdata_pos_allpts),
      f_editmap := !reverse
      ]
    outdata[
      get(sid) <= samplesize &
        !outdata_pos_allpts & !is.na(outdata_pos_allpts),
      f_editmap := reverse
      ]

  } else if (metric == "selpts") {
    outdata[get(sid) <= samplesize & uid %in% value, f_selpts := reverse]
    outdata[get(sid) <= samplesize & !uid %in% value, f_selpts := !reverse]

  } else stop("Metric is not recognised.")

  # Update global filter
  outdata[get(sid) <= samplesize, filter := f_rangev|f_rangey|f_zscorey|f_rbiasy|f_rangeq|f_pos|f_editmap|f_selpts]

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
    outdata[,c("f_rangev","f_rangey","f_zscorey","f_rbiasy","f_rangeq","f_pos","f_editmap","f_selpts","filter") := as.list(rep(FALSE,9))]
  } else {
    # TODO check that "filters" contains only allowed values
    outdata[,paste0("f_",filters) := as.list(FALSE)]
    outdata[,filter := f_rangev|f_rangey|f_zscorey|f_rbiasy|f_rangeq|f_pos|f_editmap|f_selpts]
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
  outdata[,sid1 := sample(nrow(outdata))]
  outdata[,fid := frankv(sid1), by = idfield]
  outdata[,sid2 := frank(outdata,fid,sid1)]
  outdata[,divnum3 := fid / area]
  outdata[,sid3 := frank(outdata,divnum3,sid1)]
  outdata[,divnum4 := fid/max(fid), by = idfield]
  outdata[,sid4 := frank(outdata,divnum4,sid1)]
  outdata[,c("divnum3", "divnum4") := list(NULL,NULL)]
  setkey(outdata,sid3)
  return(outdata)
}
