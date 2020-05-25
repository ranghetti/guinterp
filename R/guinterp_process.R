#' @title Process a group of fields
#' @description This function calls other functions of the library to perform
#'  interpolation over different fields.
#' @param inputpts TODO
#' @param inlayer `SpatialPolygonsDataFrame` of fields (see also `id_fieldname`)
#' @param filtered logical: is `inputpts` already filtered? If TRUE (default),
#'  no additional filter is applied; if FALSE, a standard automatic filter is applied.
#' @param id_fieldname optional: name of the `inlayer` field containing unique ID of fields (default: `"idfield"`)
#' @param interp_dir directory where rasters of single polygons are stores (default: temporary directory)
#' @param out_path path of the output merged raster (default: temporary directory)
#' @param interp_method interpolation method (`"krige"` or `"idw"`)
#' @param smooth logical: should rasters be smoothed after interpolation?
#' @param interp_res output raster resolution (numeric, in metres)
#' @param out_crs optional: CRS of output raster (default: CRS of `inputpts`)
#' @param grid_offset optional: grid offset from point (0,0) in `out_crs` (2-length numeric)
#' @param samplesize maximum size of the sample of the original data to work with (default: 10000; if NA: all the points)
#' @param samplescheme sampling scheme within geometries (`"random"`, `"strat_npts"` or `"strat_area"`)
#' @param buffer_radius numeric: buffer (default: 15) to be applied internally to field borders.
#' @param parallel (optional) Logical or integer: if TRUE (default), interpolation
#'  is executed using multiple cores in order to speed up the execution.
#'  The number of cores is automatically determined; specifying it is also
#'  possible (e.g. `parallel = 4`).
#'  If FALSE, the interpolation is forced to run with a single core.
#' @param vgm named list of variograms (names must correspond to values of field `id_fieldname`).
#'  If NA (default), they are automatically computed.
#' @param v_nmax TODO.
#' @param v_maxdist TODO.
#' @param merge logical: if TRUE (default), merge raster of single fields and
#'  return the path of the merged raster; if FALSE, return the paths of the
#'  single field rasters.
#' @param overwrite Logical: should existing raster being reprocessed? (default: FALSE)
#' @param .shiny_session TODO
#' @param .shiny_pbar_id TODO
#' @import data.table
#' @importFrom gstat vgm fit.variogram variogram
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom raster writeRaster
#' @importFrom sf st_join st_as_sf st_area st_buffer st_crs st_transform
#'  st_union st_bbox st_intersection st_polygon st_sfc
#' @importFrom dplyr mutate filter group_by summarise
#' @importFrom shinyWidgets updateProgressBar
#' @importFrom stars read_stars
#' @importFrom stats var
#' @author Luigi Ranghetti, phD (2019) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


guinterp_process <- function(
  inputpts,
  inlayer,
  filtered = TRUE,
  id_fieldname="idfield",
  interp_dir = tempdir(),
  out_path = tempfile(),
  interp_method = "krige",
  smooth = TRUE,
  interp_res = 5,
  out_crs = NA,
  grid_offset = c(0,0),
  samplesize = 1E4,
  samplescheme = "random",
  buffer_radius = 15,
  parallel = TRUE,
  vgm = NA,
  v_nmax = Inf,
  v_maxdist = Inf,
  merge = TRUE,
  overwrite = TRUE,
  .shiny_session = NULL,
  .shiny_pbar_id = NULL
) {

  ## Check if all expected outputs already exists
  out_names <- file.path(
    interp_dir,
    paste0(
      "polygon_",
      unique(inputpts$idfield),".tif"
    )
  )
  if (overwrite == FALSE & sum(!file.exists(out_names)) == 0) {
    # Exit from the function because there is nothing to do
    return(out_names)
    # FIXME: this does not manage the situation merge = TRUE
  }

  ## Autofilter if not already filtered (filtered == FALSE)
  inputpts_f <- if (filtered == FALSE) {
    filter_pts(
      inputpts, "rangeq", c(.02,.98), # filter points < 2° and > 98° percentiles
      inlayer = inlayer,
      byfield = TRUE, samplesize = Inf
    )
  } else {inputpts}

  # internal conversion
  if (is.na(out_crs)) {
    out_crs <- st_crs_utm_from_lonlat(
      lon = inputpts_f[,mean(lon,na.rm=TRUE)],
      lat = inputpts_f[,mean(lat,na.rm=TRUE)]
    )
  }
  if (!is(out_crs, "crs")) {out_crs <- st_crs2(out_crs)}
  inputpts_sf <- inputpts_to_sf(
    inputpts_f,
    outcrs = out_crs,
    sid = switch(
      samplescheme,
      random = "sid1",
      strat_npts = "sid2",
      strat_area = "sid3",
      strat_prop = "sid4"
    )
  ) %>%
    st_transform(out_crs)

  inlayer <- st_transform(inlayer, out_crs)

  # remove points outside field borders
  which_nafield <- which(!is.na(inputpts_sf$idfield))
  if (length(which_nafield) != 0) {
    inputpts_sf <- inputpts_sf[!is.na(inputpts_sf$idfield), ]
  }

  # check which fileds should be used
  min_rel_area = 0.02 # minimum relative area (0-1) of the polygon covered by points in order to be interpolated
  max_pt_dist = if (buffer_radius<Inf) {buffer_radius*2} else {100} # maximum distance betweer points
  covered_poly <- st_join(
    st_as_sf(inputpts_sf),
    mutate(st_as_sf(inlayer), area_tot=st_area(st_as_sf(inlayer)))
  ) %>%
    filter(!is.na(id_geom)) %>%
    st_buffer(max_pt_dist) %>%
    group_by(id_geom, area_tot) %>%
    summarise() %>%
    st_buffer(-max_pt_dist)
  covered_poly$area_pt <- st_area(covered_poly)
  covered_poly$perc_cov <- covered_poly$area_pt/covered_poly$area_tot
  if (length(which_nafield) != 0) {
    inputpts_sf <- inputpts_sf[inputpts_sf$idfield %in% unique(covered_poly$id_geom), ]
  }
  ## Auto compute variograms if not provided
  if (!is.list(vgm)) {
    vgm_ <- vgm
    vgm <- list()
    for (a in names(attributes(vgm_))) {
      attr(vgm, a) <- attr(vgm_, a)
    }
    rm(vgm_)
    vgm_man <- vgm(
      psill = var(inputpts_sf$selvar),
      nugget = var(inputpts_sf$selvar)/5,
      model = if (!is.null(attr(vgm, "model"))) {attr(vgm, "model")} else {"Exp"},
      range = if (!is.null(attr(vgm, "range"))) {attr(vgm, "range")} else {
        with(as.list(st_bbox(inputpts_sf)), sqrt((xmax-xmin)^2+(ymax-ymin)^2))/3/5
      }
    )
    for (sel_field in unique(inputpts_sf$idfield)) {

      if (interp_method == "krige") {
        v <- variogram(
          selvar ~ 1,
          inputpts_sf[inputpts_sf$idfield == sel_field,],
          cutoff = ceiling(sqrt(sum(with(
            as.list(st_bbox(inputpts_sf[inputpts_sf$idfield == sel_field,])),
            c((xmax-xmin)^2, (ymax-ymin)^2)
          )))/3)
        )

        vgm[[sel_field]] <- fit.variogram(v, vgm_man, fit.sills = TRUE, fit.ranges = TRUE)
        # fix range if kept
        if (!is.null(attr(vgm, "range"))) {
          vgm[[sel_field]][2, "range"] <- attr(vgm, "range")
        }
      } else {
        vgm[[sel_field]] <- vgm_man # not used in IDW
      }
    }
  } else {
    # TODO controlla che vgm sia una named list, e che i nomi corrispondano ai livelli di idfield
  }

  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 5)
  }

  # Calcola n_cores
  n_cores <- if (is.numeric(parallel)) {
    as.integer(parallel)
  } else if (parallel==FALSE) {
    1
  } else {
    min(detectCores()-1, 16) # use at most 16 cores
  }
  if (n_cores <= 1) {
    `%DO%` <- `%do%`
    parallel <- FALSE
    n_cores <- 1
  } else {
    `%DO%` <- `%dopar%`
  }
  # ricalcolo di n_cores per le operazioni che vanno in parallelo sui campi
  # (es. Ritaglio sulle aree dove ci sono dati)
  n_cores_bypol <- min(n_cores, length(unique(inputpts_sf$idfield)))
  `%DO_bypol%` <- if (n_cores_bypol <= 1) {`%do%`} else {`%dopar%`}

  # Ritaglio sulle aree dove ci sono dati
  fieldbuffers <- if (is.finite(buffer_radius)) {
    if (n_cores_bypol > 1) {
      cl_bypol <- makeCluster(
        n_cores_bypol,
        type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
      )
      registerDoParallel(cl_bypol)
    }
    fieldbuffers_step3 <- foreach(
      f = unique(inputpts_sf$idfield),
      i = seq_along(unique(inputpts_sf$idfield)),
      .combine = c,
      .packages = c("shinyWidgets","sf","guinterp")
    ) %DO_bypol% {
      out_name <- paste0("polygon_",f,".tif")
      if ((overwrite == TRUE | !file.exists(file.path(interp_dir,out_name))) & sum(inputpts_sf$idfield==f)>0) {
        sel_inputpts_sf_b  <- buffer_f(inputpts_sf[inputpts_sf$idfield==f,], buffer_radius/2, 1) # rarefy points by distance
        sel_inputpts_sf_b1 <- st_buffer(st_union(sel_inputpts_sf_b), dist=buffer_radius+10) # overbuffer (10m)
        sel_inputpts_sf_b2 <- st_buffer(sel_inputpts_sf_b1, dist=-10) # underbuffer (-10m)
        # intersect buffer with field boundaries
        sel_inputpts_sf_b3 <- st_intersection(
          sel_inputpts_sf_b2,
          st_transform(inlayer[inlayer[[id_fieldname]]==f,], st_crs(sel_inputpts_sf_b2))
        )
      } else {
        sel_inputpts_sf_b3 <- st_sfc(st_polygon())
      }
      # Update progress bar
      if (!is.null(c(.shiny_session, .shiny_pbar_id)) & n_cores<=1) {
        updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 5+30*i/length(unique(inputpts_sf$idfield)))
      }
      sel_inputpts_sf_b3
    }
    if (n_cores_bypol > 1) {
      stopCluster(cl_bypol)
    }
    # crop polygons on buffers
    st_intersection(inlayer, st_transform(st_union(fieldbuffers_step3), st_crs(inlayer)))
  } else {
    inlayer[inlayer$id_geom %in% unique(inputpts_sf$idfield),]
  }

  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id)) & n_cores<=1) {
    updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 35)
  }

  ## Interpola
  glob_grid <- make_interp_grid(
    inputpts_sf,
    outres = interp_res,
    outcrs = out_crs,
    offset = grid_offset,
    border = interp_res*15
  )

  rasters_list <- foreach(f = unique(inputpts_sf$idfield), i = seq_along(unique(inputpts_sf$idfield))) %do% {
    field_name <- f
    out_name <- paste0("polygon_",f,".tif")
    if (overwrite == TRUE | !file.exists(file.path(interp_dir,out_name))) {
      message(paste(Sys.time(), field_name))

      vgm_sel <- if (is(vgm, "variogramModel")) {vgm} else {vgm[[f]]}
      interp_inputpts(
        inputpts_sf[inputpts_sf$idfield == f,],
        fieldbuffers[fieldbuffers$id_geom == field_name,],
        vgm_sel,
        glob_grid,
        border = 0,
        # focal_types = rep("Gauss",2),
        # focal_d = list(5,9),
        method = interp_method,
        focal_types = if (smooth) "Gauss" else NULL,
        focal_d = if (smooth) interp_res else NULL, # focal_d = 5,
        outname = out_name,
        outdir = interp_dir,
        n_cores = n_cores,
        samplesize = samplesize,
        nmax = v_nmax,
        maxdist = if (!is.na(v_maxdist)) {v_maxdist} else {vgm_sel[2,"range"]*1.5}
      )

      # move the proper file
      if (smooth) {
        file.rename(
          file.path(interp_dir, paste0("focal_Gauss",interp_res,"x",interp_res), out_name),
          file.path(interp_dir, out_name)
        )
        file.remove(file.path(interp_dir, paste0("focal_Gauss",interp_res,"x",interp_res)))
        file.remove(file.path(interp_dir, "interp", out_name))
        file.remove(file.path(interp_dir, "interp"))
      } else {
        file.rename(
          file.path(interp_dir, "interp", out_name),
          file.path(interp_dir, out_name)
        )
        file.remove(file.path(interp_dir, "interp"))
      }

    }
    # Update progress bar
    if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
      updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 35+60*i/length(unique(inputpts_sf$idfield)))
    }
    file.path(interp_dir, out_name) # existing files will be also considered for merging, in this way
  }

  # merge fields (if required)
  out_list <- if (merge == TRUE) {

    # # exclude automatically rasters with too high values
    # rasters <- lapply(rasters_list, raster)
    # rasters_avg <- lapply(rasters, function(x){mean(values(x),na.rm=TRUE)})
    # rasters_filtered <- rasters[rasters_avg<30]
    # rasters_excluded <- rasters[rasters_avg>=30]
    rasters_filtered <- lapply(rasters_list, raster)

    merged <- if (length(rasters_filtered) > 1) {
      do.call(raster::merge, rasters_filtered)
    } else {
      rasters_filtered[[1]]
    }
    writeRaster(
      merged,
      out_path,
      options=c("COMPRESS=DEFLATE"),
      NAflag = -32768,
      overwrite = TRUE
    )

    out_path

  } else {

    rasters_list

  }

  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 100)
  }

  out_list

}
