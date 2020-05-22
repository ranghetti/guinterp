#' @title Parallelise kriging
#' @description TODO
#' @param formula TODO
#' @param locations TODO
#' @param newdata TODO
#' @param model TODO
#' @param n_cores TODO
#' @param method TODO
#' @param nmax TODO
#' @param maxdist TODO
#' @importFrom parallel clusterEvalQ clusterExport detectCores makeCluster parLapply stopCluster
#' @importFrom gstat idw krige
#' @importFrom stats as.formula
#' @importFrom methods is
#' @importFrom sf st_centroid st_coordinates st_as_sf
#' @importFrom stars st_rasterize
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note Based on https://gis.stackexchange.com/questions/237672/how-to-achieve-parallel-kriging-in-r-to-speed-up-the-process
#' @note License: GPL 3.0

krige_par <- function(
  formula,
  locations,
  newdata,
  model = NA,
  n_cores = NA,
  method = "krige",
  nmax = Inf,
  maxdist = Inf
) {

  # check that modl is defined f method is krige
  if (method=="krige" & anyNA(model)) {
    stop("Argument 'model' is mandatory with method=\"krige\".")
  }

  # set default n_cores value
  if (is.na(n_cores)) {
    n_cores <- if (nrow(locations) < 1000) {
      1
    } else {
      min(detectCores() - 1, 16) # use at most 16 cores
    }
  }
  if (!is(n_cores,"numeric") | n_cores < 1) {
    warning("Invalid 'n_cores' value; using a default value.")
    n_cores <- min(detectCores() - 1, 16) # use at most 16 cores
  } else {
    n_cores <- as.integer(n_cores)
  }

  # check if overlapping points are present
  if (sum(duplicated(st_coordinates(locations)))>0) {
    warning("Some overlapping points are present; only the first one will be considered.")
    locations <- locations[!duplicated(st_coordinates(locations)),]
  }

  # workaround not to get error with st_rasterize
  if (any(c(st_dimensions(newdata)[[1]]$from, st_dimensions(newdata)[[2]]$from) > 1)) {
    newdata <- newdata %>% as("Raster") %>% st_as_stars()
  }

  # fix different crs
  if (all(
    !is.na(st_crs(newdata)$epsg) & !is.na(st_crs(locations)$epsg),
    st_crs(newdata)$epsg == st_crs(locations)$epsg,
    st_crs(newdata) != st_crs(locations)
  )) {
    st_crs(newdata) <- st_crs(locations)
  }

  # run singlecore if n_cores==1, multicore elsewhere
  if (as.integer(n_cores) == 1) {
      # singlecore
    if (method == "krige") {
      out_krig <- krige(
        formula = as.formula(formula),
        locations = locations,
        newdata = newdata,
        model = model,
        nmax = as.numeric(nmax),
        maxdist = as.numeric(maxdist)
      )
    } else if (method == "idw") {
      idw(
        formula = as.formula(formula),
        locations = locations,
        newdata = newdata,
        nmax = as.numeric(nmax),
        maxdist = as.numeric(maxdist)
      )
    } else {
      stop(paste0("The method \"",method,"\" is not recognised."))
    }

  } else {

    # multicore

    newdata_pt <- suppressWarnings(st_as_sf(newdata) %>% st_centroid())
    parts <- split(
      x = seq_len(nrow(newdata_pt)),
      f = sort(rep_len(seq_len(n_cores),nrow(newdata_pt)))
    )
    newdata_parts <- lapply(parts, function(part) {newdata_pt[part,]})

    cl <- makeCluster(
      n_cores, outfile = file.path(tempdir(),"out_krige.txt"),
      type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
    )

    clusterEvalQ(cl = cl, expr = c(library(sf), library(stars), library(gstat)))
    clusterExport(
      cl,
      c("locations", "formula", "model", "nmax", "maxdist", "method"),
      envir=environment()
    )
    parallelX <- parLapply(
      cl = cl,
      X = newdata_parts,
    # parallelX <- lapply(
    #   newdata_parts,
      function(sel_newdata) {
        if (method=="krige") {
          krige(
            formula = as.formula(formula),
            locations = locations,
            newdata = sel_newdata,
            model = model,
            nmax = as.numeric(nmax),
            maxdist = as.numeric(maxdist)
          )
        } else if (method=="idw") {
          idw(
            formula = as.formula(formula),
            locations = locations,
            newdata = sel_newdata,
            nmax = as.numeric(nmax),
            maxdist = as.numeric(maxdist)
          )
        } else {
          stop(paste0("The method \"",method,"\" is not recognised."))
        }

      }
    )
    stopCluster(cl)

    # Merge all the predictions
    mergeParallelX <- do.call("rbind", parallelX)

    # Create SpatialPixelsDataFrame from mergeParallelX
    st_rasterize(mergeParallelX, template = newdata)

  }

}
