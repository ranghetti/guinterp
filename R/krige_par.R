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
#' @importFrom SearchTrees rectLookup knnLookup createTree
#' @importFrom sf st_centroid st_coordinates st_as_sf
#' @importFrom stars st_rasterize
#' @export
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

  # formula to krige a single pixel
  krig_fun <- function(in_pt, formula, out_locations, qtree) {
    # if (floor(ind / 500) - (ind/500) == 0) print(ind)
    which_neigh <- SearchTrees::rectLookup(
      qtree,
      xlim = c(st_coordinates(in_pt)[,1] - maxdist/2, st_coordinates(in_pt)[,1] + maxdist/2),
      ylim = c(st_coordinates(in_pt)[,2] - maxdist/2, st_coordinates(in_pt)[,2] + maxdist/2)
    )
    if (length(which_neigh) <= 50) {
      which_neigh <- SearchTrees::knnLookup(
        qtree, newdat = st_coordinates(in_pt),
        k = 50)[1,]
    }
    suppressMessages(gstat::krige(
      stats::as.formula(formula),
      out_locations[which_neigh,],
      in_pt,
      debug.level = 0
    ))
  }

  # check that modl is defined f method is krige
  if (method=="krige" & anyNA(model)) {
    stop("Argument 'model' is mandatory with method=\"krige\".")
  }

  # set default n_cores value
  if (is.na(n_cores)) {
    n_cores <- if (nrow(locations) < 1000) {
      1
    } else {
      min(parallel::detectCores() - 1, 16) # use at most 16 cores
    }
  }
  if (!is(n_cores,"numeric") | n_cores < 1) {
    warning("Invalid 'n_cores' value; using a default value.")
    n_cores <- min(parallel::detectCores() - 1, 16) # use at most 16 cores
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

  # # fix different crs
  # if (all(
  #   !is.na(st_crs(newdata)$epsg) & !is.na(st_crs(locations)$epsg),
  #   st_crs(newdata)$epsg == st_crs(locations)$epsg,
  #   st_crs(newdata)$proj4string != st_crs(locations)$proj4string
  # )) {
  #   st_crs(newdata) <- st_crs(locations)
  # }

  # krig_fun <- function(ind, newdata_krig) {
  #   if (floor(ind / 500) - (ind/500) == 0) print(ind)
  #   in_pt <- as(newdata_krig[ind, ], "SpatialPoints")
  # which_neigh <- t(SearchTrees::knnLookup(
  #   qtree, newdat = coordinates(in_pt),
  #   k = nmax))
  #   suppressMessages(krige(as.formula(formula),
  #                          locations[which_neigh[, 1],],
  #                          in_pt,
  #                          model,
  #                          debug.level = 0))
  # }
  # n_cores = 4
  # run singlecore if n_cores==1, multicore elsewhere
  if (as.integer(n_cores) == 1) {
      # singlecore
    if (method == "krige") {
      nmax  <- 200
      qtree <- SearchTrees::createTree(st_coordinates(locations))
      newdata_pt <- suppressWarnings(st_as_sf(newdata) %>% st_centroid())
      out_krig_list <- lapply(seq_len(nrow(newdata_pt)), FUN = function(x) {
        krig_fun(newdata_pt[x,], formula, locations, qtree)
      })
      # out_krig <- lapply(seq_len(dim(newdata)["x"]), function(x) {
      #   lapply(seq_len(dim(newdata)["y"]), function(y) {
      #     krig_fun(x,y)
      #   })
      # })
      out_krig <- do.call("rbind", out_krig_list)
      st_rasterize(out_krig["var1.pred"], template = newdata)
      # krige(as.formula(formula), locations, newdata, model, nmax=nmax, maxdist=maxdist)
    } else if (method == "idw") {
      gstat::idw(as.formula(formula), locations, newdata, nmax = nmax, maxdist = maxdist)
    } else {
      stop(paste0("The method \"",method,"\" is not recognised."))
    }

  } else {

    # multicore

    # Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)

    # avoid replicating newdata to newdata2!
    # newdata[["na"]] <- newdata[[1]]
    # newdata[["na"]][!is.na(newdata[["na"]])] <- NA
    # newdata <- newdata["na"]

    parts <- split(
      x = seq_len(nrow(newdata)),
      f = sort(rep_len(seq_len(n_cores),nrow(newdata)))
    )
    loc_small <- locations[,"selvar"]

    cl <- parallel::makeCluster(
      n_cores, outfile = file.path(tempdir(),"out_krige.txt"),
      type = if (Sys.info()["sysname"] == "Windows") {"PSOCK"} else {"FORK"}
    )

    # clusterExport(
    #   cl = cl,
    #   varlist = c("newdata", "model", "parts    ", "method", "nmax", "maxdist", "loc_small"),
    #   envir = environment()
    # )
    # cl <- makeCluster(4)
    parallel::clusterEvalQ(cl = cl, expr = c(library(sf), library(stars), library(gstat), library(SearchTrees)))
    parallelX <- parallel::parLapply(
      cl = cl,
      X = seq_len(n_cores),
    # parallelX <- lapply(
    #   seq_len(n_cores),
      function(x) {
        sel_row = parts[[x]]
        if (method=="krige") {
          qtree <- SearchTrees::createTree(st_coordinates(loc_small))
          nmax <- 200
          newdata_pt <- suppressWarnings(st_as_sf(newdata[,sel_row,]) %>% st_centroid())
          out_krig  <- lapply(seq_len(nrow(newdata_pt)), FUN = function(x) {
            krig_fun(newdata_pt[x,], formula, locations[which_neigh,], qtree)
          })
          out_krig  <- do.call("rbind", out_krig)
          out_krig
          # return(out_krig)
          # krige(as.formula(formula), locations, newdata[parts[[x]],], model, nmax=nmax, maxdist=maxdist)
        } else if (method=="idw") {
          gstat::idw(as.formula(formula), loc_small, newdata[,sel_row,], nmax=nmax, maxdist=maxdist)
        } else {
          stop(paste0("The method \"",method,"\" is not recognised."))
        }

      }
    )
    parallel::stopCluster(cl)

    # Merge all the predictions
    mergeParallelX <- do.call("rbind", parallelX)

    # Create SpatialPixelsDataFrame from mergeParallelX
    st_rasterize(mergeParallelX, template = newdata)

  }

}
