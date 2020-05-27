#' @title Read point data
#' @description TODO
#' @param input TODO
#' @param borders TODO
#' @param varname TODO
#' @param .shiny_session TODO
#' @param .shiny_pbar_id TODO
#' @import data.table
#' @importFrom data.table data.table fread rbindlist setkey
#' @importFrom sf st_read st_geometry st_transform st_coordinates st_join
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select rename mutate filter
#' @importFrom shinyWidgets updateProgressBar
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

read_inputpts <- function(
  input, borders = NULL, varname = NA,
  .shiny_session = NULL, .shiny_pbar_id = NULL
) {

  # FIXME sposta in altra funzione
    # rawdata_sf <- do.call(rbind, lapply(input, function(x) {
    #   y <- st_read(x)
    #   # Update progress bar
    #   if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    #     updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 50*which(input==x)/length(input))
    #   }
    #   y
    # }))
    # rawdata_sf$idfield <- NULL
    # borders_sf <- if (is.character(borders)) {
    #   st_read(borders)
    # } else {
    #   borders
    # }

  # to avoid NOTE on check
  id_geom <- selvar <- area <- idfield <- sid3 <- NULL

  # error if rawdata_sf does not include varname
  if (is.null(input[[varname]])) {
    stop(paste0("The selected files do not include variable '",varname,"'."))
  } else {
    rawdata_sf <- select(input, varname) %>%
      rename(selvar = varname) %>%
      st_transform(4326) # reproject to longlat
  }


  # add border ID to points
  rawdata_sf <- if (!is.null(borders)) {
    suppressMessages(
      rawdata_sf[,names(rawdata_sf)!="id_geom"] %>%
        st_join(borders)
    ) %>% filter(!is.na(id_geom))
  } else {
    mutate(rawdata_sf, id_geom = 0)
  } %>%
    filter(!is.na(selvar)) # remove points with empty varname

  # error if points do not intersect borders
  if (nrow(rawdata_sf)==0) {
    stop("Borders do not intersect points.")
  }


  # assign UID and SIDs
  outdata <- data.table(rawdata_sf)[,list(
    uid = seq_len(nrow(rawdata_sf)), # ID in raw order
    fid = as.integer(NA), # ID in sampled order (field-specific)
    sid1 = as.integer(NA), # ID in sampled order
    sid2 = as.integer(NA), # ID in sampled order weighted by field
    sid3 = as.integer(NA), # ID in sampled order weighted by field area
    sid4 = as.integer(NA), # ID in sampled order (same # points per field)
    lat = st_coordinates(st_transform(st_geometry(rawdata_sf),4326))[,"Y"],
    lon = st_coordinates(st_transform(st_geometry(rawdata_sf),4326))[,"X"],
    idfield = as.character(id_geom),
    area = as.numeric(NA),
    selvar,
    f_rangev = FALSE, f_rangey = FALSE, f_zscorey = FALSE, f_rbiasy = FALSE,
    f_rangeq = FALSE, f_pos = FALSE, f_editmap = FALSE, filter = FALSE
  )]
  outdata[,area := if (length(unique(idfield))>1) {as.numeric(st_area(borders))[match(idfield, as.character(borders$id_geom))]} else {1}]
  set.seed(24029) # to grant repetibility
  invisible(filter_pts_resample(outdata))

  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 70)
  }

  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 100)
    Sys.sleep(0.5)
  }

  # end of custom_ibf format

  setkey(outdata,sid3)
  return(outdata) #temp

}
