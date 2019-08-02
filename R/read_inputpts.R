#' @title Read point data
#' @description TODO
#' @param input TODO
#' @param borders TODO
#' @param varname TODO
#' @param .shiny_session TODO
#' @param .shiny_pbar_id TODO
#' @import data.table
#' @importFrom data.table data.table fread rbindlist setkey
#' @importFrom sf st_read
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_pad
#' @export
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
    #     shinyWidgets::updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 50*which(input==x)/length(input))
    #   }
    #   y
    # }))
    # rawdata_sf$idfield <- NULL
    # borders_sf <- if (is.character(borders)) {
    #   st_read(borders)
    # } else {
    #   borders
    # }

  # error if rawdata_sf does not include varname
  if (is.null(input[[varname]])) {
    stop(paste0("The selected files do not include variable '",varname,"'."))
  } else {
    rawdata_sf <- dplyr::select(input, varname) %>%
      dplyr::rename(selvar = varname) %>%
      sf::st_transform(4326) # reproject to longlat
  }


  # add border ID to points
  rawdata_sf <- if (!is.null(borders)) {
    suppressMessages(
      rawdata_sf[,names(rawdata_sf)!="id_geom"] %>%
        sf::st_join(borders)
    ) %>% dplyr::filter(!is.na(id_geom))
  } else {
    dplyr::mutate(rawdata_sf, id_geom = 0)
  } %>%
    dplyr::filter(!is.na(selvar)) # remove points with empty varname

  # error if points do not intersect borders
  if (nrow(rawdata_sf)==0) {
    stop("Borders do not intersect points.")
  }


  # assign UID
  set.seed(24029) # to grant repetibility
  outdata <- data.table(rawdata_sf)[,list(
    uid = seq_len(nrow(rawdata_sf)),
    sid = sample(nrow(rawdata_sf)), # ID in raw order and sampled order
    lat = st_coordinates(st_transform(rawdata_sf$geometry,4326))[,"Y"],
    lon = st_coordinates(st_transform(rawdata_sf$geometry,4326))[,"X"],
    idfield = as.character(id_geom),
    selvar,
    f_rangev = FALSE, f_rangey = FALSE, f_zscorey = FALSE, f_rbiasy = FALSE,
    f_rangeq = FALSE, f_pos = FALSE, filter = FALSE
  )]
  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    shinyWidgets::updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 70)
  }

  # Update progress bar
  if (!is.null(c(.shiny_session, .shiny_pbar_id))) {
    shinyWidgets::updateProgressBar(session = .shiny_session, id = .shiny_pbar_id, value = 100)
    Sys.sleep(0.5)
  }

  # end of custom_ibf format

  setkey(outdata,sid)
  return(outdata) #temp

}
