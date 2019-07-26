#' @title Buffer points in XY space
#' @description Returns the original data table with buffered points removed.
#'
#' Runs numerous iterations, as the random point selection can result in more/fewer output points.
#'
#' 1. Randomly select a single point
#' 2. Remove points within 50km of that point
#' 3. Randomly select of the remaining points
#' @param foo a data.frame to select from with columns x, y
#' @param buffer the minimum distance between output points
#' @param reps the number of repetitions for the points selection
#' @importFrom methods is
#' @importFrom sp coordinates
#' @author David R. Roberts (2015)
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note Taken from https://davidrroberts.wordpress.com/2015/09/25/spatial-buffering-of-points-in-r-while-retaining-maximum-sample-size/

buffer_f <- function(foo, buffer, reps){

  # ranghetti addition
  if (is(foo, "SpatialPoints")) {
    foo$x <- coordinates(foo)[,1]
    foo$y <- coordinates(foo)[,2]
  } else if (is(foo, "sf")) {
    foo$x <- st_coordinates(foo)[,1]
    foo$y <- st_coordinates(foo)[,2]
  }

  # Make list of suitable vectors
  suitable <- list()
  for(k in 1:reps){
    # Make the output vector
    outvec <- as.numeric(c())
    # Make the vector of dropped (buffered out) points
    dropvec <- c()
    for(i in 1:nrow(foo)){
      # Stop running when all points exhausted
      if(length(dropvec)<nrow(foo)){
        # Set the rows to sample from
        if(i>1){
          rowsleft <- (1:nrow(foo))[-c(dropvec)]
        } else {
          rowsleft <- 1:nrow(foo)
        }
        # Randomly select point
        outpoint <- as.numeric(sample(as.character(rowsleft),1))
        outvec[i] <- outpoint
        # Remove points within buffer
        outcoord <- foo[outpoint,c("x","y")]
        dropvec <- c(dropvec, which(sqrt((foo$x-outcoord$x)^2 + (foo$y-outcoord$y)^2)<buffer))
        # Remove unnecessary duplicates in the buffered points
        dropvec <- dropvec[!duplicated(dropvec)]
      }
    }
    # Populate the suitable points list
    suitable[[k]] <- outvec
  }
  # Go through the iterations and pick a list with the most data
  best <- unlist(suitable[which.max(lapply(suitable,length))])
  foo[best,]

}
