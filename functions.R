#' Calculate kNN mean based on distance matrix
#' 
#' @param i integer index of data in \code{pd} to interpolate
#' @param pd numeric vector of data on which to base interpolation
#' @param dma numeric distance matrix; must be symmetric with diagonal values 
#' of \code{NA}
#' @param k integer number of neighbors for calculating mean
#' 
#' @return numeric interpolated mean value for the ith observation
knn_interpolate <- function(i, pd, dma, k = 5) {
  # Extract distances from the ith site
  dists_i <- dma[, i]
  # Reorder precipitation data on the basis of distance from ith site
  precip_ordered <- pd[order(dists_i)]
  # Drop any precipitation data that are missing
  precip_ordered <- na.omit(precip_ordered)
  # Calculate mean based on k closest sites that are not missing data
  return(mean(precip_ordered[1:k]))
}

#' Apply kNN interpolation over all elements of a vector
#' 
#' @param pd numeric vector of data on which to base interpolation
#' @param dma numeric distance matrix; must be symmetric with diagonal values 
#' of \code{NA}
#' @param k integer number of neighbors for calculating mean
#' 
#' @return numeric vector of mean values, same length as \code{pd}
apply_knn <- function(pd, dma, k = 5) {
  return(sapply(X = 1:length(pd),
                FUN = knn_interpolate,
                pd = pd,
                dma = dma,
                k = k))
}
