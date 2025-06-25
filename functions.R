#' Calculate a kNN mean based on distance matrix
#' 
#' @param i integer index of data in \code{pd} to interpolate
#' @param pd numeric vector of data on which to base interpolation
#' @param dma numeric distance matrix; must be symmetric with diagonal values 
#' of \code{NA}
#' @param k integer number of neighbors for calculating mean
#' @param idw logical indicating whether or not to use inverse distance 
#' weighting when calculating the mean. If \code{FALSE} (default), calculates 
#' the arithmetic mean.
#' 
#' @return numeric interpolated mean value for the ith observation
#' 
#' @details
#' This function is designed to calculate a mean value for a single element in 
#' a vector. It should probably not be called on its own; instead use 
#' \code{apply_knn} in combination with \code{sapply} to perform calculations 
#' on an entire matrix. See examples in \code{sapply} documentation.
#' 
#' @examples
#' # Create precipitation data
#' precip_data <- c(0, 1, 2, 1)
#' # Create geographic coordinate matrix
#' lat_lon_mat <- matrix(data = c(33.77, 2.26,
#'                                33.24, 2.34,
#'                                32.28, 1.98,
#'                                33.76, 0.64),
#'                       byrow = TRUE,
#'                       ncol = 2)
#' # Calculate pairwise geographic distances in km
#' distance_mat <- as.matrix(terra::distance(x = lat_lon_mat,
#'                                           unit = "km",
#'                                           lonlat = TRUE, 
#'                                           symmetrical = TRUE))
#' # Set diagonal of distance matrix to be NA
#' diag(distance_mat) <- NA
#' # Calculate nearest neighbor mean for site 2 with k = 3
#' knn_interpolate(i = 2, pd = precip_data, dma = distance_mat, k = 3)
#' # Use inverse distance weighting on same site (2), with k = 2
#' knn_interpolate(i = 2, pd = precip_data, dma = distance_mat, k = 2, idw = TRUE)
knn_interpolate <- function(i, pd, dma, k = 5, idw = FALSE) {
  # Extract distances from the ith site
  dists_i <- dma[, i]
  # Reorder precipitation data on the basis of distance from ith site
  precip_ordered <- pd[order(dists_i)]
  # Identify indexes of missing data
  missing_indexes <- is.na(precip_ordered)
  # Drop any precipitation data that are missing
  precip_ordered <- precip_ordered[!missing_indexes]
  # To avoid index out of bounds, may need to reset k to length of data
  if (k > length(precip_ordered)) {
    k <- length(precip_ordered)
    warning("Length of data vector for site ", i, " is smaller than k; reset k to ", k)
  }
  # Calculate mean based on k closest sites that are not missing data
  if (idw) {
    # Using inverse distance weighting, so drop distances that corresponded to 
    # missing rain data (after sorting)
    dists_i <- dists_i[order(dists_i)]
    dists_i <- dists_i[!missing_indexes]
    # Use inverse of distances as weights
    inverse_dists_i <- 1/dists_i
    return(weighted.mean(x = precip_ordered[1:k], 
                         w = inverse_dists_i[1:k]))
  } else {
    # Just return arithmetic mean
    return(mean(precip_ordered[1:k]))
  }
}

#' Apply kNN interpolation over all elements of a vector
#' 
#' @param pd numeric vector of data on which to base interpolation
#' @param dma numeric distance matrix; must be symmetric with diagonal values 
#' of \code{NA}
#' @param k integer number of neighbors for calculating mean
#' @param idw logical indicating whether or not to use inverse distance 
#' weighting when calculating the mean. If \code{FALSE} (default), calculates 
#' the arithmetic mean.
#' 
#' @details
#' This function is really just a wrapper for knn_interpolate being called 
#' on each element of a vector via \code{sapply}
#' 
#' @return numeric vector of mean values, same length as \code{pd}
#' 
#' @examples
#' # Create precipitation data
#' precip_data <- c(0, 1, 2, 1)
#' # Create geographic coordinate matrix
#' lat_lon_mat <- matrix(data = c(33.77, 2.26,
#'                                33.24, 2.34,
#'                                32.28, 1.98,
#'                                33.76, 0.64),
#'                       byrow = TRUE,
#'                       ncol = 2)
#' # Calculate pairwise geographic distances in km
#' distance_mat <- as.matrix(terra::distance(x = lat_lon_mat,
#'                                           unit = "km",
#'                                           lonlat = TRUE, 
#'                                           symmetrical = TRUE))
#' # Set diagonal of distance matrix to be NA
#' diag(distance_mat) <- NA
#' # Calculate nearest neighbor mean for site 2 with k = 3
#' apply_knn(pd = precip_data, dma = distance_mat, k = 3)
#' # Use inverse distance weighting on same site (2), with k = 2
#' apply_knn(pd = precip_data, dma = distance_mat, k = 2, idw = TRUE)
#' # Can use sapply to make calculations on a matrix, instead of vector
#' # Create precipitation data (two columns, one column per date)
#' precip_data <- matrix(data = c(0, 0,
#'                                1, 0,
#'                                2, 1,
#'                                1, 0),
#'                       ncol = 2,
#'                       byrow = TRUE)
#' # Calculate arithmetic means on all sites, across columns, for k = 2
#' sapply(X = 1:ncol(precip_data),
#'        FUN = function(j, rain_matrix, distance_matrix, k) {
#'          apply_knn(pd = rain_matrix[, j],
#'                    dma = distance_matrix,
#'                    k = k)
#'          }, 
#'          rain_matrix = precip_data,
#'          distance_matrix = distance_mat,
#'          k = 2)
apply_knn <- function(pd, dma, k = 5, idw = FALSE) {
  # Start by making sure there is at least one non-NA value in pd vector, just 
  # return a vector of missing if so
  if (all(is.na(pd))) {
    return(rep(x = NA, length = length(pd)))
  } else {
    # At least some data, so proceed with calculations
    return(sapply(X = 1:length(pd),
                  FUN = knn_interpolate,
                  pd = pd,
                  dma = dma,
                  k = k,
                  idw = idw))
  }
}
