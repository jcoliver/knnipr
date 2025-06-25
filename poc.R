# Initial proof of concept
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-06-20

# Want to interpolate (impute) missing precipitation data from geographically 
# nearby sites with precipitation data

# This script includes early development stuff

# TODO: How will distance ties be resolved? (currently "broken" in order of how 
# sites are ordered in distance matrix - if elements 2 and 4 are ties, and we 
# only need one, element 2 is always chosen)

# 1. Once we have the precip data, drop missing rows
# 2. Sort by distance
# 3. Slice top k rows
# 4. Calculate average based on those top k rows

precip_data <- c(0, 1, 2, 1)
distance_mat <- matrix(data = c(NA, 0.8, 1, 2.8,
                                0.8, NA, 0.8, 2.8,
                                1, 0.8, NA, 2,
                                2.8, 2.8, 2, NA),
                       byrow = TRUE,
                       ncol = 4)

# Just trying for row 3
i <- 3
dists_i <- distance_mat[, i]
# One way, create a data frame with values (precip) and distances
# Or use order on the precip_data values
# TODO: We are assuming distance matrix diagonal is NA (not 0)
# Note order function lists NA elements last (with default na.last = TRUE)
precip_ordered <- precip_data[order(dists_i)]
# precip_ordered <- precip_data[order(dists_i)]

k <- 1
mean(precip_ordered[1:k])
# 1
k <- 2
mean(precip_ordered[1:k])
# 0.5
k <- 3
mean(precip_ordered[1:k])
# 0.67

# Now how to do this on all values in data vector?
# sapply is nice
sapply(X = precip_data, FUN = function(x) {return(x * 10)})

# Is X in this case a vector of 1...n indexes?
precip_interp <- sapply(X = 1:length(precip_data), 
                        FUN = function(x, pd, dma, k) {
                          dists_i <- dma[, x]
                          precip_ordered <- pd[order(dists_i)]
                          return(mean(precip_ordered[1:k]))
                        }, 
                        pd = precip_data,
                        dma = distance_mat, 
                        k = 3)
# WTF, it works


# Calculate geographic distance from coordinates
# Note these are LONGITUDE, then LATITUDE, not the typical lat, long order
lat_lon_mat <- matrix(data = c(33.77, 2.26,
                               33.24, 2.34,
                               32.28, 1.98,
                               33.76, 0.64),
                      byrow = TRUE,
                      ncol = 2)

# Have to coerce to a matrix to get a symmetrical distance matrix back (other-
# wise it returns object of type dist)
distance_mat <- as.matrix(terra::distance(x = lat_lon_mat,
                                          unit = "km",
                                          lonlat = TRUE,
                                          symmetrical = TRUE))
# use diag to set diagonal to NA
diag(distance_mat) <- NA

# Use the same precipitation vector with these geographic coordinates for testing
precip_interp <- sapply(X = 1:length(precip_data), 
                        FUN = function(x, pd, dma, k) {
                          dists_i <- dma[, x]
                          precip_ordered <- pd[order(dists_i)]
                          return(mean(precip_ordered[1:k]))
                        }, 
                        pd = precip_data,
                        dma = distance_mat, 
                        k = 3)

# Calculate RMSE
sqrt(mean((precip_data - precip_interp)^2))

# Try it with functions
source(file = "functions.R")
precip_interp <- apply_knn(pd = precip_data, dma = distance_mat, k = 3)
# Looks good!
