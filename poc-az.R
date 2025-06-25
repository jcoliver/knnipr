# Testing on some real data
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-06-23

library(terra)
library(dplyr)
library(tidyr)
source(file = "functions.R")

# Code below (and in functions.R) may be reinvention of approaches listed at 
# https://rspatial.org/analysis/4-interpolation.html and
# https://r-spatial.org/book/12-Interpolation.html

# arable_barcode     lat      lon      date rain_arable
#        C012632 1.97409 32.37736 10aug2023          NA

# Start by preparing data into
# 1. Distance matrix. One row & column for each site. Symmetrical.
# 2. Rain data matrix. One row per site, one column per day. Indexed in same 
# order as Distance matrix (can make arable_barcode the rownames)

arable_data <- read.csv(file = "azdata/daily_arable_missings.csv")

# 1. Distance matrix
# Pull out site data (id & coordinates)
sites <- arable_data %>%
  select(arable_barcode, lat, lon) %>%
  distinct() %>%
  arrange(arable_barcode)

# Calculate distance matrix
dist_mat <- as.matrix(terra::distance(x = sites %>% select(lon, lat),
                                      unit = "km",
                                      lonlat = TRUE,
                                      symmetrical = TRUE))
# Set diagonal to missing and set row & column names to id
diag(dist_mat) <- NA
rownames(dist_mat) <- colnames(dist_mat) <- sites$arable_barcode

# 2. Rain data. Want it to be wide, one column per day, make sure rows are 
# ordered in same order as distance matrix!
rain_data <- arable_data %>%
  select(arable_barcode, date, rain_arable) %>%
  pivot_wider(id_cols = arable_barcode,
              names_from = date,
              values_from = rain_arable)

# Pull out just the rain data as numeric matrix (drop the id column)
rain_mat <- as.matrix(rain_data[, -1])
# Add id as row name data
rownames(rain_mat) <- rain_data$arable_barcode
# Ensure rain data is same order as distance matrix
rain_mat <- rain_mat[rownames(dist_mat), ]

# Do calculations on all days
# Just try arithmetic mean with k = 4
all_days <- sapply(X = 1:ncol(rain_mat),
                   FUN = function(j, rain_matrix, distance_matrix, k) {
                     apply_knn(pd = rain_matrix[, j],
                               dma = distance_matrix,
                               k = k)
                   }, 
                   rain_matrix = rain_mat,
                   distance_matrix = dist_mat,
                   k = 4)

# Now use idw
all_days_idw <- sapply(X = 1:ncol(rain_mat),
                       FUN = function(j, rain_matrix, distance_matrix, k) {
                         apply_knn(pd = rain_matrix[, j],
                                   dma = distance_matrix,
                                   k = k,
                                   idw = TRUE)
                       }, 
                       rain_matrix = rain_mat,
                       distance_matrix = dist_mat,
                       k = 4)

# Try different values of k for each approach
# Arithmetic mean
for (k in 1:10) {
  rain_int <- sapply(X = 1:ncol(rain_mat),
                     FUN = function(j, rain_matrix, distance_matrix, k) {
                       apply_knn(pd = rain_matrix[, j],
                                 dma = distance_matrix,
                                 k = k)
                     }, 
                     rain_matrix = rain_mat,
                     distance_matrix = dist_mat,
                     k = k)
  rmse <- sqrt(mean((rain_int - rain_mat)^2, na.rm = TRUE))
  message("For k = ", k, ", rmse = ", round(rmse, digits = 3))
}
# For k = 1, rmse = 6.449
# For k = 2, rmse = 5.84
# For k = 3, rmse = 5.645
# For k = 4, rmse = 5.628  <--- min
# For k = 5, rmse = 5.658
# For k = 6, rmse = 5.691
# For k = 7, rmse = 5.724
# For k = 8, rmse = 5.764
# For k = 9, rmse = 5.801
# For k = 10, rmse = 5.838

# Inverse distance weighting
for (k in 1:10) {
  rain_int <- sapply(X = 1:ncol(rain_mat),
                     FUN = function(j, rain_matrix, distance_matrix, k) {
                       apply_knn(pd = rain_matrix[, j],
                                 dma = distance_matrix,
                                 k = k,
                                 idw = TRUE)
                     }, 
                     rain_matrix = rain_mat,
                     distance_matrix = dist_mat,
                     k = k)
  rmse <- sqrt(mean((rain_int - rain_mat)^2, na.rm = TRUE))
  message("For k = ", k, ", rmse = ", round(rmse, digits = 3))
}
# For k = 1, rmse = 6.449
# For k = 2, rmse = 5.819
# For k = 3, rmse = 5.599
# For k = 4, rmse = 5.53
# For k = 5, rmse = 5.525
# For k = 6, rmse = 5.518  <--- min
# For k = 7, rmse = 5.52
# For k = 8, rmse = 5.529
# For k = 9, rmse = 5.538
# For k = 10, rmse = 5.548

# So, to minimize RMSE, use k = 6 and inverse distance weighting