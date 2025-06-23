# Testing on some real data
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-06-23

library(terra)
library(dplyr)
library(tidyr)
source(file = "functions.R")

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

# Data are ready, use something like below, once for each day (column)
# precip_interp <- apply_knn(pd = precip_data, dma = distance_mat, k = 3)
day1 <- apply_knn(pd = rain_mat[, 1],
                  dma = dist_mat,
                  k = 3)

# Let's look at site 1 (row 1). What are non-missing rain values nearby?
dists_1 <- dist_mat[, 1]
precip_1 <- rain_mat[order(dists_1), 1]
precip_1_nomissing <- precip_1[!is.na(precip_1)]
# And how far away are these?
dists_1_nomissing <- dist_mat[names(precip_1_nomissing), 1]

# OK, seems fine? 

# Site 1 has only 13 "nearby" sites with non-missing data. We could add another
# condition that limits the maximum distance one could call a "neighbor"...

# How do we do this with all the data, i.e. once for each column, and assemble 
# it back into a matrix?
# Start with the dumb for loop way
rain_int <- matrix(data = NA, 
                   ncol = ncol(rain_mat),
                   nrow = nrow(rain_mat))
for (j in 1:ncol(rain_mat)) {
  day_j <- apply_knn(pd = rain_mat[, j],
                     dma = dist_mat,
                     k = 4)
  rain_int[, j] <- day_j
}

rmse <- sqrt(mean((rain_int - rain_mat)^2, na.rm = TRUE))

# Long way works. Do some k testing
for (k in 1:10) {
  rain_int <- matrix(data = NA, 
                     ncol = ncol(rain_mat),
                     nrow = nrow(rain_mat))
  for (j in 1:ncol(rain_mat)) {
    day_j <- apply_knn(pd = rain_mat[, j],
                       dma = dist_mat,
                       k = k)
    rain_int[, j] <- day_j
  }
  rmse <- sqrt(mean((rain_int - rain_mat)^2, na.rm = TRUE))
  message("For k = ", k, ", rmse = ", round(rmse, digits = 3))
}

# For k = 1, rmse = 6.449
# For k = 2, rmse = 5.84
# For k = 3, rmse = 5.645
# For k = 4, rmse = 5.628
# For k = 5, rmse = 5.658
# For k = 6, rmse = 5.691
# For k = 7, rmse = 5.724
# For k = 8, rmse = 5.764
# For k = 9, rmse = 5.801
# For k = 10, rmse = 5.838

# k = 4 minimizes rmse

# How about not dumb way?

all_days <- sapply(X = 1:ncol(rain_mat),
       FUN = function(j, rain_matrix, distance_matrix, k) {
         apply_knn(pd = rain_matrix[, j],
                   dma = distance_matrix,
                   k = k)
       }, 
       rain_matrix = rain_mat,
       distance_matrix = dist_mat,
       k = 4)

sum(rain_int - all_days, na.rm = TRUE)
sum(is.na(all_days))
# 80
sum(is.na(rain_int))
# 80
missings <- is.na(rain_mat)
column_missing <- colSums(missings)
# Oh, all sites are missing data for final day (31jul2024)

# Let's take that all_days thing and use it to fill in missing values rain_mat
rain_complete <- rain_mat
rain_complete[is.na(rain_mat)] <- all_days[is.na(rain_mat)]

# need to convert this back to long data, with site id
# start by making a data frame
rain_df <- data.frame(cbind(rownames(rain_complete), rain_complete))
rownames(rain_df) <- NULL
rain_long <- rain_df %>%
  pivot_longer(cols = -V1,
               names_to = "date",
               values_to = "rain_arable") %>%
  mutate(date = gsub(pattern = "X",
                     replacement = "",
                     x = date)) %>%
  rename(arable_barcode = V1)
head(rain_long)
head(rain_data)
