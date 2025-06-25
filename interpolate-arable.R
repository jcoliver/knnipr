# Interpolate precipitation data with kNN, with inverse distance weighting
# Jeff Oliver
# jcoliver@arizona.edu
# 2025-06-25

library(terra)
library(dplyr)
library(tidyr)
source(file = "functions.R")

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

# Do interpolation with k = 6, idw = TRUE
rain_imputed <- sapply(X = 1:ncol(rain_mat),
                       FUN = function(j, rain_matrix, distance_matrix, k) {
                         apply_knn(pd = rain_matrix[, j],
                                   dma = distance_matrix,
                                   k = k,
                                   idw = TRUE)
                       }, 
                       rain_matrix = rain_mat,
                       distance_matrix = dist_mat,
                       k = 6)

# Let's take that rain_imputed thing and use it to fill in missing values 
# rain_mat
rain_complete <- rain_mat
rain_complete[is.na(rain_mat)] <- rain_imputed[is.na(rain_mat)]

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

# Write to file
write.csv(x = rain_long, 
          row.names = FALSE,
          file = "azdata/daily_arable_imputed.csv")