# k Nearest Neighbor Interpolation of Precipitation

Interpolating precipitation data from geographically nearby sites.

## Dependencies

+ dplyr
+ terra
+ tidyr

## Description

Uses k-nearest neighbors to impute missing values for precipitation data. 
Neighbor distance is based on kilometers, and includes an option to base 
interpolation on arithmetic mean or using inverse distance weighting, where 
nearer neighbors are weighted higher than distant neighbors.

## Contents

+ functions.R: Pair of functions to help with vectorization of approach
+ interpolate-arable.R: Script to do the imputation of missing values based on 
interpolation with k = 6 using inverse distance weighting (this is the script 
that actually does the stuff of interest)
+ poc-az.R: Proof of concept work on real dataset and optimization of _k_
+ poc.R: Early proof of concept work

## Notes

The real-data scripts (poc-az.R and interpolate-arable.R), rely on the presence 
of a folder called "azdata" and a data file within that folder. To ensure these 
scripts work, start by creating this folder (e.g. `mkdir("azdata")`), then 
moving the data file (daily_arable_missings.csv) into the azdata folder.