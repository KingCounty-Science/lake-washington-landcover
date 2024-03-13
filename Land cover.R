# Lake Washington watershed - land cover over time
# Daniel Nidzgorski
# March 13, 2024

library(tidyverse)
library(sf)
library(terra)
library(tictoc)

rm(list=ls())

# Lake Washington watershed
watershed<-st_read("Watershed/LakeWashingtonWatershed.shp") %>% 
  st_transform(crs="ESRI:102008")


# Initial test with NLCD 2021
nlcd<-rast("NLCD/NLCD_2021_Land_Cover_L48_20230630_nDr0hjPobjWPXcj5l81y.tiff") %>% 
  project("ESRI:102008")


# Crop and mask to watershed extent
n<-crop(nlcd,watershed, mask = T)
writeRaster(n,"test.tif", overwrite=TRUE)
