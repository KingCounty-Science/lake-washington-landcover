# Lake Washington watershed - land cover over time
# Daniel Nidzgorski
# March 13, 2024

library(tidyverse)
library(sf)
library(raster)
library(tictoc)

rm(list=ls())

# Lake Washington watershed
watershed<-st_read("Watershed/LakeWashingtonWatershed.shp") %>% 
  st_transform(crs="ESRI:102008")


# Initial test with NLCD 2021
nlcd<-raster("NLCD/NLCD_2021_Land_Cover_L48_20230630_nDr0hjPobjWPXcj5l81y.tiff")
projectRaster(crs = "ESRI:102008")


# Mask to watershed extent
n<-mask(nlcd,watershed,updatevalue = 0)
writeRaster(nlcd,"test2.tif",overwrite=T)
