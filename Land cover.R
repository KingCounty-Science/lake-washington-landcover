# Lake Washington watershed - land cover over time
# Daniel Nidzgorski

library(sf)
library(terra)
library(tidyverse)


rm(list=ls())

# Lake Washington watershed boundary
watershed<-st_read("Watershed/LakeWashingtonWatershed.shp") %>% 
  st_transform(crs="ESRI:102008")

# Function to read in one year's NLCD data
# and crop and mask to watershed.
readNLCD<-function(filename,w) {
  
  year<-str_extract(filename,"\\d+")
  
  nlcd<-rast(filename) %>% 
    project("ESRI:102008")
  
  # Crop and mask to watershed extent
  n<-crop(nlcd,w, mask = T)
  
  d<-as.data.frame(n) %>% 
    table() %>% 
    as.data.frame() %>% 
    filter(Freq > 0) %>%
    mutate(Year = year)
  
  return(d)
  
}

## Impervious surface
nlcd.imp<-map_dfr(list.files(path="NLCD", pattern="Impervious.+tiff$",full.names = T), readNLCD, w=watershed)

# Layer_1 is the percent of impervious surface in that pixel.
# Directly converting it to numeric ends up off by one (1-101; row numbers??), but going via
# character works.
imp<-nlcd.imp %>% 
  mutate(PctImp = as.character(Layer_1),
         PctImp = as.numeric(PctImp), 
         ImpArea = PctImp * Freq / 100) %>% 
  # Sum up total impervious surface in the watershed for each year
  group_by(Year) %>% 
  summarize(Area = sum(ImpArea, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(LandCover = "Total Impervious")

## Land cover
nlcd.cover<-map_dfr(list.files(path="NLCD", pattern="Cover.+tiff$",full.names = T), readNLCD, w=watershed)

cover.full<-nlcd.cover %>% 
  # Translate numeric codes to names
  mutate(LandCoverFull = case_when(
    Layer_1 == 11 ~ "Water",
    Layer_1 == 21 ~ "Developed, Open space",
    Layer_1 == 22 ~ "Developed, Low intensity",
    Layer_1 == 23 ~ "Developed, Medium intensity",
    Layer_1 == 24 ~ "Developed, High intensity",
    Layer_1 == 41 ~ "Forest, Deciduous",
    Layer_1 == 42 ~ "Forest, Evergreen",
    Layer_1 == 43 ~ "Forest, Mixed",
    Layer_1 == 12 ~ "Perennial Ice/Snow",
    Layer_1 == 31 ~ "Barren land",
    Layer_1 == 52 ~ "Shrub/Scrub",
    Layer_1 == 71 ~ "Grasslands/Herbaceous",
    Layer_1 == 81 ~ "Pasture/Hay",
    Layer_1 == 82 ~ "Cultivated crops",
    Layer_1 == 90 ~ "Woody wetlands",
    Layer_1 == 95 ~ "Emergent herbaceous wetlands",
    .default = Layer_1),
    
    # Keep developed categories and water as-is.
    # Group together forests (deciduous, evergreen, mixed)
    # and all other land-cover categories.
    LandCover = case_when(
      str_detect(LandCoverFull, "Developed") ~ LandCoverFull,
      str_detect(LandCoverFull, "Forest") ~ "Forest",
      LandCoverFull == "Water" ~ "Water",
      .default = "Other"),
    LandCover = ordered(LandCover, levels = c("Water",
                                              "Forest",
                                              "Other",
                                              "Developed, Open space",
                                              "Developed, Low intensity",
                                              "Developed, Medium intensity",
                                              "Developed, High intensity"
    ))
  ) 

# Sum up area in each land cover
# using the Forest and Other groupings
cover <- cover.full %>% 
  group_by(Year, LandCover) %>% 
  summarize(Area = sum(Freq, na.rm=T)) %>% 
  ungroup() %>% 
  filter(LandCover != "Water")

totalarea<-cover %>% 
  group_by(Year) %>% 
  summarize(TotalArea = sum(Area))

# Including water, the total area is the same each year.
# But without water, the "land area" changes slightly among years. Hmm....
# Let's divide everything by a constant denominator. Use 2021 to get accurate recent land cover.
t<-totalarea %>% 
  filter(Year == 2021) %>% 
  pull(TotalArea)

# Also sum up the 4 developed categories to total developed.
totaldeveloped<-cover %>% 
  filter(str_detect(LandCover,"Developed")) %>% 
  group_by(Year) %>% 
  summarize(Area = sum(Area, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(LandCover = "Total Developed")

# Table with land cover, total developed, and impervious
# all expressed as percents of 2021 land area.
covertable<-cover %>% 
  bind_rows(totaldeveloped) %>% 
  bind_rows(imp) %>% 
  mutate(Percent = round(Area / t *100, digits = 1)) %>% 
  select(LandCover,Year,Percent) %>% 
  pivot_wider(names_from = Year,
              values_from = Percent)

write_csv(covertable, "Lake Washington - Land cover percents by year.csv")

# Also sum up area in each individual land-cover category
# for appendix.
# Similar to the table above, but with more categories.
covertable.full <- cover.full %>%
  group_by(Year, LandCoverFull) %>% 
  summarize(Area = sum(Freq, na.rm=T)) %>% 
  ungroup() %>% 
  rename(LandCover = LandCoverFull) %>% 
  filter(LandCover != "Water") %>% 
  bind_rows(totaldeveloped) %>% 
  bind_rows(imp) %>% 
  mutate(Percent = round(Area / t *100, digits = 1)) %>% 
  select(LandCover,Year,Percent) %>% 
  pivot_wider(names_from = Year,
              values_from = Percent)

write_csv(covertable.full, "Lake Washington - Land cover percents by year - all categories.csv")
