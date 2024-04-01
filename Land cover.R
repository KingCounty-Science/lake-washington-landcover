# Lake Washington watershed - land cover over time
# Daniel Nidzgorski
# March 13, 2024

library(sf)
library(terra)
library(tictoc)
library(tidyverse)


rm(list=ls())

# Lake Washington watershed
watershed<-st_read("Watershed/LakeWashingtonWatershed.shp") %>% 
  st_transform(crs="ESRI:102008")


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
tic()
nlcd.imp<-map_dfr(list.files(path="NLCD", pattern="Impervious.+tiff$",full.names = T), readNLCD, w=watershed)
toc()

# Layer_1 is the percent of impervious surface in that pixel.
# Directly converting it to numeric ends up off by one (1-101; row numbers??), but going via
# character works.
imp<-nlcd.imp %>% 
  mutate(PctImp = as.character(Layer_1),
         PctImp = as.numeric(PctImp), 
         ImpArea = PctImp * Freq / 100) %>% 
  group_by(Year) %>% 
  summarize(Area = sum(ImpArea, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(LandCover = "Total Impervious")

## Land cover

tic()
nlcd.cover<-map_dfr(list.files(path="NLCD", pattern="Cover.+tiff$",full.names = T), readNLCD, w=watershed)
toc()

# Group land cover into water, shades of development, all forests, and other.
cover.full<-nlcd.cover %>% 
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

cover <- cover.full %>% 
  group_by(Year, LandCover) %>% 
  summarize(Area = sum(Freq, na.rm=T)) %>% 
  ungroup() %>% 
  filter(LandCover != "Water")


p<-ggplot(cover, aes( x = Year, y = Area, fill = LandCover))+
  geom_area()

p

# The changes are too subtle to see in the graph! So let's try a table, as percents

totalarea<-cover %>% 
  group_by(Year) %>% 
  summarize(TotalArea = sum(Area))

# Including water, the total area is the same each year.
# But without water, the "land area" changes slightly. Hmm....
# Let's divide everything by a constant denominator. Use 2021 to get accurate recent land cover.

t<-totalarea %>% 
  filter(Year == 2021) %>% 
  pull(TotalArea)

totaldeveloped<-cover %>% 
  filter(str_detect(LandCover,"Developed")) %>% 
  group_by(Year) %>% 
  summarize(Area = sum(Area, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(LandCover = "Total Developed")

covertable<-cover %>% 
  bind_rows(totaldeveloped) %>% 
  bind_rows(imp) %>% 
  mutate(Percent = round(Area / t *100, digits = 1)) %>% 
  select(LandCover,Year,Percent) %>% 
  pivot_wider(names_from = Year,
              values_from = Percent)

write_csv(covertable, "Lake Washington - Land cover percents by year.csv")

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

# Compare to dividing by each year's land area?
covertable2<-cover %>% 
  bind_rows(totaldeveloped) %>% 
  bind_rows(imp) %>% 
  left_join(totalarea, by = "Year") %>% 
  mutate(Percent = round(Area / TotalArea *100, digits = 1)) %>% 
  select(LandCover,Year,Percent) %>% 
  pivot_wider(names_from = Year,
              values_from = Percent)
# Not much difference -- some land covers still go up/down or down/up.  







