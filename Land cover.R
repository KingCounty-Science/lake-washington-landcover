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


readNLCD<-function(year,w) {
  
  filename<-ifelse(year == 2021, 
                   sprintf("NLCD/NLCD_%s_Land_Cover_L48_20230630_nDr0hjPobjWPXcj5l81y.tiff",year),
                   sprintf("NLCD/NLCD_%s_Land_Cover_L48_20210604_nDr0hjPobjWPXcj5l81y.tiff",year)
  )
  
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

tic()
nlcd<-map_dfr(c(2001, 2004, 2006, 2008, 2011, 2013, 2016, 2019, 2021), readNLCD, w=watershed)
toc()

# Group land cover into water, shades of development, all forests, and other.
cover<-nlcd %>% 
  mutate(LandCover = case_when(
               Layer_1 == 11 ~ "Water",
               Layer_1 == 21 ~ "Developed, Open space",
               Layer_1 == 22 ~ "Developed, Low intensity",
               Layer_1 == 23 ~ "Developed, Medium intensity",
               Layer_1 == 24 ~ "Developed, High intensity",
               Layer_1 %in% c(41, 42, 43) ~ "Forest", 
               .default = "Other"),
         LandCover = ordered(LandCover, levels = c("Water",
                                                   "Forest",
                                                   "Other",
                                                   "Developed, Open space",
                                                   "Developed, Low intensity",
                                                   "Developed, Medium intensity",
                                                   "Developed, High intensity"
                                                   ))
         ) %>% 
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
  mutate(Percent = round(Area / t *100, digits = 1)) %>% 
  select(LandCover,Year,Percent) %>% 
  pivot_wider(names_from = Year,
              values_from = Percent)

write_csv(covertable, "Lake Washington - Land cover percents by year.csv")

# Compare to dividing by each year's land area?
covertable2<-cover %>% 
  bind_rows(totaldeveloped) %>% 
  left_join(totalarea, by = "Year") %>% 
  mutate(Percent = round(Area / TotalArea *100, digits = 1)) %>% 
  select(LandCover,Year,Percent) %>% 
  pivot_wider(names_from = Year,
              values_from = Percent)
# Not much difference -- some land covers still go up/down or down/up.  
               
               

               
               
