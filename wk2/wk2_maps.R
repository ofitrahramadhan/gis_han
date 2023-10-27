install.packages(c("classInt", 
                   "tmap",
                   "maptools",
                   "RColorBrewer", 
                   "sp", 
                   "rgeos", 
                   "tmaptools", 
                   "sf", 
                   "downloader", 
                   "rgdal", 
                   "geojsonio"))

#Load Packages (ignore any error messages about being built under a 
#different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(tidyverse)
library(sf)
library(janitor)

#read geojson
EW <- st_read(here::here("wk2_data",
                         "Local_Authority_Districts_May_2021_UK_BFC_2022_-815591597668846243.geojson"))
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad21cd, "^E09"))%>%
  left_join(.,
            LondonData, 
            by=c("lad21cd"="new_code"))%>%
  distinct(.,lad21cd,
           .keep_all = TRUE) #If you change to .keep_all=FALSE  then all the other will be removed.

# OSM base map with a bounding box

install.packages("OpenStreetMap")
library(OpenStreetMap)

tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "osm", zoom = NULL)

# plot with basemap

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

# merge lifeexpectancy4 with spatial

lifeexpectancy4map <- EW %>%
  clean_names(.) %>%
  left_join(., 
             life_expectancy4,
             by = c("lad21cd" = "new_code"))%>%
  distinct(.,lad21cd, 
           .keep_all = TRUE)

# map lifeexpectancy4map

tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(lifeexpectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))