## Install Packages
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

## Load Packages

library(tidyverse)
library(RColorBrewer)
library(classInt)
library(sp)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
library(here)
library(janitor)

## Load shapefile

WACounties <- st_read(here::here("wk2_data",
                                  "Washington_Counties_with_Natural_Shoreline___washsh_area.geojson"))

view(WACounties)
qtm(WACounties)

## Load csv, exclude NULL

reportcard <- read_csv(here("wk2_data","Report_Card_Assessment_Data_2018-19_School_Year_20231013.csv"),
                       locale = locale(encoding = "latin1"),
                                       na = c("NULL")
                       )%>%
  clean_names()

head(reportcard)

# summarise all categorical values in variables
sum_cols <- table(reportcard$county)
sum_cols


# filter to relevant columns + filter just science
reportcard1 <- reportcard %>%
  filter(.,test_subject=="Science") %>%
  filter(.,grade_level=="All Grades") %>%
  filter(.,county!="Multiple") %>%
  filter(.,organization_level=="School") %>%
  select(county,grade_level,organization_level,test_subject,
         count_of_students_expected_to_test,
         count_met_standard)

head(reportcard1)

# calculate pass rate

avg = sum(reportcard1$count_met_standard,na.rm=TRUE) / 
  sum(reportcard1$count_of_students_expected_to_test,na.rm=TRUE)
print(avg)

# summarise by county

reportcard2 <- reportcard1 %>%
  group_by(county) %>%
  summarise(sum_met_standard = sum(count_met_standard,na.rm=TRUE),
            sum_all_students = sum(count_of_students_expected_to_test,na.rm=TRUE)) %>%
  mutate(pass_rate = sum_met_standard/sum_all_students) %>%
  mutate(countydiff = (pass_rate - avg)*100)

## Join the 2

WAcountypassratemap <- WACounties %>%
  clean_names(.) %>%
  left_join(.,
            reportcard2,
            by=c("countylabel"="county"))
            
head(WAcountypassratemap)

# Plot

# quick plot
# qtm(WAcountypassratemap,fill="countydiff",midpoint = NA)

# define a larger bbox
bbox_new <- st_bbox(WAcountypassratemap) # current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
bbox_new[2] <- bbox_new[2] - (0.75 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.25 * yrange) # ymax - top

# OSM base map
tmapwa <- bbox_new %>%
  st_bbox(.) %>%
  tmaptools::read_osm(., type = "osm", zoom = 6.5)

# PLOT!!!
tmap_mode("plot")
tm_shape(tmapwa)+
  tm_rgb()+
  tm_shape(WAcountypassratemap) + 
  tm_polygons("countydiff", 
              style="pretty",
              palette="Greens",n=6,
              midpoint=0,
              title=str_glue("Percentage point difference between proportion of students of all grades \nmeeting required standards in Science in each county and WA state average {round(avg*100,1)}%"),
              alpha = 0.8) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Performance of science students in Washington State, by County",
            legend.bg.color = "White",
            legend.bg.alpha = 0.8,
            legend.position = c("right", "bottom"),
            legend.text.size = 0.5,
            legend.frame = TRUE,
            legend.title.size = 0.75)
