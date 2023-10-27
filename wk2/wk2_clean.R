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
library(here)
library(dplyr)
library(janitor)


### Data loading and cleaning locally
LondonData <- read_csv("wk2_data/ward-profiles-csv.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a") %>%
  clean_names()

Datatypelist <- LondonData %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to="All_variables",
               values_to="Variable_class")
Datatypelist

# Filter out London
LondonBoroughs <- LondonData %>%
  filter(str_detect(`new_code`,"^E09")) %>%
  rename(borough=`ward_name`)

view(LondonBoroughs)

# Dedupe
LondonBoroughs <- LondonBoroughs %>%
  distinct()

# One way to check
LondonBoroughs$`borough`

# Another way to check filter using DPLYR
LondonBoroughs %>%
  dplyr::select(`borough`) %>%
  print()

### Add life expectancy columns and compare to UK average

# select using condition

life_expectancy_sel <- LondonBoroughs %>%
  dplyr::select(contains("expectancy"),
                contains("obese"),
                contains("borough"))
life_expectancy_sel


# create life expectancy table from original table using mutate

life_expectancy <- LondonBoroughs %>%
  #new column average life expectancy and normalised expectancy
  mutate(averagelifeexpectancy = (female_life_expectancy_2009_13 + 
                                    male_life_expectancy_2009_13)/2) %>%
  mutate(normalisedlifeexpectancy = averagelifeexpectancy /
          mean(averagelifeexpectancy)) %>%
  #select columns we want to include
  select(new_code,
         borough,
         averagelifeexpectancy,
         normalisedlifeexpectancy) %>%
  #arrange
  arrange(desc(normalisedlifeexpectancy))
    
view(life_expectancy)


# create another column UKcompare using case_when(... ~..., TRUE ~...)

life_expectancy2 <- life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>1 ~ "above UK average",
                                    TRUE ~ "below UK average")) %>%
  mutate(UKcompare_norm = case_when(normalisedlifeexpectancy>1 ~ "above UK average",
                               TRUE ~ "below UK average"))

view(life_expectancy2)


# make UKcompare more human-readable with differences highlighted

life_expectancy3 <- life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%          #create new column
  mutate(across(where(is.numeric),round,3)) %>%           #round everything to 3SF
  mutate(across(UKdiff,round,0)) %>%                      #round UKdiff to 0SF
  mutate(UKcompare = case_when(averagelifeexpectancy>=81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff,
                                       "years",
                                       sep=" "),
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" "))) %>%
  group_by(UKcompare)%>%
  summarise(count=n())

view(life_expectancy3)

# prep data to plot

life_expectancy4 <- life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))
  
view(life_expectancy4)



# # simple plot
# plot(life_expectancy4$averagelifeexpectancy,
#      life_expectancy4$UKdiff)
# 
# 
# # fancier plot
# library(plotly)
# plot_ly(LondonBoroughs, 
#         #data for x axis
#         x = ~male_life_expectancy_2009_13, 
#         #data for y axis
#         y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14, 
#         #attribute to display when hovering 
#         text = ~borough, 
#         type = "scatter", 
#         mode = "markers")



#read geojson
EW <- st_read(here::here("wk2_data",
                         "Local_Authority_Districts_May_2021_UK_BFC_2022_-815591597668846243.geojson"))

# Join LondonData with spatial
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

# tmap_mode("plot")
# 
# tm_shape(tmaplondon)+
#   tm_rgb()+
#   tm_shape(BoroughDataMap) + 
#   tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
#               style="jenks",
#               palette="YlOrBr",
#               midpoint=NA,
#               title="Rate per 1,000 people",
#               alpha = 0.5) + 
#   tm_compass(position = c("left", "bottom"),type = "arrow") + 
#   tm_scale_bar(position = c("left", "bottom")) +
#   tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))

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








flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
# view the data
view(flytipping1)

# pivot_longer
flytipping_long <- flytipping1 %>%
  pivot_longer(
    cols = 4:10,
    names_to = "tipping_type",
    values_to = "count"
  )
view(flytipping_long)

# pivot_wider
flytipping_wide <- flytipping_long %>%
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )
view(flytipping_wide)

flytipping2 <- flytipping1[,1:4]
view(flytipping2)
widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)
view(widefly)