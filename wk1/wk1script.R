library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)

# read in the shapefile
shape <- st_read("C:/Users/shaun/Documents/CASA/gis/data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

# read in the csv
mycsv <- read_csv("C:/Users/shaun/Documents/CASA/gis/data/flytipping.csv")

# merge csv and shapefile
shape <- shape %>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="code")

# set tmap to plot
tmap_mode("plot")

# have a look at the map
qtm(shape, fill = "2021-22")

# write to a .gpkg
shape %>%
  st_write(.,"C:/Users/shaun/Documents/CASA/gis/wk1inr.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)

# connect to the .gpkg
con <- dbConnect(RSQLite::SQLite(),dbname="C:/Users/shaun/Documents/CASA/gis/wk1inr.gpkg")

# list what is in it
con %>%
  dbListTables()

# add the original .csv
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

# disconnect from it
con %>% 
  dbDisconnect()
