library(sf)
library(tmap) 
library(tmaptools)
library(RSQLite)
library(tidyverse)

# read in the shapefile
shape <- st_read("C:/Users/shaun/Documents/CASA/gis/data/statsnz-territorial-authority-2018-generalised-SHP/territorial-authority-2018-generalised.shp")

# read in the csv
mycsv <- read_csv("C:/Users/shaun/Documents/CASA/gis/data/paid_employee_2018_NZ.csv")

shape%>%
  head(.,n=10)

mycsv

# merge csv and shapefile
shape <- shape %>%
  merge(.,
        mycsv,
        by.x="TA2018_V1_", 
        by.y="code")

# set tmap to plot
tmap_mode("plot")

# have a look at the map
qtm(shape, fill = "share")

# write to a .gpkg
shape %>%
  st_write(.,"C:/Users/shaun/Documents/CASA/gis/wk1hw.gpkg",
           "paid_employee_2018",
           delete_layer=TRUE)

# connect to the .gpkg
con <- dbConnect(RSQLite::SQLite(),dbname="C:/Users/shaun/Documents/CASA/gis/wk1hw.gpkg")

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
