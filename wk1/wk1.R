library(sf)
shape <- st_read("C:/Users/shaun/Documents/CASA/gis/data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
summary(shape)

# Plot all shapes
plot(shape)

# Plot just the outline
shape %>%
  st_geometry() %>%
  plot()

# Load CSV file
library(tidyverse)
mycsv <- read_csv("C:/Users/shaun/Documents/CASA/gis/data/flytipping.csv")
mycsv

# Merge
shape <- shape %>%
  merge(.,
        mycsv,
        by.x="GSS_CODE",
        by.y="code")

library(tmap)
tmap_mode("plot")
shape %>%
  qtm(.,fill = "2021-22")

shape%>%
  head(.,n=10)

shape %>%
  st_write(.,"C:/Users/shaun/Documents/CASA/gis/wk1inr.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)

library(readr)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(),dbname="C:/Users/shaun/Documents/CASA/gis/wk1inr.gpkg")

con %>%
  dbListTables()

con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()

