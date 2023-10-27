B <- 2
C <- A+B
C

ls()
rm(A)
ls()

Data1 <- c(1:100)
Data2 <- c(101:200)
# Plot the data
plot(Data1, Data2, col="red")

Data3 <- rnorm(100, mean=53, sd=34)
Data4 <- rnorm(100, mean=64, sd=14)
# Plot the data
plot(Data3, Data4, col="blue")

df <- data.frame(Data1, Data2)
plot(df, col="green")

library(tidyverse)

df %>%
  head()

df %>%
  tail()

df[1:10,1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)

df

df$column1

df$column2

df[["column1"]]
setwd("../")
LondonDataOSK <- read.csv("wk2_data/ward-profiles-csv.csv",
                          header = TRUE,
                          sep = ",",
                          encoding = "latin1")

install.packages("here")
library(here)

here::here()

LondonDataOSK <- read.csv(here::here("wk2_data","ward-profiles-csv.csv"),
                          header = TRUE,
                          sep = ",",
                          encoding = "latin1")

here::i_am("ward-profiles-csv.csv")

here::i_am("gis")

here::i_am("gis/wk2/wk2.r")

here::here()

here()

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")

class(LondonData)
class(LondonDataOSK)

Datatypelist <- LondonData %>%
  summarise_all(class) %>%
  pivot_longer(everything(),
               names_to="All_variables",
               values_to="Variable_class")

Datatypelist




summary(df)

LondonData %>%
  colnames()%>%
  head()

LondonBoroughs <- LondonData[626:658,]

LondonBoroughs <- LondonData %>%
  slice(626:658)

Femalelifeexp <- LondonData %>%
  filter("Female life expectancy -2009-13">90)

LondonBoroughs <- LondonData %>%
  filter(str_detect(LondonData$`New code`, pattern = "^E09"))

LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))

LondonBoroughs$`Ward name`

LondonBoroughs %>%
  dplyr::select(`Ward name`) %>%
  print()

LondonBoroughs <- LondonBoroughs %>%
  distinct()

LondonBoroughs$`Ward name`

LondonBoroughs_manualcols <- LondonBoroughs[,c(1,19,20,21)]

LondonBoroughs_manualcols

LondonBoroughs_contains <- LondonBoroughs %>%
  dplyr::select(contains("expectancy"),
                contains("obsese - 2011/12 to 2013/14"),
                contains("Ward name"))

LondonBoroughs_contains

install.packages("janitor")
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()

life_expectancy <- LondonBoroughs %>%
  # create new colum calculated from existing column
  mutate(averagelifeexpectancy = (female_life_expectancy_2009_13 + 
                                    male_life_expectancy_2009_13)/2) %>%
  # create new colum calculated from existing column
  mutate(normalisedlifeexpectancy = averagelifeexpectancy / 
           mean(averagelifeexpectancy)) %>%
  # pull out columns we want
  dplyr::select(new_code,
                 borough,
                 averagelifeexpectancy,
                 normalisedlifeexpectancy) %>%
  arrange(desc(normalisedlifeexpectancy))

life_expectancy2 <- life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))

life_expectancy2

life_expectancy2_group <- life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff),count=n(), Average=mean(UKdiff))

life_expectancy2_group

life_expectancy3 <- life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy - 81.16)%>%
  mutate(across(where(is.numeric),round,3))%>%
  mutate(across(UKdiff, round,0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >=81 ~
                                 str_c("equal or above UK average by",
                                       UKdiff,
                                       "years",
                                       sep=" "),
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by (UKcompare) %>%
  summarise(count=n())

  
life_expectancy3

life_expectancy4 <- life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  mutate(across(where(is.numeric),round,3)) %>%
  mutate(across(UKdiff, round,0))

plot(LondonBoroughs$male_life_expectancy_2009_13,
     LondonBoroughs$percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14)

install.packages("plotly")

library(plotly)

plot_ly(LondonBoroughs,
        x = ~male_life_expectancy_2009_13,
        y = ~percent_children_in_reception_year_who_are_obese_2011_12_to_2013_14,
        text = ~borough,
        type = "scatter",
        mode = "markers")

install.packages("maptools")

install.packages(c("classInt", "tmap"))

install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
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

# geojson in local folder
EW <- st_read(here::here("gis","wk2_data",
                         "Local_Authority_Districts_May_2021_UK_BFC_2022_-815591597668846243.geojson"))

EW %>%
  head()
London 

LondonData <- clean_names(LondonData)
LondonData

  clean_names()%>%
  filter(str_detect(LAD21CD, "E09"))%>%
  merge(.,
        LondonData,
        by.x="LAD21CD",
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,LAD21CD,
           .keep_all = TRUE)

LondonData

EW %>%
  head()