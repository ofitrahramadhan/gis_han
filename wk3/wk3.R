library(sf) 
library(here)

## sf Read layers in a geopackage

st_layers(here("wk3_data","gadm36_AUS_gpkg","gadm36_AUS.gpkg"))

## Extract the Aus outline layer to one object 
Ausoutline <- st_read(here("wk3_data","gadm36_AUS_gpkg","gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')

print(Ausoutline)

## What is the projection

st_crs(Ausoutline)$proj4string 
# proj4string is to check projection stored in the shapefile

## Set projection system in case there is none. Code 4326 is the ESPG code for WGS86
Ausoutline <- Ausoutline %>% 
  st_set_crs(., 4326)

## Transform to GDA94, code 3112
AusoutlinePROJECTED <- Ausoutline %>% 
  st_transform(.,3112)

print(AusoutlinePROJECTED)

## From sf to sp
AusoutlineSP <- Ausoutline %>% 
  as(., "Spatial")

## From sp to sf
AusoutlineSF <- AusoutlineSP %>% 
  st_as_sf()

library(raster)
library(terra)
jan <- terra::rast(here("wk3_data", "wc2.1_5m_tavg","wc2.1_5m_tavg_01.tif"))

# have a look at the raster layer jan
jan

# Plot it
raster::plot(jan)


# pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


# set the proj 4 to a new object
newproj<-"ESRI:54009"
# get the jan raster and give it the new proj4
pr1 <- jan %>%
  terra::project(., newproj)
plot(pr1)

# back to WGS86
pr1 <- pr1 %>%
  terra::project(., "EPSG:4326")
plot(pr1)

# look in our folder, find the files that end with .tif and
library(fs)
dir_info(here("wk3_data", "wc2.1_5m_tavg"))

# Use dplyr to get the files you want
library(tidyverse)
listfiles <- dir_info(here("wk3_data", "wc2.1_5m_tavg")) %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull() #similar to $ used to select columns

# have a look at the file names 
listfiles

# load all files into one single raster stack
worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp


# access the january layer
worldclimtemp[[1]]

# rename each layer into month

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month

# Using a raster stack we can extract data with a single command!! For example let’s make a dataframe of some sample sites — Australian cities/towns

site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this information into one list 
samples <- data.frame(site, lon, lat, row.names="site")

View(samples)

# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)

Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")

view(Aucitytemp2)

# Raster spatial descriptive statistics
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

Perthtemp

# hist(as.numeric(Perthtemp)) # simple, below is more stylized

library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
t<-Perthtemp %>%
  dplyr::select(Jan:Dec)

hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)

histinfo


# Use a shape instead of point to carve out data from raster
plot(Ausoutline$geom)

# simplify
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(.,dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

  #preserveTopology=TRUE

# check if the crs is the same
print(Ausoutline)
crs(Ausoutline)
print(worldclimtemp)
crs(worldclimtemp)

# Join the two, plotted
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)


# If want to just get raster data within the outline of the shape use mask() which uses a cookie cutter of the raster with the vector layer

# mask from the big map
exactAus1 <- terra::mask(worldclimtemp, Ausoutline)
plot(exactAus1)

# crop from the big map
exactAus2 <- terra::crop(worldclimtemp, Ausoutline)
plot(exactAus2)

# CROP THEN MASK
Austemp_cropped <- Ausoutline %>%
  terra::crop(worldclimtemp,.)
  
exactAus <- Austemp_cropped %>%
  terra::mask(.,Ausoutline)

plot(exactAus) # this would be the same as plot(Austemp) but faster


#subset using the known location of the raster
hist(exactAus$Mar, col="red", main ="March temperature")


# Histogram with ggplot
exactAusdf <- exactAus %>%
  as.data.frame()

library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))

# Pivot longer so that we have multiple months on the histogram and not just one
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )




# Subsetting two months
twomonths <- squishdata %>%
  filter(., Month=="Jan" | Month=="Jun")

# mean two months
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths

# PLot them nicely!
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))





# Plot all 12 months!
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))
view(data_complete_cases)

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))




# Using plotly
library(plotly)

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist <- plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist





# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)




# standard deviation per month
sdofall <- squishdata %>%
  group_by(Month) %>%
  summarize(sd = sd(Temp, na.rm=TRUE))

# maximum per month
maxofall <- squishdata %>%
  group_by(Month) %>%
  summarize(max = max(Temp, na.rm=TRUE))

# minimum per month
minofall <- squishdata %>%
  group_by(Month) %>%
  summarize(min = min(Temp, na.rm=TRUE))

# Interquartlie range per month
IQRofall <- squishdata %>%
  group_by(Month) %>%
  summarize(IQR = IQR(Temp, na.rm=TRUE))

# perhaps you want to store multiple outputs in one list..
lotsofstats <- squishdata %>%
  group_by(Month) %>%
  summarize(mean = mean(Temp, na.rm=TRUE),
            max=max(Temp, na.rm=T),
            min = min(Temp, na.rm=TRUE),
            sd = sd(Temp, na.rm=TRUE),
            IQR = IQR(Temp, na.rm=TRUE)) 

view(lotsofstats)

# or you want to know the mean (or some other stat) 
# for the whole year as opposed to each month...

meanwholeyear <- squishdata %>%
  summarize(mean = mean(Temp, na.rm=TRUE),
            max=max(Temp, na.rm=T),
            min = min(Temp, na.rm=TRUE),
            sd = sd(Temp, na.rm=TRUE),
            IQR = IQR(Temp, na.rm=TRUE)) 

view(meanwholeyear)
