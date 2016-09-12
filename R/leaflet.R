library(leaflet)
library(dplyr)
library(tidyr)
library(foreign)
library(ggplot2)

accidents <- read.dbf("./data/FARS2015NationalDBF/accident.dbf")
carInfo <- read.dbf("./data/FARS2015NationalDBF/VINDecode.dbf")
vehicle <- read.dbf("./data/FARS2015NationalDBF/vehicle.dbf")

allData <- merge(accidents, carInfo) %>%
  filter(LONGITUD < 777) %>%
  merge(vehicle)

carTypes <- allData %>%
  group_by(make = VINMAKE_T, model = VINMODEL_T) %>%
  summarize(value = n()) %>%
  na.omit() %>%
  arrange(value)

carMfcts <- carTypes %>%
  group_by(make) %>%
  summarize(value = sum(value))

carBodies <- allData %>%
  group_by(type = BODYSTYL_T) %>%
  summarize(value = n()) %>%
  na.omit() %>%
  arrange(value)

pal <- colorFactor(
  palette = "Accent",
  domain = allData$VEHTYPE_T
)

m <- leaflet(allData) %>%
  addTiles() %>%
  addCircleMarkers(radius = 12,
                   clusterOptions = markerClusterOptions(),
                   fillColor = ~pal(VEHTYPE_T),
                   fillOpacity = .7,
                   stroke = FALSE,
                   lng = allData$LONGITUD, 
                   lat = allData$LATITUDE,
                   popup = ~paste0("Date of Accident: ", allData$MONTH, "/", allData$DAY,"/", allData$YEAR, "</br>", 
                                   "Vehicle Make: ", allData$VINMAKE_T, "</br>", 
                                   "Vehicle Model: ", allData$VINMODEL_T, "</br>", 
                                   "Type of Vehicle: ", allData$VEHTYPE_T, "</br>", 
                                   "Drunk Driver Involved: ", ifelse(allData$DRUNK_DR == 1, "Yes", "No"), "</br>",
                                   "Number of Vehicles Involved: ", allData$VE_TOTAL, "</br>",
                                   "Total Fatalities: ", allData$FATALS, "</br>",
                                   "Fatalities in This Vehicle: ", allData$DEATHS)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~VEHTYPE_T,
            title = "Type of Vehicle"
)
