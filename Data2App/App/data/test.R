library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)

# GeoJSON Data
states <-  read_sf("https://raw.githubusercontent.com/rowanhogan/australian-states/master/states.geojson")
# sp <- as(states, "Spatial")
tab1 <- read.csv("data/tab_1_map.csv")
colnames(tab1)[1] <- "Covid"
# tab1$Time <- format(tab1$Time, format = "%H:%M:%S")
# tab1$Time <- as.POSIXct(tab1$Time,format="%H:%M:%S")

colnames(tab1)[1] <- "Covid"

lat_lon <- read.csv("data/lat_lon.csv")

# tab1 <- tab1 %>%
#   dplyr::inner_join(lat_lon, by=c("State" = "State"))



# Join to count data
data <- states %>%
  dplyr::left_join(tab1, by=c("STATE_NAME" = "State"))

# Specify choropleth colors
pal <- colorQuantile("Blues", domain = data$Value)

# Plot Map
leaflet(data) %>%
  addTiles() %>%
  addPolygons(fillColor=~pal(Value), fillOpacity=0.8, color="white", weight=1)

sf <- st_read("data/india_shapefiles/IND_adm1.shp")
sp <- as(sf, "Spatial")


tab3 <- read.csv("data/tab_3.csv")
tab3 <- tab3[tab3$State == "Victoria", ]

tab3$SETTLEMENTDATE <- strptime(x = as.character(tab3$SETTLEMENTDATE),
                                format = "%d-%m-%Y %H:%M")
# tab3$SETTLEMENTDATE <- as.POSIXct(tab3$SETTLEMENTDATE,'%Y-%m-%d %H:%M')

tab3 <- tab3[!(is.na(tab3$SETTLEMENTDATE)), ]
colnames(tab3)[colSums(is.na(tab3)) > 0]




tab2 <- read.csv("data/tab_2.csv")
tab2 <- tab2[tab2$State == "Victoria", ]
tab2$Date <-as.Date(tab2$Date,'%d-%m-%Y')

tab2$yearmonth <- as.Date(paste0("2018-", tab2$Month, "-1"))





tab1 <- tab1[tab1$Covid == "Yes", ]
tab1 <- tab1[tab1$Electricity == "RegionalReferencePrice", ]
tab1 <- tab1[tab1$State == "Victoria", ]

tab1 <- aggregate(tab1$Value, list(tab1$Time, tab1$Season), mean)
colnames(tab1)[1] <- "Time"
colnames(tab1)[2] <- "Season"
colnames(tab1)[3] <- "Value"

tab1$Time <- as.POSIXct(strptime(tab1$Time, format="%H:%M:%S"))

p <- tab1 %>%
  ggplot( aes(x=Time, y=Value, group=Season, color=Season)) +
  geom_line() +
  ggtitle("Popularity of American names in the previous 30 years") +
  ylab("Number of babies born") +
  scale_x_datetime(date_breaks = "2 hour",
                   date_labels = "%H:%M")

ggplotly(p, tooltip = "text")



tab4_map <- read.csv("data/tab_4.csv")



tab1_map <- read.csv("data/tab_1.csv")
lat_lon <- read.csv("data/lat_lon.csv")
tab1_map <- tab1_map %>%
  dplyr::inner_join(lat_lon, by=c("State" = "state"))


tab1_map <- tab1_map[tab1_map$Covid == "No", ]
tab1_map <- tab1_map[tab1_map$Electricity == "ElectricityDemand", ]

data <- states %>%
  dplyr::inner_join(tab1_map, by=c("STATE_NAME" = "State"))
