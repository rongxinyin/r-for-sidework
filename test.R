library(ggplot2)
library(sf)
library(leaflet)

# Simple ggplot test
ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) + geom_point()

# Simple sf test
nc <- st_read(system.file("shape/nc.shp", package = "sf"))
print(nc)

# Simple leaflet test
leaflet() %>%
  addTiles() %>%
  addMarkers(lng = 174.768, lat = -36.852, popup = "The birthplace of R")