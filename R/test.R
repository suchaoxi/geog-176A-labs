library(tidyverse)
library(sf)
library(leaflet)

states = USAboundaries::us_states() %>%
  st_transform(5070)

state.of.interest = "Pennsylvania"

soi = filter(states, state_name == state.of.interest)

plot(soi$geometry)

adjoining = st_filter(states, soi, .predicate = st_touches)

closest = st_make_grid(soi, n = 70, square = TRUE) %>%
  st_centroid()  %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)

unique(closest$state_name)

leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircles(data = st_transform(closest, 4326), radius = 1,  color = ~colorFactor("YlOrRd", state_name)(state_name), color = "black", weight = .5)
