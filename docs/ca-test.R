

library(tidyverse)
library(sf)
library(USAboundaries)
library(cowplot)
library(sp)
library(leaflet)


source("docs/utils.R")

### USGS SPATIAL + TIMESERIES DATAFRAMES 

# Arizona state shape (CRS = 5070)
ca = ca_shp %>%  
  st_transform(5070)

# add 'well #' for each unique well
usgs = ca_nwis_unique_sites
usgs$wellid = paste("well", 1:nrow(usgs))

# remove wells located at same lat/long but have different well ID
usgs_spatial = usgs %>% 
  st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
  st_transform(5070) %>%
  as_Spatial() %>% 
  remove.duplicates() %>% 
  st_as_sf() %>% 
  group_by(wellid)

# match 'well #' from spatial dataframe to corresponding well in timeseries
usgs_time = ca_nwis_all %>% 
  group_by(site_id) 
usgs_time = left_join(usgs_time, select(usgs, site_id, wellid), by = "site_id") 
usgs_time = usgs_time %>% filter(wellid %in% usgs_spatial$wellid)

# FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
thresh = usgs_time %>% 
  group_by(wellid) %>% 
  filter(measurement_dist >= 10) %>% 
  mutate(measure_period = year - lag(year)) %>% 
  filter(any(measure_period > 5))
usgs_time = usgs_time %>% filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>% 
  mutate(measure_period = year - lag(year))
usgs_spatial = usgs_spatial %>% 
  filter(!wellid %in% thresh$wellid) %>% 
  filter(measurement_dist >= 10) 

### STATE SPATIAL + TIMESERIES DATAFRAMES

# add 'well #' for each unique well
state = adwr_unique_sites %>% 
  filter(date > 1960-01-01, date < as.Date.character('2000-01-01'))
state$wellid = paste("well", 1:nrow(state))


# remove wells located at same lat/long but have different well ID
state_spatial = state %>% 
  st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
  st_transform(5070) %>% 
  as_Spatial() %>% 
  remove.duplicates() %>% 
  st_as_sf() %>% 
  group_by(site_id)

# match 'well #' from spatial dataframe to corresponding well in timeseries
state_time = adwr_all %>% 
  group_by(site_id) 
state_time = left_join(state_time, select(state, site_id, wellid), by = "site_id") 
state_time = state_time %>% filter(wellid %in% state_spatial$wellid)

thresh_state = state_time %>% 
  group_by(wellid) %>% 
  filter(measurement_dist >= 10) %>% 
  mutate(measure_period = year - lag(year)) %>% 
  filter(any(measure_period > 5))
state_time = state_time %>% filter(!wellid %in% thresh_state$wellid, measurement_dist >= 10) %>% 
  mutate(measure_period = year - lag(year)) 
state_spatial = state_spatial %>% 
  filter(!wellid %in% thresh_state$wellid) %>% 
  filter(measurement_dist >= 10)



### AQUIFERS
aquifer = read_sf('data/shps/us_aquifers.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")

aquifer2 = aquifer %>% st_transform(4326)

box = ca %>% st_transform(4326) %>% 
  st_bbox() %>% 
  st_as_sfc()
temp = st_intersects(aquifer2, box)

aquifer2 = aquifer2[which(lengths(temp) != 0), ]




### LEAFLET MAPS
# USGS leaflet
x = usgs_spatial %>% st_transform(4326)

# RColorBrewer::display.brewer.all(n=10, exact.n=FALSE)
col1 = RColorBrewer::brewer.pal(9,"Blues")
col2 = RColorBrewer::brewer.pal(9,"YlOrRd")
col3 = RColorBrewer::brewer.pal(9,"YlGnBu")
col4 = RColorBrewer::brewer.pal(9,"Spectral")

pals1 = colorNumeric(col1, domain = x$dtw)
pals2 = colorNumeric(col2, domain = x$dtw)
pals3 = colorBin(col3, domain = 1:8)
pals4 = colorFactor(col4, domain = aquifer2$AQ_NAME)

a = dtw_range(x, 0, 100) %>% select(wellid, date, dtw, measurement_dist)
b = dtw_range(x, 100, 200) %>% select(wellid, date, dtw, measurement_dist)
c = dtw_range(x, 200, 300) %>% select(wellid, date, dtw, measurement_dist)
d = dtw_range(x, 300, 400) %>% select(wellid, date, dtw, measurement_dist)
e = dtw_range(x, 400, 500) %>% select(wellid, date, dtw, measurement_dist)
f = dtw_range(x, 500, 600) %>% select(wellid, date, dtw, measurement_dist)
g = dtw_range(x, 600, 700) %>% select(wellid, date, dtw, measurement_dist)
h = dtw_range(x, 700, 800) %>% select(wellid, date, dtw, measurement_dist)
i = dtw_range(x, 800, 900) %>% select(wellid, date, dtw, measurement_dist)
j = dtw_range(x, 900, 1500) %>% select(wellid, date, dtw, measurement_dist)

# # State leaflet 
# z = state_spatial %>% st_transform(4326)
# 
# aa = dtw_range(z, 0, 100) %>% select(wellid, date, dtw, measurement_dist)
# bb = dtw_range(z, 100, 200) %>% select(wellid, date, dtw, measurement_dist)
# cc = dtw_range(z, 200, 300) %>% select(wellid, date, dtw, measurement_dist)
# dd = dtw_range(z, 300, 400) %>% select(wellid, date, dtw, measurement_dist)
# ee = dtw_range(z, 400, 500) %>% select(wellid, date, dtw, measurement_dist)
# ff = dtw_range(z, 500, 600) %>% select(wellid, date, dtw, measurement_dist)
# gg = dtw_range(z, 600, 700) %>% select(wellid, date, dtw, measurement_dist)
# hh = dtw_range(z, 700, 800) %>% select(wellid, date, dtw, measurement_dist)
# ii = dtw_range(z, 800, 900) %>% select(wellid, date, dtw, measurement_dist)
# jj = dtw_range(z, 900, 1500) %>% select(wellid, date, dtw, measurement_dist)

leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron, group = 'Tiles') %>% 
  addPolygons(data = aquifer2, fillColor = ~pals4(AQ_NAME), fillOpacity = 0.3,
              color = 'black',
              label = ~AQ_NAME, group = 'Aquifer') %>%
  addCircleMarkers(data = a, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(a, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '0 - 100 ft') %>% 
  addCircleMarkers(data = b, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(b, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '100 - 200 ft') %>% 
  addCircleMarkers(data = c, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(c, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '200 - 300 ft') %>% 
  addCircleMarkers(data = d, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(d, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '300 - 400 ft') %>% 
  addCircleMarkers(data = e, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(e, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '400 - 500 ft') %>% 
  addCircleMarkers(data = f, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(f, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '500 - 600 ft') %>% 
  addCircleMarkers(data = g, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(g, feature.id = FALSE, 
                                               row.numbers = FALSE), group = '600 - 700 ft') %>% 
  addCircleMarkers(data = h, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(h, feature.id = FALSE,
                                               row.numbers = FALSE), group = '700 - 800 ft') %>%
  # addCircleMarkers(data = i, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = ~pals2(dtw), fillOpacity = .8,
  #                  stroke = FALSE,
  #                  popup = leafpop::popupTable(i, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '800 - 900 ft') %>%
  # addCircleMarkers(data = j, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = ~pals2(dtw), fillOpacity = .8,
  #                  stroke = FALSE,
  #                  popup = leafpop::popupTable(j, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '900 > ft') %>%
  # addCircleMarkers(data = aa, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1, fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(aa, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '0 - 100 ft') %>%
  # addCircleMarkers(data = bb, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1,
  #                  fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(bb, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '100 - 200 ft') %>%
  # addCircleMarkers(data = cc, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1,
  #                  fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(cc, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '200 - 300 ft') %>%
  # addCircleMarkers(data = dd, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1, fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(dd, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '300 - 400 ft') %>%
  # addCircleMarkers(data = ee, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1, fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(ee, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '400 - 500 ft') %>%
  # addCircleMarkers(data = ff, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1, fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(ff, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '500 - 600 ft') %>%
  # addCircleMarkers(data = gg, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1, fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(gg, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '600 - 700 ft') %>%
  # addCircleMarkers(data = ii, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = 'black', fillColor = ~pals2(dtw), opacity = 1, fillOpacity = .8,
  #                  stroke = TRUE,
  #                  popup = leafpop::popupTable(ii, feature.id = FALSE,
  #                                              row.numbers = FALSE), group = '800 - 900 ft') %>%
  addLayersControl(overlayGroups = c('0 - 100 ft', '100 - 200 ft', '200 - 300 ft',
                                     '300 - 400 ft', '400 - 500 ft',
                                     '500 - 600 ft', '600 - 700 ft',
                                     '700 - 800 ft', 'Aquifer'),
                   baseGroups = c("Tiles"))


# TIME SERIES PLOTS

# SAN BERNINDINO
plot_well(usgs_time, 2215)
plot_well(usgs_time, 2204)


# VICTORVILLE

plot_well(usgs_time, 2643)
plot_well(usgs_time, 2204)













