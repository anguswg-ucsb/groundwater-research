

library(tidyverse)
library(sf)
library(USAboundaries)
library(ggthemes)
library(cowplot)
library(sp)
library(leaflet)


source("docs/utils.R")

### USGS SPATIAL + TIMESERIES DATAFRAMES 

# Arizona state shape (CRS = 5070)
az = us_states() %>% 
  filter(name == 'Arizona') %>% 
  st_transform(5070)

# add 'well #' for each unique well
usgs = az_nwis_unique_sites
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
usgs_time = az_nwis_all %>% 
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
  filter(date > 1960-01-01, date < as.Date.character('1990-01-01'))
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


### TRENDS
getJenksBreaks(trends$sd, 4)

plot_trends(usgs_time, 30, 15, az)


### DTW RANGES (0 -100, 100 - 200, 200 - 300)
pal = RColorBrewer::brewer.pal(9,"YlOrRd")


a = map_dtw(az, usgs_spatial, 500, 1000)
b = map_dtw(az, usgs_spatial, 1000, 1800)


h = plot_dtw(usgs_time, 500, 1000)
i = plot_dtw(usgs_time, 1000, 1500)



### BUFFER FUNCTION

buffer_fun1(usgs_spatial, state_spatial, b[5,], 100000, az)

buffer_fun(usgs_spatial, b[1,], 50000, az)

x = plot_dtw(usgs_time, 4, 20, 65)



# MEASUREMENT THRESHOLD FUNCTION


# ACTIVE MANAGEMENT AREAS

# read in AMA shapefiles
ama = read_sf('data/Act_Man_Areas.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")

ama2 = ama %>% st_transform(4326)


map_ama = function(df1, df2, ama, state) {
  i = st_intersection(df1, df2[ama,])
  
  plot1 = ggplot() + 
    geom_sf(data = state) +
    geom_sf(data = df2, fill = NA) + 
    geom_sf(data = i, aes(col = dtw), size = .5) + 
    scale_colour_gradient(low = 'lightblue', high = 'blue') +
    labs(caption = paste(nrow(i), 'wells')) +
    theme_void() +
    theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5))
  plot2 = ggplot(df2[1:8, ]) +
    geom_sf(data = state) +
    geom_sf(data = df2, aes(fill = OBJECTID)) +
    geom_sf_label(aes(label = OBJECTID))
  plot3 = plot_grid(plot2, plot1, nrow = 1)
  print(plot3)
  return(i)
}

  


### LEAFLET MAPS

x = usgs_spatial %>% st_transform(4326)

ama3 = ama %>% st_transform(4326)
RColorBrewer::display.brewer.all(n=10, exact.n=FALSE)

pal1 = RColorBrewer::brewer.pal(9,"Blues")
pal2 = RColorBrewer::brewer.pal(9,"YlOrRd")
pal3 = RColorBrewer::brewer.pal(9,"YlGnBu")

pals1 = colorNumeric(pal1, domain = x$dtw)
pals2 = colorNumeric(pal2, domain = x$dtw)
pals3 = colorBin(pal3, domain = 1:8)


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


multi_well(x, usgs_time, 600, 700)
leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron, group = 'Tiles') %>% 
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
  addCircleMarkers(data = i, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(i, feature.id = FALSE,
                                               row.numbers = FALSE), group = '800 - 900 ft') %>%
  addCircleMarkers(data = j, #clusterOptions = markerClusterOptions(interactive()),
                   color = ~pals2(dtw), fillOpacity = .8,
                   stroke = FALSE,
                   popup = leafpop::popupTable(j, feature.id = FALSE,
                                               row.numbers = FALSE), group = '900 > ft') %>%
  # addCircleMarkers(data = x, #clusterOptions = markerClusterOptions(interactive()),
  #                  color = ~pals2(dtw), fillOpacity = .8,
  #                  stroke = FALSE,
  #                  popup = leafpop::popupTable(st_drop_geometry(x[,c(4, 7, 8, 13)]),
  #                                              feature.id = FALSE,
  #                                              row.numbers = FALSE), group = 'All') %>%
  addPolygons(data = ama3,
              fillColor  = ~pals3(OBJECTID), fillOpacity = 0.4,
              color = 'black',
              label = ~MAP_LABEL, group = 'AMA') %>%
  addLayersControl(overlayGroups = c('0 - 100 ft', '100 - 200 ft', '200 - 300 ft',
                                     '300 - 400 ft', '400 - 500 ft',
                                     '500 - 600 ft', '600 - 700 ft',
                                     '700 - 800 ft', '800 - 900 ft', '900 > ft', 'AMA'),
                   baseGroups = c("Tiles"))

ggplot(data = well, aes(x = date, y = dtw_ft)) +
  geom_line(aes(y = dtw_ft, col = wellid), size = 1) +
  scale_y_reverse() +
  labs(title = paste('Depth to water: well', id),
       # subtitle = min(wells$dtw_ft), '-', max(wells$dtw_ft),
       x = 'Year',
       y = 'DTW (ft)') + 
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', color = 'black', size = 20),
        axis.text.x = element_text(face = 'bold', color="black", size=14), 
        axis.text.y = element_text(face = 'bold', color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        legend.position = 'none')



plot_well(usgs_time, 3450)
tmppp = buffer_fun(usgs_spatial, 575, 10000, az)
usgs_time %>% filter(wellid == 'well 24400')

# pheonix
plot_well(usgs_time, 5284)
plot_well(usgs_time, 5324)

# Harquana AMA
plot_well(usgs_time, 292)
plot_well(usgs_time, 504)

# Kingman
plot_well(usgs_time, 1139)

plot_well(usgs_time, 575)

multi_well_plot(usgs_spatial, usgs_time, 500, 1000)

temp = multi_well_plot(usgs_spatial, usgs_time, 500, 600)

multi_well_plot(usgs_spatial, usgs_time, 600, 700)

multi_well_plot(usgs_spatial, usgs_time, 700, 800)

multi_well_plot(usgs_spatial, usgs_time, 900, 1400)

#for (i in ama2) {
#  tmp = st_intersection(x, ama2[i,])
#}

#tmp = tmp %>% group_by(OBJECTID) 


#ama4 = ama3 %>% st_join(tmp, by = 'OBJECTID')


                   

map_dtw(az, usgs_spatial, 500, 600)


plot_well(usgs_time, 2)























# SQ GRID MAP OF WELLS
library(rmapshaper)
sq_grid = st_make_grid(az, n = 80) %>% 
  st_as_sf() %>%
  mutate(id = 1:n())
plot(sq_grid)

az_u = az %>%
  st_union() %>%
  st_cast() %>%
  st_sf() %>%
  ms_simplify(keep = .05)

sq_clip = st_intersection(az_u, sq_grid)
az_5070 = az_nwis_spatial %>% st_transform(5070)

pips = pip_function(az_5070, sq_clip, 'id')
plot_pip(pips, 'Arizona Groundwater wells')
















     
     
     
     
     
