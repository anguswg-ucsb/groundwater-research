
# Angus Watters
# Arizona Groundwater 
# 11-26-2020

library(tidyverse)
library(sf)
library(USAboundaries)
library(sp)
library(leaflet)
library(plotly)

source("docs/utils.R")

usgs = readRDS('data/usgs-unique-sites.rds')
usgs_time = readRDS("data/usgs-time.rds")
state = readRDS('data/state-unique-site.rds')
state_time = readRDS('data/state-time.rds')


join_time = readRDS("data/join_time.rds")
join = readRDS('data/join_az.rds')
join_spatial = readRDS('data/join_spatial.rds')

# FILTER BY THRESHOLD - atleast 10 measurements and atleast 1 measurement every 5 years
thresh = join_time %>% 
  group_by(wellid) %>% 
  filter(measurement_dist >= 10) %>% 
  mutate(measure_period = year - lag(year)) %>%
  filter(measure_period > 5)

join_time = join_time %>% 
  filter(!wellid %in% thresh$wellid, measurement_dist >= 10) %>%
  mutate(measure_period = year - lag(year))
join_time$range = cut(join_time$dtw, breaks = seq(0, 2000, by = 100),
                      labels = paste('R', 1:20, sep = '' )) 
join_spatial = join_spatial %>%
  filter(!wellid %in% thresh$wellid) %>%
  filter(measurement_dist >= 10)

join_spatial$range = cut(join_spatial$dtw, breaks = seq(0, 2000, by = 100),
                         labels = paste('R', 1:20, sep = '' )) 
################################################
################################################
class(pb1)
pb1 = plotBuffer(join_spatial, 4573, 12500)
f1 = 'img/plotly-buff1.html'
htmlwidgets::saveWidget(pb1, file = 'img/plotly-buff1.html', selfcontained = FALSE)
htmlwidgets::saveWidget(pb1,file.path(normalizePath(dirname(f1)),basename(f1)))

################################################
################################################

# complete time series plots
m1 = join_time %>% filter(between(date_min, 1924.000, 1935.000), date_max > 1970.000)

m2 = join_time %>% filter(between(dtw, 900.00, 1500))


plotMultipleWells(m1)
plotMultipleWells(m2)

ggplot(mtcars, aes(mpg, disp, colour = factor(cyl))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

mtcars %>% 
  group_by(cyl) %>% 
  do({
    mod = lm(disp ~ mpg, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })


################################################
################################################
tmp = join_time %>% filter(wellid == 'well 4679')



# Pinal 
plot_well(join_time, 4669)
plot_well(join_time, 4686) 
plot_well(join_time, 16989)

# Douglas
plot_well(join_time, 2581)
plot_well(join_time, 4415) 

# Santa Cruz
plot_well(join_time, 9214)
plot_well(join_time, 5243)


az_leaflet

multi_well_plot(join_spatial, join_time, 500, 600)

################################################
################ BUFFER ANALYSIS ###############
################################################

safford = join_spatial %>% filter(wellid == 'well 470')
buffer1 = st_buffer(join_spatial[safford, ], 12500)
near_safford = st_intersection(join_spatial, buffer1)
near_time = join_time %>% filter(wellid %in% near_safford$wellid, year > 1995)
plotMultipleWells(near_time)


king = join_spatial %>% filter(wellid == 'well 1971')
buffer2 = st_buffer(join_spatial[king, ], 20000)
near_king = st_intersection(join_spatial, buffer2)
near_time2 = join_time %>% filter(wellid %in% near_king$wellid)
plotMultipleWells(near_time2)

temp = join_time %>%
  filter(range %in% c('R1', 'R2')) %>% 
  mutate(date_length = date_max - date_min) %>% 
  filter(date_length > 40) %>% 
  group_by(wellid) %>% 
  arrange(desc(measurement_dist)) %>% 
  slice_max(n = 5)

plotMultipleWells(temp)

multi_well_plot(join_spatial, temp, 0, 100)
################################################
############## LEAFLET DATA  ###################
################################################

# Arizona state shape (CRS = 5070)
az = us_states() %>%
  filter(state_name == 'Arizona') %>% 
  st_transform(5070)

# read in AMA shapefiles
ama = read_sf('data/Act_Man_Areas.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")
ama2 = ama %>% st_transform(4326)

### AQUIFERS
aquifer = read_sf('data/shps/us_aquifers.shp') %>% 
  st_transform(5070) %>% 
  st_cast("MULTIPOLYGON")

aquifer2 = aquifer %>% st_transform(4326)

box = az %>% st_transform(4326) %>% 
  st_bbox() %>% 
  st_as_sfc()
temp = st_intersects(aquifer2, box)

aquifer2 = aquifer2[which(lengths(temp) != 0), ]


### LEAFLET MAPS
x = join_spatial %>% st_transform(4326)

r1 = dtw_range(x, 0, 100) %>% select(wellid, date, dtw, measurement_dist)
r2 = dtw_range(x, 100, 200) %>% select(wellid, date, dtw, measurement_dist)
r3 = dtw_range(x, 200, 300) %>% select(wellid, date, dtw, measurement_dist)
r4 = dtw_range(x, 300, 400) %>% select(wellid, date, dtw, measurement_dist)
r5 = dtw_range(x, 400, 500) %>% select(wellid, date, dtw, measurement_dist)
r6= dtw_range(x, 500, 600) %>% select(wellid, date, dtw, measurement_dist)
r7 = dtw_range(x, 600, 700) %>% select(wellid, date, dtw, measurement_dist)
r8 = dtw_range(x, 700, 800) %>% select(wellid, date, dtw, measurement_dist)
r9 = dtw_range(x, 800, 900) %>% select(wellid, date, dtw, measurement_dist)
r10 = dtw_range(x, 900, 1000) %>% select(wellid, date, dtw, measurement_dist)
r11 = dtw_range(x, 1000, 1400) %>% select(wellid, date, dtw, measurement_dist)





################################################
############ WELL DECLINE ANALYSIS #############
################################################
font = list(
  family = 'Courier',
  size = 15,
  color = 'white')
label = list(
  bgcolor = '#232F34',
  bordercolor = 'transparent',
  font = font)

cont_records = join_time %>%
  filter(wellid != 6008) %>% 
  mutate(time_span = max(year) - min(year)) %>% 
  filter(time_span > 65)


plotBuffer(join_spatial, 4573, 12500)
m = join_spatial %>% head(200)
plotMultipleWells(m)


# Range: 300 - 500 ft ------------ 65 years
plotRange(cont_records, 300, 500) 
plotBuffer(join_spatial, 4573, 12500)
plotBuffer(join_spatial, 4740, 12500)
plotBuffer(join_spatial, 3472, 12500)

# Range: 400 - 500 ft ------------ 65 years
plotRange(cont_records, 400, 500)
plotBuffer(join_spatial, 4401, 12500)
plotBuffer(join_spatial, 4182, 12500)

t = join_spatial %>% filter(wellid %in% c('4471', '4573'))
tt = join_spatial %>% filter(wellid %in% c('107', '1087'))
ggplot() +
  geom_sf(data = az2) +
  geom_sf(data = t) +
  geom_sf(data = tt, color = 'red')
az2 = az %>% st_transform(4326)
# 4573
w1 = join_spatial %>% filter(wellid == 4573)
b1 = st_buffer(join_spatial[w1, ], 12500)
n1 =st_intersection(join_spatial, b1)
time1 = cont_records %>% filter(wellid %in% n1$wellid)
plotMultipleWells(time1)

df_time = join_time %>% filter(time_span > 65, wellid %in% n1$wellid) 
plotBuffer(join_spatial, 4573, 12500)

# 4740 ****************
w2 = join_spatial %>% filter(wellid == 4740)
b2 = st_buffer(join_spatial[w2, ], 1250)
n2 =st_intersection(join_spatial, b2)
time2 = cont_records %>% filter(wellid %in% n2$wellid)
plotMultipleWells(time2)
plotBuffer(join_spatial, 4740, 12500)

# 3472
w3 = join_spatial %>% filter(wellid == 3472)
b3 = st_buffer(join_spatial[w3, ], 12500)
n3 =st_intersection(join_spatial, b3)
time3 = cont_records %>% filter(wellid %in% n3$wellid)
plotMultipleWells(time3)
plotBuffer(join_spatial, 3472, 12500)

# Range: 400 - 500 ft ------------ 65 years
plotRange(cont_records, 400, 500)

# 4401
w4 = join_spatial %>% filter(wellid == 4401)
b4 = st_buffer(join_spatial[w4, ], 12500)
n4 =st_intersection(join_spatial, b4)
time4 = cont_records %>% filter(wellid %in% n4$wellid)
plotMultipleWells(time4)

# 4740 *******************************
w5 = join_spatial %>% filter(wellid == 4740)
b5 = st_buffer(join_spatial[w5, ], 12500)
n5 =st_intersection(join_spatial, b5)
time5 = cont_records %>% filter(wellid %in% n5$wellid)
plotMultipleWells(time5)

# 3435 *********************
w6 = join_spatial %>% filter(wellid == 3435)
b6 = st_buffer(join_spatial[w6, ], 12500)
n6 =st_intersection(join_spatial, b6)
time6 = cont_records %>% filter(wellid %in% n6$wellid)
plotMultipleWells(time6)

# 4182
w7 = join_spatial %>% filter(wellid == 4182)
b7 = st_buffer(join_spatial[w7, ], 12500)
n7 =st_intersection(join_spatial, b7)
time7 = cont_records %>% filter(wellid %in% n7$wellid)
plotMultipleWells(time7)

# 4993 *************************
w8 = join_spatial %>% filter(wellid == 4993)
b8 = st_buffer(join_spatial[w8, ], 20000)
n8 =st_intersection(join_spatial, b8)
time8 = cont_records %>% filter(wellid %in% n8$wellid)
plotMultipleWells(time8)

# 3450 ***************
w9 = join_spatial %>% filter(wellid == 3450)
b9 = st_buffer(join_spatial[w9, ], 12500)
n9 =st_intersection(join_spatial, b9)
time9 = cont_records %>% filter(wellid %in% n9$wellid)
plotMultipleWells(time9)

# Range: 500 - 600 ft ------------ 50 years
plotRange(cont_records, 500, 600)

# 4953 ----- near well 4993
w10 = join_spatial %>% filter(wellid == 4953)
b10 = st_buffer(join_spatial[w10, ], 20000)
n10 =st_intersection(join_spatial, b10)
time10 = cont_records %>% filter(wellid %in% n10$wellid)
plotMultipleWells(time10)


# 5730 ------------- 30 km from nearest wells
w11 = join_spatial %>% filter(wellid == 5730)
b11 = st_buffer(join_spatial[w11, ], 30000)
n11 =st_intersection(join_spatial, b11)
time11 = cont_records %>% filter(wellid %in% n11$wellid)
plotMultipleWells(time11)

# 1853 
w12 = join_spatial %>% filter(wellid == 1853)
b12 = st_buffer(join_spatial[w12, ], 12500)
n12 =st_intersection(join_spatial, b12)
time12 = cont_records %>% filter(wellid %in% n12$wellid)
plotMultipleWells(time12)

# 3450
w13 = join_spatial %>% filter(wellid == 3450)
b13 = st_buffer(join_spatial[w13, ], 12500)
n13 =st_intersection(join_spatial, b13)
time13 = cont_records %>% filter(wellid %in% n13$wellid)
plotMultipleWells(time13)

# 3450
w14 = join_spatial %>% filter(wellid == 3450)
b14 = st_buffer(join_spatial[w14, ], 12500)
n14 =st_intersection(join_spatial, b14)
time14 = cont_records %>% filter(wellid %in% n14$wellid)
plotMultipleWells(time14)

plotWell(join_time, 4401)
# Range: 600 - 1400 ft ------------ 50 years
plotRange(cont_records, 600, 1400)

# 5743 ------- (~ 1200 ft) -------- 20 km buffer
w13 = join_spatial %>% filter(wellid == 5743)
b13 = st_buffer(join_spatial[w13, ], 20000)
n13 =st_intersection(join_spatial, b13)
time13 = cont_records %>% filter(wellid %in% n13$wellid)
plotMultipleWells(time13)







plotBuffer(join_spatial, 4401, 10000)








################################################
################################################
################################################



################################################
############## AMA WELL ANALYSIS ###############
################################################

# Joseph City AMA
joseph_shp = ama2 %>% 
  filter(OBJECTID == 1) %>% 
  st_intersection(x)
joseph_wells = join_time %>% filter(wellid %in% joseph_shp$wellid)


plot_test1 = ggplot(data = joseph_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

joseph_ama_wells = ggplotly(plot_test1, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Prescott AMA
presc_shp = ama2 %>% 
  filter(OBJECTID == 2) %>% 
  st_intersection(x)
presc_wells = join_time %>% filter(wellid %in% presc_shp$wellid)
plotMultipleWells(presc_wells)

plot_test2 = ggplot(data = presc_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

prescott_ama_wells = ggplotly(plot_test2, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Pheonix AMA
pheonix_shp = ama2 %>% 
  filter(OBJECTID == 3) %>% 
  st_intersection(x)
pheonix_wells = join_time %>% filter(wellid %in% pheonix_shp$wellid)
plotMultipleWells(pheonix_wells)

pheonix_wells = pheonix_wells %>% 
  filter(!range %in% c('R7', 'R8', 'R9', 'R10')) %>% 
  mutate(date_length = date_max - date_min) %>% 
  filter(date_length > 40)


plot_test3 = ggplot(data = pheonix_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

pheonix_ama_wells = ggplotly(plot_test3, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)

# Harquahala AMA
harq_shp = ama2 %>% 
  filter(OBJECTID == 4) %>% 
  st_intersection(x)
harq_wells = join_time %>% filter(wellid %in% harq_shp$wellid)
plotMultipleWells(harq_wells)

plot_test4 = ggplot(data = harq_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test4, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)

# Pinal AMA
pinal_shp = ama2 %>% 
  filter(OBJECTID == 5) %>% 
  st_intersection(x)
pinal_wells = join_time %>% filter(wellid %in% pinal_shp$wellid)
plotMultipleWells(pinal_wells)

plot_test5 = ggplot(data = pinal_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test5, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Tuscon AMA
tucson_shp = ama2 %>% 
  filter(OBJECTID == 6) %>% 
  st_intersection(x)
tucson_wells = join_time %>% filter(wellid %in% tucson_shp$wellid)
plotMultipleWells(tucson_wells)

plot_test6 = ggplot(data = tucson_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')
# Joseph City AMA
joseph_shp = ama2 %>% 
  filter(OBJECTID == 1) %>% 
  st_intersection(x) %>% 
  mutate(name = 'JOSEPH CITY')
joseph_wells = join_time %>% filter(wellid %in% joseph_shp$wellid)


plot_test1 = ggplot(data = joseph_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  ylim(1000, 0) +
  geom_smooth(col = 'black') +
  labs(title = paste(joseph_shp$name),
       x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10))

joseph_ggplotly = ggplotly(plot_test1, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
joseph_ggplotly
ggplotly(plot_test6, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
# Douglas AMA
douglas_shp = ama2 %>% 
  filter(OBJECTID == 7) %>% 
  st_intersection(x)
douglas_wells = join_time %>% filter(wellid %in% douglas_shp$wellid)

plot_test7 = ggplot(data = douglas_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test7, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)


# Santa Cruz AMA
santacruz_shp = ama2 %>% 
  filter(OBJECTID == 8) %>% 
  st_intersection(x)
santacruz_wells = join_time %>% filter(wellid %in% santacruz_shp$wellid)

plot_test8 = ggplot(data = santacruz_wells, aes(x = date, y = dtw)) +
  geom_line(aes(col = wellid), size = 1) +
  scale_y_reverse() +
  geom_smooth(col = 'black') +
  labs(x = 'Year',
       y = 'DTW (ft)') +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1),
        strip.text.x = element_text(face = 'bold', size = 10), legend.position = 'none')

ggplotly(plot_test8, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)


######################################################
################### NEGATIVE DTW #####################
######################################################



#Criteria: > =10 measurements, >= 5 distinct years, 1 pt after 2010, 1 pt before 1980 

spear_rank = join_time %>% 
  group_by(wellid) %>% 
  summarise(spear_rank = cor(dec_date, dtw, method = 'spearman'))

join_spear = inner_join(spear_rank, join_time, by = 'wellid') %>% 
  filter(year_dist >= 5, date_max >= 2010, date_min <= 1980) 

neg_wells = join_spear %>% filter(dtw < 0)

split_df = split(neg_wells, neg_wells$wellid)



neg1 = join_time %>% filter(wellid %in% r0$wellid)

plotMultipleWells(neg1)


gg = ggplot(data = near_time, aes(x = date, y = dtw)) +
  geom_line(aes(y = dtw, col = wellid), size = 1) +
  scale_y_reverse() +
  labs(x = 'Year',
       y = 'DTW (ft)') + 
  theme_bw() +
  theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5), 
        axis.text.x = element_text(color="black", size=14), 
        axis.text.y = element_text(color="black", size=14), 
        axis.title.x = element_text(face="bold", color="black", size=16), 
        axis.title.y = element_text(face="bold", color="black", size=16), 
        panel.grid.major = element_line(colour = "#808080"),
        panel.grid.minor = element_line(colour = "#808080", size = 1))
plot = ggplotly(gg, tooltip = c('x', 'y', 'wellid')) %>%
  style(hoverlabel = label) %>% 
  layout(font = font, 
         yaxis = list(fixedrange = TRUE)) %>% 
  config(displayModeBar = FALSE)
print(plot)

plotMultipleWells(nea)
######### Negative well time series ###############

# well 1
neg2 = join_spatial %>% filter(wellid == 1)
buff2 = st_buffer(join_spatial[neg2, ], 15000)
near_neg2 = st_intersection(join_spatial, buff2)
near_time2 = join_time %>% filter(wellid %in% near_neg2$wellid)
plotNegativeWells(near_time2)
# well 222
neg3 = join_spatial %>% filter(wellid == 222)
buff3 = st_buffer(join_spatial[neg3, ], 12500)
near_neg3 = st_intersection(join_spatial, buff3)
near_time3 = join_time %>% filter(wellid %in% near_neg3$wellid)
plotNegativeWells(near_time3)
# well 524
neg1 = join_spatial %>% filter(wellid == 524)
buff1 = st_buffer(join_spatial[neg1, ], 15000)
near_neg1 = st_intersection(join_spatial, buff1)
near_time1 = join_time %>% filter(wellid %in% near_neg1$wellid)
plotNegativeWells(near_time1)
# Chino Valley
neg4 = join_spatial %>% filter(wellid == 5534)
buff4 = st_buffer(join_spatial[neg4, ], 12500)
near_neg4 = st_intersection(join_spatial, buff4)
near_time4 = join_time %>% filter(wellid %in% near_neg4$wellid)
plotNegativeWells(near_time4)



######################################################
################### RANGE ANALYSIS ###################
######################################################

# SLOPE
slope = join_time %>% 
  group_by(wellid) %>% 
  mutate(rownum = row_number()) %>% 
  summarise(slope = lm(dtw ~ rownum)$coefficients['rownum'])


# 250 - 400 ft
range300 = join_time %>% 
  filter(dtw > 250, dtw < 400) %>% 
  add_count(wellid) %>% 
  mutate(sd = sd(dtw)) %>% 
  filter(n >= 100, sd < 36)
plotMultipleWells(range300)
library(htmlwidgets)

htmlwidgets::saveWidget(prescott_ggplotly, file = 'img/prescott-ama-plot.')
prescott_ggplotly
# 400 - 600 ft
range400 = join_time %>% 
  filter(dtw > 400, dtw < 600) %>% 
  add_count(wellid) %>% 
  mutate(sd = sd(dtw)) %>% 
  filter(n >= 60, sd < 55)

plotMultipleWells(range400)

BAMMtools::getJenksBreaks(range400$sd, 4)

# 600 - 1100 ft
range600 = join_time %>% 
  filter(dtw > 600, dtw < 1100) %>% 
  add_count(wellid) %>% 
  mutate(sd = sd(dtw)) %>% 
  filter(n >= 50)

plotMultipleWells(range0)


hist(range600$n)

hist(range0$n, breaks = 100)

# Wells with dramatic decrease in DTW
# 5934
# 5936

# 3211
# 4993

# 5934
decr1 = join_spatial %>% filter(wellid == 5934)
rad1 = st_buffer(join_spatial[decr1, ], 15000)
near_decr1 = st_intersection(join_spatial, rad1)
decr1_time = join_time %>% filter(wellid %in% near_decr1$wellid)
plotMultipleWells(decr1_time)


decr2 = join_spatial %>% filter(wellid == 4993)
rad2 = st_buffer(join_spatial[decr2, ], 15000)
near_decr2 = st_intersection(join_spatial, rad2)
decr2_time = join_time %>% filter(wellid %in% near_decr2$wellid)
plotMultipleWells(decr2_time)

decr3 = join_spatial %>% filter(wellid == 3211)
rad3 = st_buffer(join_spatial[decr3, ], 15000)
near_decr3 = st_intersection(join_spatial, rad3)
decr3_time = join_time %>% filter(wellid %in% near_decr3$wellid)
plotMultipleWells(decr3_time)

# post 1980 well replenishment
# 4401
# 4493
# 4070








