


#  POINT-IN-POLYGON 
pip_function = function(points, polygon, bar){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(bar)) %>%
    setNames(c(bar, "n")) %>%
    left_join(polygon, by = bar) %>%
    st_as_sf()
}
# PLOTS POINT-IN-POLYGON 
plot_pip = function(data, text1){
  ggplot() +
    geom_sf(data = data, aes(fill = n), alpha = .9, size = .2) +
    viridis::scale_fill_viridis() +
    theme_void() +
    theme(plot.title = element_text(face = "bold", color = "black", size = 24), plot.subtitle = element_text(size = 12),
          plot.caption = element_text(face = 'bold', size = 12), legend.title = element_text(face = 'bold'),
          legend.text = element_text(face = 'bold')) +
    labs(title = text1,
         fill = 'Number of dams',
         caption = paste0(sum(data$n), " dams")) +
    theme(aspect.ratio = .5)
}





#TIME SERIES DATA - FUNCTION RETURNS DATAFRAME + GRAPH OF DTW TRENDS OVER TIME
plot_trends = function(df_time, thresh, measurements, state) {
  df_time = df_time %>% 
    filter(measurement_dist >= measurements) %>%
    mutate(sd = sd(dtw))
  flat = df_time %>% filter(sd <= thresh, measurement_dist >= measurements) %>% 
    mutate(trend = 'flat')
  osc = df_time %>% filter(sd > thresh, measurement_dist >= measurements) %>% 
    mutate(trend = 'oscilate')
  well_trends = full_join(osc, flat)
  plot1 = ggplot(data = well_trends, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = site_id)) +
    scale_y_reverse() +
    labs(title = 'All wells (DTW vs. Time)',
         x = 'Year',
         y = 'DTW (ft)') + 
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', color = 'black', size = 18),
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face="bold", color="black", size=16), 
          axis.title.y = element_text(face="bold", color="black", size=16)) + 
    theme(legend.position = "none")
  plot2 = ggplot(data = osc, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = site_id)) +
    scale_y_reverse() +
    labs(title = 'Oscilating wells (DTW vs. time)',
         x = 'Year',
         y = 'DTW (ft)') +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', color = 'black', size = 18),
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face="bold", color="black", size=16), 
          axis.title.y = element_text(face="bold", color="black", size=16)) + 
    theme(legend.position = "none")
  plot3 = ggplot(data = flat, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = site_id)) +
    scale_y_reverse() +
    labs(title = 'Flat wells (DTW vs. time)', 
         x = 'Year',
         y = 'DTW (ft)') +
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', color = 'black', size = 18),
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face="bold", color="black", size=16), 
          axis.title.y = element_text(face="bold", color="black", size=16)) + 
    theme(legend.position = "none")
  map_data = well_trends %>% distinct(site_id, .keep_all = TRUE) %>% 
    st_as_sf(coords = c('long_nad83', 'lat_nad83'), crs = 4269) %>% 
    st_transform(5070)
  map1 = ggplot() + 
    geom_sf(data = state) +
    geom_sf(data = map_data, aes(col = trend), size = 1.5) + 
    scale_color_manual(values = c('flat' = "red", 'oscilate' = "blue")) +
    theme_bw() +
    labs(title = 'Variance in well DTW',
         col = 'Trend', 
         subtitle = paste('Flat vs oscilating classification determined by standard deviation of', thresh)) +
    theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5),
          plot.title = element_text(face = 'bold', color = 'black', size = 18),
          legend.title = element_text(color="black", size=14), 
          legend.text = element_text(color="black", size=14))
  plot_all = plot_grid(plot1 + theme(axis.title.x = element_blank()), map1, plot2, plot3 + theme(axis.title.y = element_blank()), nrow = 2)
  print(plot_all)
  return(well_trends)
}


plot_dtw = function(df_time, min, max) {
  df_time = df_time %>% filter(dtw <= max, dtw >= min) %>% 
    mutate(sd = sd(dtw))
  plot1 = ggplot(data = df_time, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = wellid), size = 1) +
    scale_y_reverse() +
    labs(title = paste('Depth to water', min, '-', max, 'ft'),
         x = 'Year',
         y = 'DTW (ft)') + 
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', color = 'black', size = 20),
          axis.text.x = element_text(face = 'bold', color="black", size=14), 
          axis.text.y = element_text(face = 'bold', color="black", size=14), 
          axis.title.x = element_text(face="bold", color="black", size=16), 
          axis.title.y = element_text(face="bold", color="black", size=16), 
          legend.position = 'none')
  print(plot1)
  return(df_time)
}


#mypalette = RColorBrewer::brewer.pal(9,"YlOrRd")
#image(1:7,1,as.matrix(1:7),col=mypalette,xlab="Greens (sequential)",
 #     ylab="",xaxt="n",yaxt="n",bty="n")
#RColorBrewer::display.brewer.all(n=10, exact.n=FALSE)
#devAskNewPage(ask=TRUE)

map_dtw = function(state, df, min, max) {
  df = df %>% filter(dtw <= max, dtw >= min) %>% 
    mutate(sd = sd(dtw)) %>% 
    arrange(dtw)
  plot1 = ggplot() + 
    geom_sf(data = state, size = 1, col = 'black') +
    geom_sf(data = df, aes(col = dtw), size = 1.5) + 
    theme_void() +
    theme(plot.title = element_text(face = "bold", color = "black", size = 18),
          plot.subtitle = element_text(size = 12),
          plot.caption = element_text(face = 'bold', size = 12), 
          legend.title = element_text(color="black", size=14), 
          legend.text = element_text(color="black", size=14)) +
    scale_colour_gradient() +
    labs(title = paste('DTW', min, '-', max, 'ft'),
         fill = 'Number of wells', 
         col = 'Depth to Water (ft)')
  print(plot1)
  return(df)
}


# RETURNS POINTS WITHIN BUFFER AREA + PLOT
# arguments: USGS dataframe, State dataframe, specified well, desired buffer distance, state shape
# returns dataframe of wells within buffer area & returns ggplot of buffered well and points within buffer
buffer_fun1 = function(df, df2, well, buff, state) {
  buffer = st_buffer(df[well,], buff)
  near1 = st_intersection(df[,], buffer) %>% filter(measurement_dist >= 10)
  near2 = st_intersection(df2[,], buffer) %>% filter(measurement_dist >= 10)
  
  plot1 = ggplot() + 
    geom_sf(data = state) +
    geom_sf(data = buffer, fill = NA) + 
    geom_sf(data = near1, col = "red", size = 1) + 
    geom_sf(data = near2, col = "black", size = 1) +
    labs(title = paste('Wells within', buff, 'meter buffer of'),
         caption = paste(nrow(near2), 'wells')) +
    theme_void() +
    theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5))
  plot2 = ggplot(data = near2, aes(x = wellid, y = dtw_ft)) +
    geom_col(aes(y = dtw_ft), fill = 'cyan', size = 1) +
    scale_y_reverse() +
    labs(title = 'DTW State wells',
         x = 'Well',
         y = 'DTW (ft)') + 
    theme_bw() +
    theme(plot.title = element_text(face = 'bold', color = 'black', size = 20),
          axis.text.x = element_text(angle=90, face = 'bold', color="black", size=14, hjust = .3), 
          axis.text.y = element_text(face = 'bold', color="black", size=14),
          axis.title.x = element_text(face="bold", color="black", size=16), 
          axis.title.y = element_text(face="bold", color="black", size=16))
          #legend.position = element_blank())
  plot3 = plot_grid(plot2, plot1, nrow = 1)
  print(plot3)
  return(near1)
  return(near2)
} 

# BUFFER FUNCTION FOR 1 DATAFRAME INPUT
buffer_fun = function(df, well, buff, state) {
  buffer = st_buffer(df[well,], buff)
  near1 = st_intersection(df[,], buffer) %>% filter(measurement_dist >= 10)
  
  plot = ggplot() + 
    geom_sf(data = state) +
    geom_sf(data = buffer, fill = NA) + 
    geom_sf(data = near1, col = "red", size = .5) + 
    labs(caption = paste(nrow(near1), 'wells')) +
    theme_void() +
    theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5))
  print(plot)
  return(near1)
}
#buffer_fun = function(df, well, buff, state) {
  #if(missing(df2)) {
  #buffer = st_buffer(df[well,], buff)
  #near1 = st_intersection(df[,], buffer)
  #near2 = st_intersection(df2[,], buffer)
  
 # plot = ggplot() + 
  #  geom_sf(data = state) +
   # geom_sf(data = buffer, fill = NA) + 
    #geom_sf(data = near1, col = "red", size = .5) + 
    #geom_sf(data = near2, col = "black", size = .5) +
    #labs(caption = paste(nrow(near), 'wells')) +
    #theme_void() +
    #theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5))
  #print(plot)
  #return(near1)
#} else {
 # buffer = st_buffer(df[well,], buff)
  #near1 = st_intersection(df[,], buffer)
  #near2 = st_intersection(df2[,], buffer)
  
#  plot2 = ggplot() + 
  #  geom_sf(data = state) +
 #   geom_sf(data = buffer, fill = NA) + 
  #  geom_sf(data = near1, col = "red", size = .5) + 
   # geom_sf(data = near2, col = "black", size = .5) +
    #labs(caption = paste(nrow(near), 'wells')) +
  #  theme_void() +
   # theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5))
  #print(plot2)
  #return(near1)
  #return(near2)
#}
#}




### RETURNS DATAFRAME OF WELLS IN DTW RANGE (MIN - MAX)
dtw_range = function(df, min, max) {
  df = df %>% filter(dtw <= max, dtw >= min) %>% 
    mutate(sd = sd(dtw)) %>% 
    arrange(desc(date))
  return(df)
}

### PLOT AN INDIVIDUAL WELL BY WELL ID (DTW TIME SERIES)
plot_well = function(df_time, id) {
  wells = df_time %>% filter(wellid == paste('well', id))
  plot = ggplot(data = wells, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = wellid), size = 2) +
    scale_y_reverse() +
    labs(title = paste('Well', id),
         caption = paste('Min depth:', min(wells$dtw), '\nMax depth:',
                          max(wells$dtw), '\nNumber of measurements:',
                          wells$measurement_dist),
         x = 'Year',
         y = 'DTW (ft)') + 
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5),
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face = 'bold', color="black", size=16), 
          axis.title.y = element_text(face = 'bold', color="black", size=16),
          plot.caption = element_text(face = 'bold', color = 'black', size = 14),
          panel.grid.major = element_line(colour = "#808080"),
          panel.grid.minor = element_line(colour = "#808080", size = 1),
          legend.position = 'none') 
  print(plot)
}

### PLOT AN INDIVIDUAL WELL BY WELL ID (DTW TIME SERIES)
plot_well_state = function(df_time, id) {
  df_time = usgs_time
  wells = df_time %>% filter(wellid == paste('well', id))
  plot = ggplot(data = wells, aes(x = date, y = dtw)) +
    geom_line(aes(y = dtw, col = wellid), size = 2) +
    scale_y_reverse() +
    labs(title = paste('Well', id),
         x = 'Year',
         y = 'DTW (ft)') + 
    theme_bw() +
    theme(plot.title = element_text(face = 'bold',color = 'black', size = 18, hjust = 0.5),
          axis.text.x = element_text(color="black", size=14), 
          axis.text.y = element_text(color="black", size=14), 
          axis.title.x = element_text(face = 'bold', color="black", size=16), 
          axis.title.y = element_text(face = 'bold', color="black", size=16),
          plot.caption = element_text(face = 'bold', color = 'black', size = 14),
          panel.grid.major = element_line(colour = "#808080"),
          panel.grid.minor = element_line(colour = "#808080", size = 1),
          legend.position = 'none') 
  print(plot)
}

# PLOT WELLS IN DTW RANGE
multi_well_plot = function(df, df_time, min, max) {
  df = df %>% filter(dtw <= max, dtw >= min) %>% 
    arrange(desc(date))
  df_time = df_time %>% filter(wellid %in% df$wellid)
  plot1 = ggplot(data = df_time, aes(x = date, y = dtw)) +
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
          panel.grid.minor = element_line(colour = "#808080", size = 1),
          legend.position = 'none')
  print(plot1)
  return(df)
}

### MAP WELLS IN AMA BY SPECIFIC AREA (1 - 8)
map_ama = function(df1, df2, ama, state) {
  i = st_intersection(df1, df2[ama,])
  
  plot1 = ggplot() + 
    geom_sf(data = state) +
    geom_sf(data = df2, fill = NA) + 
    geom_sf(data = i, aes(col = dtw), size = .5) + 
    scale_colour_gradient(guide=guide_colourbar(reverse = TRUE)) +
    labs(caption = paste(nrow(i), 'wells')) +
    theme_void() +
    theme(plot.caption = element_text(size = 22, face = "bold", hjust = 0.5))
  plot2 = ggplot(df2[1:8, ]) +
    geom_sf(data = state) +
    geom_sf(data = df2, aes(fill = OBJECTID)) +
    geom_sf_label(aes(label = df2$OBJECTID))
  plot3 = plot_grid(plot2, plot1, nrow = 1)
  print(plot3)
  return(i)
}
