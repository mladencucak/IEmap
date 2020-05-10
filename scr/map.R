
################################################333
#Libraries
#####################################################

list.of.packages <-
  c(
    "tidyverse",
    "maps",
    "here",
    "ggthemes",
    "ggrepel",
    "sf",
    "ggspatial"
  )

new.packages <-
  list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

#Download packages that are not already present in the library
if (length(new.packages))
  install.packages(new.packages)

if (length(new.packages))
  install.packages(new.packages, repos = c(CRAN="https://cran.r-project.org/"))

packages_load <-
  lapply(list.of.packages, require, character.only = TRUE)

#Print warning if there is a problem with installing/loading some of packages
if (any(as.numeric(packages_load) == 0)) {
  warning(paste("Package/s", paste(list.of.packages[packages_load != TRUE]), "not loaded!"))
} else {
  print("All packages were successfully loaded.")
}

rm(packages_load, list.of.packages, new.packages)

################################################333
#Mapping
#####################################################

load(here::here("dat", "All_Ireland.RData"))#shape file for irish counties

#file with locations found in dat folder, This file could be modified to suit other needs
df_loc <- 
  read.csv( here::here("dat", "stations_final.csv"))

df_loc$lab <- 
  gsub("_", " (", df_loc$lab) %>% 
    paste0(., ")")

#Convert to simple features object
df_loc_sf <- 
  df_loc %>% 
  mutate(colstna = ifelse(stna %in% c("Oak Park", "Dunsany", "Moore Park", "Johnstown", "Gurteen"), "Observed", "Observed and forecasted" )) %>%  
  st_as_sf( agr = "lab",coords= c( "long","lat"),remove = FALSE)


#Set coordinate reference system
st_crs(df_loc_sf) <- 
  st_crs(all_counties.sf)

 # basemap <- 
ggplot() +
  # Plot borders (shapefile)
  geom_sf(
    data = 
      all_counties.sf[all_counties.sf$CountyName  != c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),],
    color= "#81E8C2",
    fill = "#81E8C2"
  ) +
  geom_sf(
    data = 
      all_counties.sf[all_counties.sf$CountyName  == c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),],
    color= "#F6F6B2",
    fill = "#F6F6B2"
  ) +
  #Set the theme
  theme_bw(base_family = "Roboto Condensed",
           base_size = 12 #Change the overall font size 
           ) +
  #limit the plotting area
  coord_sf(xlim = c(-11.4, -4.6), ylim = c(51.2, 55.65), expand = FALSE) +
  # Define names for labs
  labs(x = "Longitude", y = "Latitude")+
  #add fancy anotation
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.35, "in"),
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  
  annotation_scale(location = "br", width_hint = 0.4) 

# save the plot
ggsave(
  file = here::here("out" , "map.png"),
  plot = basemap,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 600
)

#Note that the plotted map will be different after applying the size/resolution settings 
#Hence it is best to take a look at it straight 
shell.exec(here::here("out" , "map.png"))


#---------------------- 
#Ireland with point locations

 pointmap <- 
basemap+
  geom_sf(
    data = df_loc_sf,
    aes(fill = colstna, color = colstna),
    shape = 23,
    size = 2
  ) +
  #Add names of stations
  geom_text_repel(data = df_loc_sf, 
                  aes(x = long, y = lat, label = lab),
                  size = 2.7
                  # nudge_x = c(1, -1.5, 2, 2, -1), 
                  # nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5)
  ) +
  #Change the border color. This section can be removed in single color of point is ok
  scale_color_manual(name = "Data:",
                     labels = c("Observed", "Observed&\nForecast"),
                     values = c( "black","blue")) +
  #change the fill manually
  scale_fill_manual(name = "Data:",
                    labels = c("Observed", "Observed&\nForecast"),
                    values = c("black","blue")) +
  theme(
    strip.background = element_blank(),
    legend.position = c(.16, .90), #place position of the legend inside plotting area
    legend.box.background = element_rect(color = "black", size = .5),
    legend.key = element_rect(colour = "transparent", fill = "white")
  )

# save the plot
ggsave(
  file = here::here("out" , "map_points.png"),
  plot = pointmap,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 600
)

shell.exec(here::here("out" , "map_points.png"))

#---------------------- 
#ROI map

roi_map <- 
  ggplot() +
  geom_sf(
    data = 
      all_counties.sf[all_counties.sf$CountyName  != c("Tyrone","Antrim","Armagh", "Fermanagh","Londonderry","Down"),],
    color= "black",
    fill = "white"
  ) +
  theme_bw(base_family = "Roboto Condensed",
           base_size = 12) +
  coord_sf(xlim = c(-11.4, -4.6), ylim = c(51.2, 55.65), expand = FALSE) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.35, "in"),
                         pad_y = unit(0.25, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x = "Longitude", y = "Latitude")+
  annotation_scale(location = "br", width_hint = 0.4) 

# save the plot
ggsave(
  file = here::here("out" , "roi_map.png"),
  plot = roi_map,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 600
)

shell.exec(here::here("out" , "roi_map.png"))
