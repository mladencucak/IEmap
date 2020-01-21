
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
    "sf"
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


library("ggspatial")


load(here::here("dat", "All_Ireland.RData"))

df_loc <- 
  read.csv( here::here("dat", "stations_final.csv"))

df_loc_sf <- 
  df_loc %>% 
  mutate(colstna = ifelse(stna %in% c("Oak Park", "Dunsany", "Moore Park", "Johnstown", "Gurteen"), "Observed", "Observed and forecasted" )) %>%  
  st_as_sf( agr = "lab",coords= c( "long","lat"),remove = FALSE)


#Set coordinate reference system
st_crs(df_loc_sf) <- 
  st_crs(all_counties.sf)


  
  ggplot() +
    geom_sf(
      data = all_counties.sf,
      color= "darkolivegreen3",
      fill = "darkolivegreen3"
    ) +
    geom_sf(data = df_loc_sf, aes(fill = colstna,color = colstna ),shape = 23, size =2) +
    geom_text_repel(data = df_loc_sf, 
                     aes(x = long, y = lat, label = lab),
                    size = 2.7
                    # nudge_x = c(1, -1.5, 2, 2, -1), 
                    # nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5)
                    ) +
    scale_color_manual(name = "Data:",
                       labels = c("Observed", "Observed&Forecast"),
                       values = c("blue", "black")) +
    scale_fill_manual(name = "Data:",
                      labels = c("Observed", "Observed&Forecast"),
                      values = c("blue", "black")) +
    theme_bw(base_family = "Roboto Condensed",
             base_size = 12) +
    coord_sf(xlim = c(-11.4, -4.6), ylim = c(51.2, 55.65), expand = FALSE) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.5, "in"),
                           pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    labs(x = "Longitude", y = "Latitude")+
    annotation_scale(location = "br", width_hint = 0.4) +
    theme(
      strip.background = element_blank(),
      legend.position = c(.18, .91),
       legend.box.background = element_rect(color = "black", size = .5),
      legend.key = element_rect(colour = "transparent", fill = "white")
    )+
  
  ggsave(
    file = here::here("out" , "map.png"),
    width = 15,
    height = 15,
    units = "cm",
    dpi = 600
  )
  
  shell.exec(here::here("out" , "map.png"))
  