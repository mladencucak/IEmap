
################################################333
#Libraries
#####################################################

list.of.packages <-
  c(
    "tidyverse",
    "maps",
    "here",
    "ggthemes",
    "ggrepel"
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

ireland = fortify(map_data("world", region = "ireland"))
ni = fortify(map_data("world", region = "uk"))
ni <- ni[ni$subregion == "Northern Ireland",]
ireland <- bind_rows(ireland,ni)

df_loc <- 
  read.csv( here::here("dat", "stations_final.csv"))

df_loc %>%   
  mutate(colstna = ifelse(stna %in% c("Oak Park", "Dunsany", "Moore Park", "Johnstown", "Gurteen"), "Observed", "Observed and forecasted" )) %>% 
  ggplot() + 
  geom_polygon(data = ireland, aes(x=long, y = lat, group = group), fill = "darkolivegreen3")  +
  coord_fixed(1.5)+
  geom_point( aes(x = long, y = lat, fill = colstna, color = colstna, alpha = 0.8), size = 2.3, shape = 17) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  scale_color_manual(name = "Data:", values = c( "blue","black"))+
  labs(x = "Longitude", y = "Latitude")+
  ggrepel::geom_text_repel(aes(x = c(long), y = c(lat), label = lab),size = 2.5)+ 
  # annotate("text",x = c(df_loc$long), y = c(df_loc$lat-0.05), label = df_loc$stna, size = 2)+
  # ggthemes::theme_map()+
  theme_tufte()+
  theme(legend.position = c(.8,.08),
        legend.box.background = element_rect(color="black", size=.5)
  )+
  ggsave(file = here::here("out" , "map.png"), width = 15, height = 15, units = "cm")

shell.exec(here::here("out" , "map.png"))

