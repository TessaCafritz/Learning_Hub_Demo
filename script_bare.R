library(tidyverse)
library(terra)
library(sf)
library(here)
library(DT)
library(rnaturalearth)

suitability_data <- read_csv('_data/shark_suitability.csv') %>%
  mutate(suitability = round(suitability, 3))

ocean_mask <- read_csv('_data/ocean_mask.csv')

DT::datatable(suitability_data)

silky_data <- suitability_data %>%
  filter(species == 'silky shark')

hammerhead_data <- suitability_data %>%
  filter(species == 'scalloped hammerhead')

average_data <- suitability_data %>%
  group_by(long, lat) %>%
  summarize(suitability = mean(suitability), .groups = 'drop') %>%
  mutate(species = 'average')

average_raster <- rast(average_data)

africa_sf <- rnaturalearth::ne_countries(continent = 'Africa', returnclass = 'sf')
sf_use_s2(FALSE) 
africa_sf <- africa_sf %>%
  st_crop(st_bbox(average_raster))

combined_data <- bind_rows(silky_data, hammerhead_data, average_data)

tripanel_plot <- ggplot() +
  geom_raster(data = combined_data, aes(x = long, y = lat, fill = suitability)) +
  geom_sf(data = africa_sf) +
  scale_fill_viridis_c() +
  facet_wrap(~ species) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = 'Longitude', y = 'Latitude',
       title = 'Habitat suitability, Mozambique')

ggsave('img/shark_suitability.png', height = 3, width = 8, dpi = 300)
