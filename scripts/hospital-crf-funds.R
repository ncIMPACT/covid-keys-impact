library(tidyverse)
library(tigris)
library(sf)
library(extrafont)
library(here)
library(ggtext)

source(here("api_key/census_api_key.R"))

map_theme <- theme(text = element_text(family = "Arial"),
                   plot.background = element_rect(fill = "transparent"),
                   panel.background = element_rect(fill = "transparent"),
                   panel.grid = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 24, face = "bold"),
                   plot.subtitle = element_text(size = 14),
                   axis.title = element_text(size = 12, face = "italic"),
                   axis.text = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   legend.justification = "left",
                   plot.caption = element_markdown(size = 11, hjust = 0))

blue_pal <- c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB","#1696D2","#12719E","#0A4C6A","#062635")

nc_counties <- counties(state = 37)

nc_hospitals <- read_csv(here("data/nc_crf_hospital_allocations.csv")) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(nc_counties))

overlay <- nc_counties %>%
  st_join(nc_hospitals)

overlay %>%
  group_by(NAME) %>%
  summarise(amount = sum(amount)) %>%
  ungroup() %>%
  ggplot() +
  geom_sf(aes(fill = amount), color = "white") +
  labs(title = "North Carolina Coronavirus Relief Fund\nHospital Distributions",
       subtitle = "State distributions of relief funds as assigned to containing county",
       caption = "<b>Source:</b> NC Pandemic Recovery Office") +
  scale_fill_gradientn(name = NULL, labels = scales::dollar_format(), na.value = "#F4E8DD",
                       guide = guide_colorbar(barwidth = unit(4, "inches")),
                       colors = blue_pal) +
  map_theme


nc_hospitals %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "#F8F8F8", fill = "#F4E8DD") +
  geom_sf(aes(color = amount, fill = amount, size = amount), shape = 21, alpha = 0.6) +
  labs(title = "North Carolina Coronavirus Relief Fund\nHospital Distributions",
       subtitle = "State distributions of relief funds to hospitals",
       caption = "<b>Source:</b> NC Pandemic Recovery Office") +
  scale_color_gradientn(name = NULL, labels = scales::dollar_format(), na.value = "#F4E8DD",
                        guide = guide_colorbar(barwidth = unit(4, "inches")),
                        colors = blue_pal) +
  scale_fill_gradientn(guide = NULL, colors = blue_pal) +
  scale_size(guide = NULL, range = c(0, 10)) +
  map_theme


