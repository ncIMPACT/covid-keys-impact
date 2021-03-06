library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(extrafont)
library(here)
library(ggtext)
library(janitor)
library(patchwork)
library(classInt)
library(glue)
library(lubridate)

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

logo <- png::readPNG(here("logo/black-logo-long.png"), native = TRUE)

blue_pal <- c("#CFE8F3", "#73BFE2","#1696D2","#0A4C6A","#000000")

nc_counties <- counties(state = 37)
nc_counties_merge <- nc_counties %>%
  as_tibble() %>%
  select(-geometry) %>%
  clean_names() %>%
  rename(county_geoid = geoid, county_name = name, county_full_name = namelsad) %>%
  select(county_geoid, county_name, county_full_name)

poverty_rate <- get_acs(geography = "tract", variables = "B17001_002", state = 37,
                      key = api_key, summary_var = "B17001_001", geometry = T)

cleaned <- poverty_rate %>%
  mutate(acs_pov_rate = estimate / summary_est) %>%
  mutate(acs_pov_rate = ifelse(is.nan(acs_pov_rate), 0, acs_pov_rate)) %>%
  mutate(acs_pov_rate = round(acs_pov_rate, digits = 2)) %>%
  rename(acs_pov_total = estimate) %>%
  mutate(county_geoid = substr(GEOID, 1, 5)) %>%
  clean_names() %>%
  select(geoid, name, county_geoid, acs_pov_total, acs_pov_rate) %>%
  left_join(nc_counties_merge, by = "county_geoid")

natural_breaks <- classIntervals(filter(cleaned, !(is.na(acs_pov_rate)))$acs_pov_rate, 5, style = "jenks")

final <- cleaned %>%
  mutate(natural_breaks = cut(acs_pov_rate, breaks = c(-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(upper = ifelse(upper == "-0.01", "0", upper)) %>%
  mutate(label = glue("{round(as.numeric(upper) * 100, digits = 2)}% to {round(as.numeric(lower) * 100, digits = 2)}%"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- final %>%
  filter(!(is.na(acs_pov_rate))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Poverty Rate",
       subtitle = "Poverty Rate by Census Tract\nCensus Tracts with no or insufficient data have been removed",
       caption = "<b>Source:</b> Census Bureau 5 Year ACS") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "acs-poverty.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

final_dat <- final %>%
  mutate(acs_pov_score = (acs_pov_rate - mean(final$acs_pov_rate, na.rm = T)) / sd(final$acs_pov_rate, na.rm = T)) %>%
  select(geoid, acs_pov_score) %>%
  as_tibble() %>%
  select(-geometry) %>%
  right_join(final) %>%
  as_tibble() %>%
  rename(tract_name = name, tract_geoid = geoid) %>%
  select(tract_geoid, tract_name, county_geoid, county_name, county_full_name, acs_pov_total, acs_pov_rate, acs_pov_score)

write_csv(final_dat, here("composite/acs-pov.csv"))
