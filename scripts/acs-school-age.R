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

school <- get_acs(geography = "tract", table = "B14001", state = 37,
                        key = api_key, geometry = T)

cleaned <- school %>%
  as_tibble() %>%
  select(-moe) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(school_total = B14001_003 + B14001_004 + B14001_005 + B14001_006 + B14001_007) %>%
  mutate(school_age = school_total / B14001_001) %>%
  select(GEOID, NAME, school_total, school_age, geometry) %>%
  st_as_sf() %>%
  mutate(school_age = ifelse(is.nan(school_age), 0, school_age)) %>%
  mutate(school_age = round(school_age, digits = 2)) %>%
  mutate(county_geoid = substr(GEOID, 1, 5)) %>%
  clean_names() %>%
  select(geoid, name, county_geoid, school_total, school_age) %>%
  left_join(nc_counties_merge, by = "county_geoid")

natural_breaks <- classIntervals(filter(cleaned, !(is.na(school_age)))$school_age, 5, style = "jenks")

final <- cleaned %>%
  mutate(natural_breaks = cut(school_age, breaks = c(-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
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
  filter(!(is.na(school_age))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "School Age Children",
       subtitle = "Percentage of population, 3 years and over, currently enrolled in School\nCensus Tracts with no or insufficient data have been removed",
       caption = "<b>Source:</b> Census Bureau 5 Year ACS<br><b>Note: </b>School is defined as Nursery or Preschool through Grade 12") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "acs-school-age.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

final_dat <- final %>%
  mutate(acs_pov_score = (school_age - mean(final$school_age, na.rm = T)) / sd(final$school_age, na.rm = T)) %>%
  select(geoid, acs_pov_score) %>%
  as_tibble() %>%
  select(-geometry) %>%
  right_join(final) %>%
  as_tibble() %>%
  rename(tract_name = name, tract_geoid = geoid) %>%
  select(tract_geoid, tract_name, county_geoid, county_name, county_full_name, school_total, school_age, acs_pov_score)

write_csv(final_dat, here("composite/acs-school-age.csv"))
