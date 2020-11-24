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

health_ins <- get_acs(geography = "tract", table = "B27001", state = 37,
                  key = api_key, geometry = T, summary_var = "B27001_001")

vars <- load_variables(2018, "acs5")

cleaned <- health_ins %>%
  as_tibble() %>%
  select(-moe) %>%
  left_join(vars, by = c("variable" = "name")) %>%
  filter(str_detect(label, "No health insurance coverage")) %>%
  select(-c(label, concept, summary_moe)) %>%
  pivot_wider(names_from = variable, values_from = estimate)

cleaned <- cleaned %>%
  select(starts_with("B27001")) %>%
  rowSums() %>%
  cbind(cleaned) %>%
  rename(health_ins_total = ".") %>%
  select(1:5) %>%
  mutate(health_ins_pct = health_ins_total / summary_est) %>%
  select(GEOID, NAME, health_ins_total, health_ins_pct, geometry) %>%
  st_as_sf() %>%
  mutate(health_ins_pct = ifelse(is.nan(health_ins_pct), 0, health_ins_pct)) %>%
  mutate(health_ins_pct = round(health_ins_pct, digits = 2)) %>%
  mutate(county_geoid = substr(GEOID, 1, 5)) %>%
  clean_names() %>%
  select(geoid, name, county_geoid, health_ins_total, health_ins_pct) %>%
  left_join(nc_counties_merge, by = "county_geoid")

natural_breaks <- classIntervals(filter(cleaned, !(is.na(health_ins_pct)))$health_ins_pct, 5, style = "jenks")

final <- cleaned %>%
  mutate(natural_breaks = cut(health_ins_pct, breaks = c(-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
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
  filter(!(is.na(health_ins_pct))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Health Insurance Coverage",
       subtitle = "Percentage of population with no health insurance coverage\nCensus Tracts with no or insufficient data have been removed",
       caption = "<b>Source:</b> Census Bureau 5 Year ACS") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "acs-health.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

final_dat <- final %>%
  mutate(acs_health_score = (health_ins_pct - mean(final$health_ins_pct, na.rm = T)) / sd(final$health_ins_pct, na.rm = T)) %>%
  select(geoid, acs_health_score) %>%
  as_tibble() %>%
  select(-geometry) %>%
  right_join(final) %>%
  as_tibble() %>%
  rename(tract_name = name, tract_geoid = geoid) %>%
  select(tract_geoid, tract_name, county_geoid, county_name, county_full_name, health_ins_total, health_ins_pct, acs_health_score)

write_csv(final_dat, here("composite/acs-health.csv"))
