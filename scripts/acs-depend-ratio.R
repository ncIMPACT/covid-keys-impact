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

# Dependency Ratio: 1)Population under 18 - B09001_001, 2) 65 Years and Over - B08101_008, 3) 18 to 64 - B16004_024
pop_18_un <- get_acs(geography = "tract", variables = "B09001_001",
                     state = 37, key = api_key)

pop_18_un <- pop_18_un %>%
  rename(pop_18_un = estimate) %>%
  select(GEOID, pop_18_un)

over_65 <- get_acs(geography = "tract", variables = "B08101_008",
                   state = 37, key = api_key)

over_65 <- over_65 %>%
  rename(over_65 = estimate) %>%
  select(GEOID, over_65)

mid <- get_acs(geography = "tract", variables = "B16004_024",
               state = 37, key = api_key, geometry = T)

depend_ratio <- mid %>%
  rename(mid = estimate) %>%
  left_join(pop_18_un) %>%
  left_join(over_65) %>%
  mutate(estimate = round((pop_18_un + over_65) / mid, digits = 2)) %>%
  mutate(estimate = ifelse(is.nan(estimate), 0, estimate)) %>%
  select(-mid, -pop_18_un, -over_65)

cleaned <- depend_ratio %>%
  mutate(depend_ratio = estimate) %>%
  mutate(county_geoid = substr(GEOID, 1, 5)) %>%
  clean_names() %>%
  select(geoid, name, county_geoid, depend_ratio) %>%
  left_join(nc_counties_merge, by = "county_geoid")

natural_breaks <- classIntervals(filter(cleaned, !(is.na(depend_ratio)))$depend_ratio, 5, style = "jenks")

final <- cleaned %>%
  mutate(natural_breaks = cut(depend_ratio, breaks = c(-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(upper = ifelse(upper == "-0.01", "0", upper)) %>%
  mutate(label = glue("{upper} to {lower}"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- final %>%
  filter(!(is.na(depend_ratio))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Dependency Ratio by Census Tract",
       subtitle = "Dependency Ratio is an age-population ratio of those typically not in the labor force\n(under 18 & over 65) and those typically in the labor force (population 18 to 64)",
       caption = "<b>Source:</b> Census Bureau 5 Year ACS") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "acs-depend.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  mutate(depend_score = (depend_ratio - mean(final$depend_ratio, na.rm = T)) / sd(final$depend_ratio, na.rm = T)) %>%
  select(geoid, depend_score) %>%
  as_tibble() %>%
  select(-geometry) %>%
  right_join(final) %>%
  as_tibble() %>%
  rename(tract_name = name, tract_geoid = geoid) %>%
  select(tract_geoid, tract_name, county_geoid, county_name, county_full_name, depend_ratio, depend_score)

write_csv(final_dat, here("composite/acs-depend.csv"))
