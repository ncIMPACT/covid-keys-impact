library(tidyverse)
library(tidycensus)
library(ggtext)
library(extrafont)
library(classInt)
library(sf)
library(here)
library(patchwork)
library(tigris)

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
  mutate(estimate = (pop_18_un + over_65) / mid) %>%
  select(-mid, -pop_18_un, -over_65)

natural_breaks <- classIntervals(filter(depend_ratio, !(is.na(estimate)))$estimate, 5, style = "jenks")

p1 <- depend_ratio %>%
  mutate(natural_breaks = cut(estimate, breaks = c(natural_breaks$brks[1]-.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  filter(!(is.nan(estimate))) %>%
  ggplot() +
  geom_sf(aes(fill = natural_breaks)) +
  guides(fill = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Dependency Ratio by Census Tract",
       subtitle = "Dependency Ratio is an age-population ratio of those typically not in the labor force\n(under 18 & over 65) and those typically in the labor force (population 18 to 64)",
       caption = "<b>Source:</b> Census Bureau, 5 Year ACS") +
  scale_fill_manual(values = blue_pal, na.value = "#F4E8DD",
                    labels = c("0 - 0.209", "0.209 - 0.344", "0.344 - 0.436", "0.436 - 0.544", "0.544 - 1")) +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "depend.png", device = "png",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 16, height = 9)

p2 <- depend_ratio %>%
  mutate(natural_breaks = cut(estimate, breaks = c(natural_breaks$brks[1]-.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  filter(!(is.nan(estimate))) %>%
  filter(estimate > median(depend_ratio$estimate, na.rm = T)) %>%
  ggplot() +
  geom_sf(data = nc_counties, fill = "#F4E8DD", color = "white") +
  geom_sf(aes(fill = natural_breaks)) +
  guides(fill = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Dependency Ratio by Census Tract\nAbove State Median",
       subtitle = "Dependency Ratio is an age-population ratio of those typically not in the labor force\n(under 18 & over 65) and those typically in the labor force (population 18 to 64)",
       caption = "<b>Source:</b> Census Bureau, 5 Year ACS") +
  scale_fill_manual(values = blue_pal[3:5], na.value = "#F4E8DD",
                    labels = c("0.344 - 0.436", "0.436 - 0.544", "0.544 - 1")) +
  map_theme

p2 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "depend_above.png", device = "png",
       path = "C:/Users/jsnjns/Desktop/", dpi = "retina", width = 16, height = 9)
