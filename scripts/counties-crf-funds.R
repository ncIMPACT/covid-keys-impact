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

nc_pop <- get_acs(geography = "county", variables = "B01003_001",
                  state = 37, key = api_key)

crf_dat <- read_csv(here("data/nc-crf-county-allocations.csv"))

merged <- crf_dat %>%
  left_join(nc_counties, by = c("county" = "NAME")) %>%
  left_join(nc_pop) %>%
  clean_names() %>%
  mutate(per_capita = total_allocation / estimate) %>%
  st_as_sf()

natural_breaks <- classIntervals(filter(merged, !(is.na(per_capita)))$per_capita, 5, style = "jenks")

merged <- merged %>%
  mutate(natural_breaks = cut(per_capita, breaks = c(natural_breaks$brks[1]-.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(label = glue("${upper} - ${lower}"))

merged %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- merged %>%
  ggplot() +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  guides(fill = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "North Carolina Coronavirus Relief Funds\nCounty Allocations Per Capita",
       subtitle = "Direct federal or indirect state distributions of relief funds per capita",
       caption = "<b>Source:</b> Census Bureau 5 Year ACS & NC Pandemic Recovery Office") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "counties-crf.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

final <- merged %>%
  as_tibble() %>%
  mutate(county_crf_score = (per_capita - mean(merged$per_capita, na.rm = T)) / sd(merged$per_capita, na.rm = T)) %>%
  rename(county_crf_per_capita = per_capita, county_crf_total = total_allocation) %>%
  select(name, namelsad, geoid, county_crf_per_capita, county_crf_total, county_crf_score)

write_csv(final, here("composite/county_crf.csv"))
