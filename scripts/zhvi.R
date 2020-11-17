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

zhvi <- read_csv(here("data/zhvi-three-bedroom.csv")) %>%
  filter(StateCodeFIPS == 37) %>%
  pivot_longer(cols = contains("/"), names_to = "date", values_to = "zhvi") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

cleaned <- zhvi %>%
  clean_names() %>%
  filter(date == "2020-09-30" | date =="2019-09-30") %>%
  pivot_wider(names_from = date, values_from = zhvi) %>%
  mutate(change = (`2020-09-30` - `2019-09-30`) / `2019-09-30`) %>%
  select(region_name, change) %>%
  right_join(nc_counties, by = c("region_name" = "NAMELSAD")) %>%
  st_as_sf()

natural_breaks <- classIntervals(cleaned$change, 5, style = "jenks")

final <- cleaned %>%
  clean_names() %>%
  mutate(natural_breaks = cut(change, breaks = c(natural_breaks$brks[1]-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(label = glue("{round(as.numeric(upper), digits = 2) * 100}% to {round(as.numeric(lower), digits = 2) * 100}%"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- final %>%
  filter(!(is.na(change))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Zillow Home Value Index Twelve Month Percent Change\nThree Bedroom Homes",
       subtitle = "Zillow Home Value Index (ZHVI) is a smoothed, seasonally adjusted measure of the\ntypical home value and market changes across a given region and housing type",
       caption = "<b>Source:</b> Zillow") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "zhvi-change.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  clean_names() %>%
  as_tibble() %>%
  filter(change > median(final$change, na.rm = T)) %>%
  select(geoid) %>%
  mutate(zhvi_score = 1) %>%
  right_join(final) %>%
  as_tibble() %>%
  rename(zhvi_pct_change = change, namelsad = region_name) %>%
  select(name, namelsad, geoid, zhvi_pct_change, zhvi_score) %>%
  mutate(zhvi_score = if_else(is.na(zhvi_score), 0, 1))

write_csv(final_dat, here("composite/zhvi.csv"))
