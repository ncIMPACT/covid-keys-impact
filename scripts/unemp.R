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

unemp <- read_csv(here("data/unemployment-rate.csv")) %>%
  clean_names() %>%
  filter(str_starts(region_code, "37")) %>%
  mutate(change = x2020_october - x2019_october) %>%
  select(region_name, region_code, change) %>%
  rename(geoid = region_code)

cleaned <- unemp %>%
  right_join(nc_counties, by = c("geoid" = "GEOID")) %>%
  st_as_sf()

natural_breaks <- classIntervals(cleaned$change, 5, style = "jenks")

final <- cleaned %>%
  clean_names() %>%
  mutate(natural_breaks = cut(change, breaks = c(natural_breaks$brks[1]-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(label = glue("{upper} to {lower}"))

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
  labs(title = "Unemployment Rate Twelve Month Change",
       subtitle = "Change in unemployment rate between October 2019 and October 2020",
       caption = "<b>Source:</b> U.S. Bureau of Labor Statistics, retrieved from FRED") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "unemp-change.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  as_tibble() %>%
  replace_na(list(change = 0)) %>%
  mutate(unemp_score = (change - mean(final$change, na.rm = T)) / sd(final$change, na.rm = T)) %>%
  rename(unemp_change = change) %>%
  select(name, namelsad, geoid, unemp_change, unemp_score)

write_csv(final_dat, here("composite/unemp.csv"))
