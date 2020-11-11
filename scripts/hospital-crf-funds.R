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
                  state = 37, key = api_key) %>%
  clean_names()

nc_hospitals <- read_csv(here("data/nc_crf_hospital_allocations.csv")) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(nc_counties))

overlay <- nc_counties %>%
  st_join(nc_hospitals) %>%
  clean_names() %>%
  group_by(name, namelsad, geoid) %>%
  summarise(amount = sum(amount, na.rm = T)) %>%
  left_join(select(nc_pop, geoid, estimate)) %>%
  mutate(per_capita = amount / estimate) %>%
  ungroup()
  
natural_breaks <- classIntervals(filter(overlay, !(is.na(per_capita)))$per_capita, 5, style = "jenks")  
  
final <- overlay %>%
  mutate(natural_breaks = cut(per_capita, breaks = c(0, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(label = glue("${upper} - ${lower}"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels
  
p1 <- final %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "North Carolina Coronavirus Relief Fund\nHospital Distributions Per Capita",
       subtitle = "State distributions of relief funds as assigned to containing county",
       caption = "<b>Source:</b> NC Pandemic Recovery Office") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "hospitals-crf.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

final_dat <- final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  filter(per_capita > median(final$per_capita, na.rm = T)) %>%
  select(geoid) %>%
  mutate(hospital_crf_score = 1) %>%
  right_join(final) %>%
  as_tibble() %>%
  select(name, namelsad, geoid, hospital_crf_score) %>%
  mutate(hospital_crf_score = if_else(is.na(hospital_crf_score), 0, 1))

write_csv(final_dat, here("composite/hospital_crf.csv"))
