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
library(zipcodeR)

options(scipen = 999)

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

zips <- search_state("NC") %>%
  select(zipcode, major_city, county) %>%
  rename(Zip = zipcode)

nc_pop <- get_acs(geography = "county", variables = "B01003_001",
                  state = 37, key = api_key) %>%
  clean_names()

nc_ppp <- read_csv(here("data/nc-ppp.csv"), col_types = cols(Zip = col_character())) %>%
  left_join(zips) %>%
  rename(NAMELSAD = county)

cleaned <- nc_ppp %>%
  clean_names() %>%
  mutate(total_paid = loan_amount) %>%
  group_by(namelsad, state) %>%
  summarise(hhs_uninsured = sum(total_paid, na.rm = T)) %>%
  ungroup() %>%
  left_join(nc_counties, by = c("namelsad" = "NAMELSAD")) %>%
  st_as_sf()

final <- cleaned %>%
  clean_names() %>%
  select(name, namelsad, geoid, hhs_uninsured) %>%
  left_join(select(nc_pop, geoid, estimate)) %>%
  mutate(per_capita = hhs_uninsured / estimate)

natural_breaks <- classIntervals(filter(final, !(is.na(per_capita)))$per_capita, 5, style = "jenks")

final <- final %>%
  mutate(natural_breaks = cut(per_capita, breaks = c(0, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(label = ifelse(per_capita > 658, glue("${upper} - $1,036"), glue("${upper} - ${lower}")))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- final %>%
  filter(!(is.na(per_capita))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Paycheck Protection Program\nLoan Amounts Per Capita",
       subtitle = "PPP per capita loan amounts by county in North Carolina",
       caption = "<b>Source:</b> SBA") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "nc-ppp.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  as_tibble() %>%
  filter(per_capita > median(final$per_capita, na.rm = T)) %>%
  select(geoid) %>%
  mutate(ppp_score = 1) %>%
  right_join(final) %>%
  as_tibble() %>%
  rename(ppp_per_capita = per_capita, ppp_total = hhs_uninsured) %>%
  select(name, namelsad, geoid, ppp_per_capita, ppp_total, ppp_score) %>%
  mutate(ppp_score = if_else(is.na(ppp_score), 0, 1))

write_csv(final_dat, here("composite/ppp.csv"))
