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

nc_pop <- get_acs(geography = "county", variables = "B01003_001",
                  state = 37, key = api_key) %>%
  clean_names()

ui_claims <- read_csv(here("data/continued-ui-claims.csv"))

cleaned <- ui_claims %>%
  clean_names() %>%
  rename(county = area) %>%
  filter(month_year == "Sep-20") %>%
  filter(area_type_table_sort == "County") %>%
  filter(county != "Unknown") %>%
  filter(attribute_category == "Claimants") %>%
  select(county, value) %>%
  rename(total_claimants = value) %>%
  right_join(nc_counties, by = c("county" = "NAME")) %>%
  clean_names() %>%
  right_join(select(nc_pop, geoid, estimate)) %>%
  mutate(ui_claims_per = total_claimants / (estimate / 1000)) %>%
  st_as_sf()

natural_breaks <- classIntervals(cleaned$ui_claims_per, 5, style = "jenks")

final <- cleaned %>%
  clean_names() %>%
  mutate(natural_breaks = cut(ui_claims_per, breaks = c(0, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
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
  filter(!(is.na(ui_claims_per))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Unemployment Insurance Claimants\nPer 1,000 Residents",
       subtitle = "Total unemployment insurance claimants for Sep 2020 per 1,000 county residents",
       caption = "<b>Source:</b> NC Department of Commerce") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "ui-claims.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  as_tibble() %>%
  replace_na(list(ui_claims_per = 0)) %>%
  mutate(ui_claims_score = (ui_claims_per - mean(final$ui_claims_per, na.rm = T)) / sd(final$ui_claims_per, na.rm = T)) %>%
  rename(name = county, ui_claimants = total_claimants) %>%
  select(name, namelsad, geoid, ui_claimants, ui_claims_per, ui_claims_score)

write_csv(final_dat, here("composite/ui-claims.csv"))
