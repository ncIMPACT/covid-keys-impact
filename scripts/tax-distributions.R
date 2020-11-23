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

distributions <- read_csv("https://raw.githubusercontent.com/jasonajones73/ncdor/master/data/article_overview.csv")

cleaned <- distributions %>%
  rename(county = county_name) %>%
  filter(county != "Foreign") %>%
  filter(year == 2019 | year == 2020) %>%
  filter(month(date) <= month(max(date))) %>%
  group_by(county, year) %>%
  summarise(distributable_proceeds = sum(distributable_proceeds, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = distributable_proceeds) %>%
  mutate(change = (`2020` - `2019`) / `2019`) %>%
  select(county, change) %>%
  right_join(nc_counties, by = c("county" = "NAME")) %>%
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
  scale_fill_manual(values = rev(blue_pal), labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Total Sales Tax Distribution Change",
       subtitle = "Percent change in total distributable proceeds during the same period (Jan to Sep) for 2019 and 2020",
       caption = "<b>Source:</b> NC Department of Revenue") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "tax-distributions.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  as_tibble() %>%
  replace_na(list(change = 0)) %>%
  mutate(tax_distribution_score = ((change - mean(final$change, na.rm = T)) / sd(final$change, na.rm = T)) * -1) %>%
  rename(tax_distribution_change = change, name = county) %>%
  select(name, namelsad, geoid, tax_distribution_change, tax_distribution_score)

write_csv(final_dat, here("composite/tax-distributions.csv"))
