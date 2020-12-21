library(tidyverse)
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

map_theme <- theme(text = element_text(family = "Arial"),
                   plot.background = element_rect(fill = "transparent"),
                   panel.background = element_rect(fill = "transparent"),
                   panel.grid = element_blank(),
                   axis.ticks = element_blank(),
                   plot.title = element_text(size = 24, face = "bold"),
                   plot.subtitle = element_markdown(size = 14),
                   axis.title = element_text(size = 12, face = "italic"),
                   axis.text = element_blank(),
                   legend.text = element_text(size = 12),
                   legend.position = "top",
                   legend.justification = "left",
                   plot.caption = element_markdown(size = 11, hjust = 0))

logo <- png::readPNG(here("logo/black-logo-long.png"), native = TRUE)

blue_pal <- c("#ca5800", "#fdbf11", "#fdd870", "#73bfe2","#1696D2", "#0A4C6A")

nc_counties <- counties(state = 37) %>%
  clean_names() %>%
  select(-c(name, namelsad))

nc_tracts <- tracts(state = 37) %>%
  clean_names() %>%
  rename(tract_geoid = geoid) %>%
  select(tract_geoid)

layer_one <- read_csv("composite/layer-one-composite.csv", col_types = cols(geoid = col_character()))

layer_two <- read_csv("composite/layer-two-composite.csv", col_types = cols(geoid = col_character()))

layer_three <- read_csv("composite/layer-three-composite.csv", col_types = cols(county_geoid = col_character(),
                                                                                tract_geoid = col_character()))

################################################################################

combined <- layer_three %>%
  select(1:5, ends_with("score"))

z_score_total = rowSums(select(combined, ends_with("score")))

final <- combined %>%
  cbind(z_score_total) %>%
  left_join(nc_tracts) %>%
  st_as_sf()

natural_breaks <- classIntervals(filter(final, !(is.na(z_score_total)))$z_score_total, 6, style = "jenks")

final <- final %>%
  mutate(natural_breaks = cut(z_score_total, breaks = c(natural_breaks$brks[1]-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(upper = ifelse(upper == "-0.01", "0", upper)) %>%
  mutate(label = glue("{round(as.numeric(upper), digits = 2)} to {round(as.numeric(lower), digits = 2)}"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p1 <- final %>%
  filter(!(is.na(z_score_total))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = rev(blue_pal), labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Community Resilience Composite",
       subtitle = "Total z-score by Census Tract for 7 community resilience composite metrics. The z-score is a measure of distance from the average for all census tracts.<br>Metric z-scores have all been standardized so that a <b style='color: #ca5800;'>high z-score</b> is not preferred. This composite includes Census data capturing unemployment,<br>poverty, school age children, broadband access, health insurance coverage, and racial diversity.",
       caption = "<b>Source:</b> UNC School of Government, ncIMPACT Initiative") +
  map_theme

p1 <- p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

p1

ggsave(filename = "prior-position-composite.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

################################################################################

combined <- layer_one %>%
  select(1:3, ends_with("score"))

z_score_total = rowSums(select(combined, ends_with("score")))

final <- combined %>%
  cbind(z_score_total) %>%
  left_join(nc_counties, by = "geoid") %>%
  st_as_sf()

natural_breaks <- classIntervals(filter(final, !(is.na(z_score_total)))$z_score_total, 6, style = "jenks")

final <- final %>%
  mutate(natural_breaks = cut(z_score_total, breaks = c(natural_breaks$brks[1]-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(upper = ifelse(upper == "-0.01", "0", upper)) %>%
  mutate(label = glue("{round(as.numeric(upper), digits = 2)} to {round(as.numeric(lower), digits = 2)}"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p2 <- final %>%
  filter(!(is.na(z_score_total))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = rev(blue_pal), labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Emergency Financial Support Composite",
       subtitle = "Total z-score by county for all 7 financial support composite metrics. The z-score is a measure of distance from the average for all counties.<br>Metric z-scores have all been standardized so that a <b style='color: #ca5800;'>high z-score</b> is not preferred. This composite includes public data capturing the NC CRF,<br>HHS Uninsured Relief Fund, HHS RHC Testing Fund, HHS Provider Relief Fund, HHS COVID-19 Awards, and SBA PPP Loans.",
       caption = "<b>Source:</b> UNC School of Government, ncIMPACT Initiative") +
  map_theme

p2 <- p2 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

p2

ggsave(filename = "fin-support-composite.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

################################################################################

combined <- layer_two %>%
  select(1:3, ends_with("score"))

z_score_total = rowSums(select(combined, ends_with("score")))

final <- combined %>%
  cbind(z_score_total) %>%
  left_join(nc_counties, by = "geoid") %>%
  st_as_sf()

natural_breaks <- classIntervals(filter(final, !(is.na(z_score_total)))$z_score_total, 6, style = "jenks")

final <- final %>%
  mutate(natural_breaks = cut(z_score_total, breaks = c(natural_breaks$brks[1]-0.01, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
  mutate(upper = str_extract(natural_breaks, "[^(]*(?=,)")) %>%
  mutate(lower = str_extract(natural_breaks, "(?<=,)[^\\]]*")) %>%
  mutate(upper = ifelse(upper == "-0.01", "0", upper)) %>%
  mutate(label = glue("{round(as.numeric(upper), digits = 2)} to {round(as.numeric(lower), digits = 2)}"))

final %>%
  filter(!(is.na(natural_breaks))) %>%
  as_tibble() %>%
  distinct(natural_breaks, .keep_all = T) %>%
  select(natural_breaks, label) %>%
  arrange(natural_breaks) %>%
  pull(label) -> legend_labels

p3 <- final %>%
  filter(!(is.na(z_score_total))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "#151515", size = 0.01) +
  scale_fill_manual(values = rev(blue_pal), labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "Real-Time Fluctuations Composite",
       subtitle = "Total z-score by county for all 6 real-time fluctuation composite metrics. The z-score is a measure of distance from the average for all counties.<br>Metric z-scores have all been standardized so that a <b style='color: #ca5800;'>high z-score</b> is not preferred. This composite includes public data capturing unemployment,<br>taxable sales, sales tax distributions, COVID-19 cases, home values, and unemployment claims.",
       caption = "<b>Source:</b> UNC School of Government, ncIMPACT Initiative") +
  map_theme

p3 <- p3 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

p3

ggsave(filename = "real-time-composite.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)

################################################################################

p2 / p3 / p1
