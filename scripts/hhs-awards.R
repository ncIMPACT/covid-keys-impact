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

nc_places <- places(state = 37) %>%
  mutate(NAME = str_replace(NAME, "-", " "))

nc_counties <- counties(state = 37)

nc_pop <- get_acs(geography = "county", variables = "B01003_001",
                  state = 37, key = api_key) %>%
  clean_names()

hhs_awards <- read_csv(here("data/covid-19-award-details.csv"))

cleaned <- hhs_awards %>%
  clean_names() %>%
  mutate(city = str_to_title(city)) %>%
  mutate(fix_city = case_when(
    city == "Skyland" ~ "Asheville",
    city == "Arden" ~ "Asheville",
    city == "Candler" ~ "Asheville",
    city == "Leicester" ~ "Asheville",
    city == "Linville" ~ "Newland",
    city == "Nebo" ~ "Marion",
    city == "Prospect Hill" ~ "Yanceyville",
    city == "Saint Pauls" ~ "Lumberton",
    city == "Supply" ~ "Shallotte",
    city == "Swanquarter" ~ "Swan Quarter",
    city == "Snow Camp" ~ "Burlington",
    city == "Mcadenville" ~ "McAdenville",
    city == "Mcleansville" ~ "McLeansville",
    city == "Collettsville" ~ "Lenoir",
    city == "Rsch Triangle Pk" ~ "Cary",
    city == "Snowhill" ~ "Snow Hill",
    city == "Winston-Salem" ~ "Winston Salem"
  )) %>%
  mutate(city = ifelse(is.na(fix_city), city, fix_city)) %>%
  select(-fix_city) %>%
  mutate(city = ifelse(city == "Bowman Gray School Of", "Winston Salem", city)) %>%
  mutate(across(.cols = starts_with("award_amount"), .fns = ~str_remove_all(.x, "\\$"))) %>%
  mutate(across(.cols = starts_with("award_amount"), .fns = ~str_remove_all(.x, "\\,"))) %>%
  mutate(across(.cols = starts_with("award_amount"), .fns = ~str_replace(.x, "\\(", "-"))) %>%
  mutate(across(.cols = starts_with("award_amount"), .fns = ~str_remove_all(.x, "\\)"))) %>%
  mutate(across(.cols = starts_with("award_amount"), .fns = as.numeric)) %>%
  mutate(total_paid = award_amount) %>%
  group_by(city, state) %>%
  summarise(total_paid = sum(total_paid, na.rm = T)) %>%
  ungroup() %>%
  filter(!(is.na(city))) %>%
  left_join(nc_places, by = c("city" = "NAME")) %>%
  st_as_sf()

final <- nc_counties %>%
  st_join(select(st_centroid(cleaned), 1:3)) %>%
  clean_names() %>%
  group_by(name, namelsad, geoid) %>%
  summarise(hhs_uninsured = sum(total_paid)) %>%
  ungroup() %>%
  left_join(select(nc_pop, geoid, estimate)) %>%
  mutate(per_capita = hhs_uninsured / estimate)

natural_breaks <- classIntervals(filter(final, !(is.na(per_capita)))$per_capita, 5, style = "jenks")

final <- final %>%
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
  filter(!(is.na(per_capita))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "HHS COVID-19 Awards\nAwards Per Capita",
       subtitle = "Awards made by HHS using emergency supplemental\nappropriation funding provided in the CARES Act",
       caption = "<b>Source:</b> HHS TAGGS") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "hhs-awards.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  replace_na(list(per_capita = 0, hhs_uninsured = 0)) %>%
  as_tibble() %>%
  mutate(hhs_awards_score = (per_capita - mean(final$per_capita, na.rm = T)) / sd(final$per_capita, na.rm = T)) %>%
  rename(hhs_awards_per_capita = per_capita, hhs_awards_total = hhs_uninsured) %>%
  select(name, namelsad, geoid, hhs_awards_per_capita, hhs_awards_total, hhs_awards_score)

write_csv(final_dat, here("composite/hhs_awards.csv"))
