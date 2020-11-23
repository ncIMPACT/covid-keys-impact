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

system_datetime <- as.POSIXct(Sys.time())
adjusted_datetime <- as.Date(format(system_datetime, tz = "EST",usetz = TRUE))

confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>%
  filter(Province_State == "North Carolina") %>%
  filter(Admin2 != "Unassigned" & Admin2 != "Out of NC") %>%
  pivot_longer(cols = 12:(as.integer(adjusted_datetime - as.Date("2020-01-22")) + 11), names_to = "date", values_to = "cases") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  group_by(Admin2) %>%
  mutate(daily_cases = lag(cases, n = 1)) %>%
  ungroup() %>%
  mutate(daily_cases = ifelse(is.na(daily_cases), 0, daily_cases)) %>%
  mutate(daily_cases = cases - daily_cases)

deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>%
  filter(Province_State == "North Carolina") %>%
  filter(Admin2 != "Unassigned" & Admin2 != "Out of NC") %>%
  pivot_longer(cols = 13:(as.integer(adjusted_datetime - as.Date("2020-01-22")) + 12), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
  group_by(Admin2) %>%
  mutate(daily_deaths = lag(deaths, n = 1)) %>%
  ungroup() %>%
  mutate(daily_deaths = ifelse(is.na(daily_deaths), 0, daily_deaths)) %>%
  mutate(daily_deaths = deaths - daily_deaths)

confirmed_map <- confirmed %>%
  group_by(Admin2) %>%
  top_n(1, date) %>%
  ungroup() %>%
  filter(Lat > 1) %>%
  left_join(select(deaths, Admin2, Population)) %>%
  distinct(Admin2, .keep_all = TRUE) %>%
  mutate(cases_per = as.integer(cases / (Population / 10000)))

cleaned <- confirmed_map %>%
  rename(county = Admin2) %>%
  select(county, cases_per) %>%
  right_join(nc_counties, by = c("county" = "NAME")) %>%
  st_as_sf()

natural_breaks <- classIntervals(cleaned$cases_per, 5, style = "jenks")

final <- cleaned %>%
  clean_names() %>%
  mutate(natural_breaks = cut(cases_per, breaks = c(0, natural_breaks$brks[2:length(natural_breaks$brks)]))) %>%
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
  filter(!(is.na(cases_per))) %>%
  ggplot() +
  geom_sf(data = nc_counties, color = "white", fill = "#F4E8DD") +
  geom_sf(aes(fill = natural_breaks), color = "white") +
  scale_fill_manual(values = blue_pal, labels = legend_labels,
                    guide = guide_legend(title = NULL, nrow = 1)) +
  labs(title = "COVID-19 Cases Per 10,000 Residents",
       subtitle = "Total COVID-19 confirmed cases per 10,000 county residents",
       caption = "<b>Source:</b> JHU Center for Systems Science and Engineering (CSSE)") +
  map_theme

p1 + inset_element(logo, left = 0.7, bottom = 0, right = 1, top = 0.09, align_to = 'full')

ggsave(filename = "covid-cases.png", device = "png",
       path = here("plots/"), dpi = "retina", width = 16, height = 9)         

final_dat <- final %>%
  as_tibble() %>%
  replace_na(list(cases_per = 0)) %>%
  mutate(cases_per_score = (cases_per - mean(final$cases_per, na.rm = T)) / sd(final$cases_per, na.rm = T)) %>%
  rename(name = county) %>%
  select(name, namelsad, geoid, cases_per, cases_per_score)

write_csv(final_dat, here("composite/cases-per.csv"))
