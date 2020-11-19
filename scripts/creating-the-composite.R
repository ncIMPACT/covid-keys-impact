library(tidyverse)
library(here)

county_crf <- read_csv(here("composite/county_crf.csv"), col_types = cols("geoid" = col_character()))
hospital_crf <- read_csv(here("composite/hospital_crf.csv"), col_types = cols("geoid" = col_character()))
hhs_uninsured <- read_csv(here("composite/hhs_uninsured_relief.csv"), col_types = cols("geoid" = col_character()))
hhs_rhc <- read_csv(here("composite/rhc_testing.csv"), col_types = cols("geoid" = col_character()))
hhs_prov <- read_csv(here("composite/hhs_provider_relief.csv"), col_types = cols("geoid" = col_character()))
hhs_awards <- read_csv(here("composite/hhs_awards.csv"), col_types = cols("geoid" = col_character()))
nc_ppp <- read_csv(here("composite/ppp.csv"), col_types = cols("geoid" = col_character()))

layer_one <- county_crf %>%
  left_join(select(hospital_crf, 3:6)) %>%
  left_join(select(hhs_uninsured, 3:6)) %>%
  left_join(select(hhs_rhc, 3:6)) %>%
  left_join(select(hhs_prov, 3:6)) %>%
  left_join(select(hhs_awards, 3:6)) %>%
  left_join(select(nc_ppp, 3:6)) %>%
  mutate(across(.fns = ~replace_na(.x, 0))) %>%
  select(name, namelsad, geoid, ends_with("total"), ends_with("capita"), ends_with("score"))

write_csv(layer_one, file = here("composite/layer-one-composite.csv"))

zhvi <- read_csv(here("composite/zhvi.csv"), col_types = cols("geoid" = col_character()))
unemp <- read_csv(here("composite/unemp.csv"), col_types = cols("geoid" = col_character()))
tax_sales <- read_csv(here("composite/tax-sales.csv"), col_types = cols("geoid" = col_character()))
tax_dist <- read_csv(here("composite/tax-distributions.csv"), col_types = cols("geoid" = col_character()))
covid <- read_csv(here("composite/cases-per.csv"), col_types = cols("geoid" = col_character()))
ui_claims <- read_csv(here("composite/ui-claims.csv"), col_types = cols("geoid" = col_character()))

layer_two <- zhvi %>%
  left_join(select(unemp, 3:5)) %>%
  left_join(select(tax_sales, 3:5)) %>%
  left_join(select(tax_dist, 3:5)) %>%
  left_join(select(covid, 3:5)) %>%
  left_join(select(ui_claims, 3:6)) %>%
  mutate(across(.fns = ~replace_na(.x, 0))) %>%
  select(name, namelsad, geoid, ends_with("change"), ends_with("per"), ends_with("score"))

write_csv(layer_two, file = here("composite/layer-two-composite.csv"))

layer_one %>%
  select(name, namelsad, geoid, ends_with("score")) %>%
  pivot_longer(cols = ends_with("score"), names_to = "layer_metric", values_to = "score") %>%
  group_by(name, geoid) %>%
  summarise(total = sum(score)) %>%
  arrange(desc(total))
