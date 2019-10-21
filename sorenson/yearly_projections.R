require(tidyverse)
require(siverse)
require(furrr)
require(tigris)

source('./SCRIPTS/003-proj_basedataload.R')

ssp <- read_csv("~/Google Drive/SI/DataScience/data/gates/Hauer Population Projections/SSP_asrc.csv") %>% 
  clean_names()

plan(multisession)

past <- K05_pop %>% 
  clean_names() %>% 
  ungroup() %>% 
  select(-countyrace) %>% 
  group_by(geoid, race, state, county, year) %>% 
  summarize(total_pop = sum(population)) %>% 
  ungroup() %>% 
  mutate(race = as.numeric(race))

fut <- ssp %>% 
  group_by(geoid, race, state, county, year) %>% 
  summarize(total_pop = sum(ssp2))

poprocks <- bind_rows(past, fut) %>% 
  arrange(geoid, race, state, county, year) %>% 
  filter(geoid != "99999") %>% 
  add_count(geoid, name = "county_rows") %>% 
  mutate(county_changed = county_rows < 180) %>% 
  select(-county_rows) %>% 
  group_by(geoid, race, state, county, county_changed) %>% 
  nest() %>% 
  mutate(total_pop_interp = future_map(data, ~approx(x = .$year, y = .$total_pop, xout = 1990:2100) %>% as_tibble())) %>% 
  unnest(total_pop_interp) %>% 
  rename(year = x, total_pop_interp = y) %>% 
  select(-data) %>% 
  mutate(race = recode(race, `1` = "White NH", `2` = "Black NH", `3` = "Hispanic", `4` = "Other NH")) %>% 
  left_join(fips_codes %>% select(state_abb = state, state = state_code, state_name) %>% distinct()) %>%
  left_join(fips_codes %>% mutate(geoid = paste0(state_code, county_code)) %>% select(geoid, county_name = county) %>% distinct(), by = "geoid")

#Save
#poprocks %>% filter(between(year, 1990, 2030)) %>% write_rds("~/Google Drive/SI/DataScience/data/gates/Hauer Population Projections/county_pop_race_1990_2030.rds")

#poprocks <- read_rds("~/Google Drive/SI/DataScience/data/gates/Hauer Population Projections/county_pop_race_1990_2030.rds")


#Look at some of this
ssp_state <- poprocks %>% 
  group_by(state_name, year, race) %>% 
  summarize(state_race_pop = sum(total_pop_interp, na.rm = T))

ssp_state %>% 
  #filter(between(year, 2017, 2020)) %>% 
  ggplot(aes(x = year, y = state_race_pop, color = race, fill = race)) +
  geom_line() + facet_wrap(~state_name, scales = "free_y")

ssp_state %>% 
  filter(state_name == "Alaska", between(year, 2017, 2020))

#Save full thing with age
pop_long <- K05_pop %>% clean_names() %>% 
  ungroup() %>% 
  select(-countyrace) %>% 
  mutate(race = as.numeric(race),
         sex = as.numeric(sex)) %>% 
  bind_rows(ssp %>% rename(population = ssp2) %>% select(-contains("ssp"))) %>% 
  mutate(race = recode(race, `1` = "White NH", `2` = "Black NH", `3` = "Hispanic", `4` = "Other NH")) %>% 
  mutate(age = paste0((age - 1) * 5, " - ", (age * 5) - 1),
         age = ifelse(age == "85 - 89", "85+", age)) %>% 
  arrange(geoid, race, state, county, year) %>% 
  filter(geoid != "99999") %>% 
  add_count(geoid, name = "county_rows") %>% 
  mutate(county_changed = county_rows < 180) %>% 
  select(-county_rows) %>% 
  group_by(geoid, race, state, county, county_changed, age, sex) %>% 
  nest() %>% 
  mutate(interp = future_map(data, ~approx(x = .$year, y = .$population, xout = 1990:2100) %>% as_tibble())) %>% 
  unnest(interp) %>% 
  rename(year = x, total_pop_interp = y) %>% 
  select(-data)

temp <- K05_pop %>% clean_names() %>% 
  ungroup() %>% 
  select(-countyrace) %>% 
  mutate(race = as.numeric(race),
         sex = as.numeric(sex)) %>% 
  bind_rows(ssp %>% rename(population = ssp2) %>% select(-contains("ssp"))) %>% 
  mutate(race = recode(race, `1` = "White NH", `2` = "Black NH", `3` = "Hispanic", `4` = "Other NH")) %>% 
  mutate(age = paste0((age - 1) * 5, " - ", (age * 5) - 1),
         age = ifelse(age == "85 - 89", "85+", age)) %>% 
  arrange(geoid, race, state, county, year) %>% 
  filter(geoid != "99999") %>% 
  add_count(geoid, name = "county_rows") %>% 
  mutate(county_changed = county_rows < 180) %>% 
  select(-county_rows) %>% 
  group_by(geoid, race, state, county, county_changed, age, sex)

temp %>% 
  group_split() %>% 
  future_map(function(x) {
    
    interp <- approx(x = x$year, y = x$population, xout = 1990:2100) %>% as_tibble() %>% 
      rename(year = x, population = y)
    
    grp <- x %>% select(-year, -population) %>% distinct()
    
    interp %>% add_column(!!!grp)
    
  }, .progress = T)
