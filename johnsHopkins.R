
library(tidyverse) 

# cases -------------------------------------------------------------------

## johns hopkins university github
johns_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

## johns cases ky map
johns_cases_ky_map <- johns_cases %>%
  filter(Province_State %in% 'Kentucky') %>%
  dplyr::select(-c(UID,iso2,iso3,code3,Province_State,Country_Region,
            Lat,Long_,Combined_Key)) %>%
  group_by(FIPS, Admin2) %>%
  dplyr::select(FIPS, Admin2, cases_cum = tail(names(.), 1))

# deaths ------------------------------------------------------------------

## johns hopkins university github
johns_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

## johns deaths ky map
johns_deaths_ky_map <- johns_deaths %>%
  filter(Province_State %in% 'Kentucky') %>%
  dplyr::select(-c(UID,iso2,iso3,code3,Province_State,Country_Region,
            Lat,Long_,Combined_Key,Population)) %>%
  group_by(FIPS, Admin2) %>%
  dplyr::select(FIPS, Admin2,deaths_cum = tail(names(.), 1))

# turn raw case/deaths counts into rates ----------------------------------

ky_pop <- read_csv("dat/worldpopreview_ky2020_county.csv")
ky_pop$CTYNAME <- word(ky_pop$CTYNAME, 1)
ky_pop$GrowthRate <- NULL

## cases map epi
johns_cases_ky_map <- inner_join(johns_cases_ky_map, ky_pop, by = c('Admin2' = 'CTYNAME')) %>%
  mutate(pop_char = format(pop2018, big.mark=","),
         cases_county_rate = round(cases_cum * 10000 / pop2018, 1))

## cases rank
johns_cases_ky_map$cases_rank <- min_rank(johns_cases_ky_map$cases_county_rate)

## cases statewide avg
johns_cases_ky_map$cases_statewide_rate <- round(sum(
  johns_cases_ky_map$cases_cum) *10000 / sum(johns_cases_ky_map$pop2018), 1)


## deaths map epi
johns_deaths_ky_map <- inner_join(johns_deaths_ky_map, ky_pop, by = c('Admin2' = 'CTYNAME')) %>%
  mutate(pop_char = format(pop2018, big.mark=","),
         deaths_county_rate = round(deaths_cum * 10000 / pop2018, 2))

## deaths rank
johns_deaths_ky_map$deaths_rank <- min_rank(johns_deaths_ky_map$deaths_county_rate)

## deaths statewide avg
johns_deaths_ky_map$deaths_statewide_rate <- round(sum(
  johns_deaths_ky_map$deaths_cum) * 10000 / sum(johns_deaths_ky_map$pop2018), 2)
