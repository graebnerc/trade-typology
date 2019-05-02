rm(list = ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(haven)

# Data preparation=============================================================
# R version of "Syntax_aufbereitung"
# Here the data will be prepared for the upcoming clustering exercise
data_source <- "MIT" # or "HARV

endownments <- data.table::fread(
  paste0("data/dimension_endownment_data_", data_source, ".csv")
)
ictwss <- haven::read_dta("data/ictwss_short.dta") %>%
  dplyr::mutate(country=countrycode(country, "country.name", "iso3c"),
                year=as.double(year),
                adjcov=as.double(adjcov),
                coord=as.double(coord))

cluster_data_R <- dplyr::full_join(ictwss, endownments, 
                                   by=c("country"="location_code", "year"))

cluster_data_DTA <- haven::read_dta("data/v34_cluster.dta")

# TODO cluster_data_DTA weist deutlich mehr variablen auf als sich aus dem do file ergibt. Warum?

# Process data=================================================================
# Make means-------------------------------------------------------------------
# hier werden die mittelwerte genommen
cluster_data_DTA_v2 <- cluster_data_DTA %>%
  select(-one_of("v1", "x", "country")) %>%
  dplyr::group_by(un_ccode) %>%
  dplyr::summarise_all(mean, na.rm=T) %>%
  dplyr::ungroup()

# Process data: make means-----------------------------------------------------
drop_countries <- c("Canada", "Mexico", "New Zealand", "Turkey", "Switzerland",
                    "United States", "Iceland", "Australia", "Korea", "Norway",
                    "Japan", "Bulgaria", "Slovak Republic", "Lithuania")
drop_countries <- countrycode::countrycode(drop_countries, 
                                           "country.name", "un")

cluster_data_DTA_v3 <- cluster_data_DTA_v2 %>%
  dplyr::filter(!un_ccode %in% drop_countries)

# Process data: z standartization----------------------------------------------
# TODO Check warum man das genau braucht und ob das sinnvoll ist








