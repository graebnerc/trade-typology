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

