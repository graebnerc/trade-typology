# This script creates the descriptive figures in the text

# Current account in % of GDP
# GDP growth
# Unemployment
# Adjusted wage share
# Gini
library(tidyverse)
library(data.table)

# Set up dataset===============================================================
set_up_macro_data <- FALSE # Extracts data from package MacroDataR
macro_data_file_name <- "data/descriptive_data.csv"

if (set_up_macro_data){
  countries_considered <- countrycode::countrycode(
    strsplit(
      "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT",
      ", "
    )[[1]], "iso2c", "iso3c"
  )
  
  macro_data <- MacroDataR::macro_data
  macro_data <- macro_data %>%
    dplyr::select(dplyr::one_of("iso3c", "year", "current_account_GDP_ameco", 
                                "unemp_rate", "gdp_real_lcu_growth", 
                                "gini_post_tax", "gini_pre_tax", "wage_share")
    ) %>%
    dplyr::filter(iso3c %in% countries_considered, 
                  year>1994)
  
  data.table::fwrite(macro_data, macro_data_file_name)
} else {
  macro_data <- data.table::fread(macro_data_file_name)
}

