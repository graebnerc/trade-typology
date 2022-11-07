# Creates figures 3 to 6 from the main paper
here::i_am("src/trade-balance-current-account.R")
library(here)
library(tidyverse)
library(data.table)
library(icaeDesign)
library(countrycode)

clustering <- list(
  "Primary goods model" =
    countrycode::countrycode(
      c("Latvia", "Estonia"),
      "country.name", "iso3c"
    ),
  "Industrial workbench model" = countrycode::countrycode(
    c("Slovenia", "Poland", "Slovakia", "Hungary", "Czech Republic", "Czechia"),
    "country.name", "iso3c"
  ),
  "Flexible labor markets model" = countrycode::countrycode(
    c("United Kingdom"),
    "country.name", "iso3c"
  ),
  "Financel hub" = countrycode::countrycode(
    c("Luxembourg"),
    "country.name", "iso3c"
  ),
  "Periphery" = countrycode::countrycode(
    c("Greece", "Portugal", "Spain", "Italy", "France"),
    "country.name", "iso3c"
  ),
  "High tech model" = countrycode::countrycode(
    c(
      "Sweden", "Finland", "Denmark", "Netherlands",
      "Belgium", "Germany", "Austria", "Ireland"
    ),
    "country.name", "iso3c"
  )
)

cluster_cols <- list(
  "High tech model" = unname(get_icae_colors("dark blue")),
  "Periphery" = unname(get_icae_colors("purple")),
  "Flexible labor markets model" = unname(get_icae_colors("dark red")),
  "Industrial workbench model" = unname(get_icae_colors("dark green")),
  "Primary goods model" = unname(get_icae_colors("sand")),
  "Financel hub" = unname(get_icae_colors("orange"))
)

# Set up dataset===============================================================
set_up_macro_data <- FALSE # Extracts data from package MacroDataR
macro_data_file_name <- "data/descriptive_data.csv"

if (set_up_macro_data) {
  countries_considered <- countrycode::countrycode(
    strsplit(
      "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT",
      ", "
    )[[1]], "iso2c", "iso3c"
  )
  
  macro_data <- MacroDataR::macro_data
  macro_data <- macro_data %>%
    dplyr::select(
      dplyr::one_of(
        "iso3c", "year", "current_account_GDP_ameco",
        "unemp_rate",
        "gdp_real_ppp", "gdp_real_pc_ppp",
        "gini_post_tax", "gini_pre_tax", "wage_share"
      )
    ) %>%
    dplyr::filter(
      iso3c %in% countries_considered,
      year > 1993
    )
  trade_balance <- WDI::WDI(
    country = countrycode(unique(macro_data$iso3c), "iso3c", "iso2c"), 
    start = min(macro_data$year), 
    end = max(macro_data$year), 
    indicator = c(
      "ext_balance_usd"="NE.RSB.GNFS.CD",
      "ext_balance_gdp"="NE.RSB.GNFS.ZS")) 
  macro_data <- left_join(macro_data, trade_balance, by = c("iso3c", "year"))
  data.table::fwrite(macro_data, here(macro_data_file_name))
} else {
  macro_data <- data.table::fread(here(macro_data_file_name))
}
macro_data_2 <- select(macro_data, iso3c, year, ext_balance_gdp, current_account_GDP_ameco)

macro_data_agg <- macro_data_2 %>% 
  group_by(iso3c) %>% 
  summarise(across(
    .fns = ~ mean(.x, na.rm=TRUE), 
    names = "{.fn}"), .groups = "drop") 

scatter_plot <- ggplot(
  data = macro_data_agg, aes(x=ext_balance_gdp, 
                y=current_account_GDP_ameco, 
                color=iso3c))+
  geom_point() +
  ggrepel::geom_label_repel(
    aes(label=iso3c), show.legend = FALSE, max.overlaps = 15) +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_euf(palette = "mixed") +
  theme_icae() +
  theme(legend.position = c(0.9, 0.3))
scatter_plot

ggsave(plot = scatter_plot, width = 9, height = 6, filename = here("TB-CA.pdf"))
