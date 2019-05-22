# This script creates the descriptive figures in the text

# Current account in % of GDP
# GDP growth
# Unemployment
# Adjusted wage share
# Gini
library(tidyverse)
library(data.table)

# Set up dataset===============================================================
set_up_macro_data <- TRUE # Extracts data from package MacroDataR
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
                  year>1993)
  
  data.table::fwrite(macro_data, macro_data_file_name)
} else {
  macro_data <- data.table::fread(macro_data_file_name)
}

# Add the clusters to the data=================================================

clustering <- list(
  "primary_goods" = countrycode::countrycode(
    c("Latvia", "Estonia"), 
    "country.name", "iso3c"),
  "finance" = countrycode::countrycode(
    c("Luxembourg"), "country.name", "iso3c"),
  "flexible_labor" = countrycode::countrycode(
    c("United Kingdom", "Ireland", "Hungary"), 
    "country.name", "iso3c"),
  "less_globalized" = countrycode::countrycode(
    c("Poland", "Greece", "Portugal", "Slovakia", "Spain", "Italy", "Slovenia",
      "Czech Republic", "Czechia"), 
    "country.name", "iso3c"),
  "high_tech" = countrycode::countrycode(
    c("France", "Sweden", "Finland", "Denmark", "Netherlands", "Belgium", 
      "Germany", "Austria"), 
    "country.name", "iso3c")
)


macro_data$cluster <- NA

for (cl in names(clustering)){
  macro_data <- macro_data %>%
    mutate(cluster=ifelse(iso3c %in% clustering[[cl]], cl, cluster))
}

macro_data_agg <- macro_data %>%
  select(-iso3c) %>%
  group_by(year, cluster) %>%
  summarise_all(.funs = c(mean, sd), na.rm=T) %>%
  ungroup()


# Create figures===============================================================

#' Transform plot appearance
#' 
#' Transforms plots into a uniform appearance.
#' 
#' @param old_plot The original ggplot2 plot.
#' @return An updated ggplot2 object.
pretty_up_ggplot <- function(old_plot, 
                             type_x_axis="continuous"){
  new_plot <- old_plot +
    theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line(),
          legend.position = "bottom",
          legend.title = element_blank()
    )
  if (type_x_axis=="continuous"){
    new_plot <- new_plot +    
      scale_x_continuous(expand = c(0, 0))
  }
  return(new_plot)
}

fig_width <- 9
fig_height <- 6


# Figure 3: Current account----------------------------------------------------

fig_current_account <- ggplot(macro_data_agg, 
                           aes(x=year,
                               y=current_account_GDP_ameco_fn1,
                               color=cluster)
) + 
  geom_ribbon(
    aes(ymin = current_account_GDP_ameco_fn1 - 0.5*current_account_GDP_ameco_fn2, 
        ymax = current_account_GDP_ameco_fn1 + 0.5*current_account_GDP_ameco_fn2,
        fill=cluster), 
    alpha=0.5, color=NA
  ) +
  geom_line() + 
  geom_point()

fig_current_account <- pretty_up_ggplot(fig_current_account) +
  ggtitle("Current Account") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme(
    axis.title = element_blank()
  )

fig_current_account

ggsave(filename = "output/fig_3_current-account.pdf", 
       width = fig_width, height = fig_height)


# Figure 4: GDP growth---------------------------------------------------------

fig_gdp_growth <- ggplot(filter(macro_data_agg, year<2018), 
                              aes(x=year,
                                  y=gdp_real_lcu_growth_fn1,
                                  color=cluster)
) + 
  geom_ribbon(
    aes(ymin = gdp_real_lcu_growth_fn1 - 0.5*gdp_real_lcu_growth_fn2, 
        ymax = gdp_real_lcu_growth_fn1 + 0.5*gdp_real_lcu_growth_fn2,
        fill=cluster), 
    alpha=0.5, color=NA
  ) +
  geom_line() + 
  geom_point()

fig_gdp_growth <- pretty_up_ggplot(fig_gdp_growth) +
  ggtitle("Growth of real GDP") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme(
    axis.title = element_blank()
  )

fig_gdp_growth

ggsave(filename = "output/fig_4_gdp-growth.pdf", 
       width = fig_width, height = fig_height)


# Figure 5: Unemployment rate, 1994 - 2016-------------------------------------

fig_unemployment <- ggplot(macro_data_agg, 
                           aes(x=year,
                               y=unemp_rate_fn1,
                               color=cluster)
                           ) + 
  geom_ribbon(
    aes(ymin = unemp_rate_fn1 - 0.5*unemp_rate_fn2, 
        ymax = unemp_rate_fn1 + 0.5*unemp_rate_fn2,
        fill=cluster), 
    alpha=0.5, color=NA
    ) +
  geom_line() + 
  geom_point()

fig_unemployment <- pretty_up_ggplot(fig_unemployment) +
  ggtitle("Unemployment rate") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
    ) +
  theme(
    axis.title = element_blank()
    )

fig_unemployment

ggsave(filename = "output/fig_5_unemployment.pdf", 
       width = fig_width, height = fig_height)

# Figure 6: Inequality comparison----------------------------------------------

ineq_data_overall <- macro_data %>%
  filter(year %in% c(1994, 2016)) %>%
  select(
    one_of("cluster", "year", "gini_post_tax", "gini_pre_tax", "wage_share")
    ) %>%
  group_by(year, cluster) %>%
  summarise_all(mean, na.rm=T) %>%
  ungroup() %>%
  gather(variable, value, -year, -cluster) %>%
  unite("observation", c("variable", "year")) %>%
  spread(observation, value) %>%
  mutate(
    gini_post_tax_diff=gini_post_tax_2016-gini_post_tax_1994,
    gini_post_tax_change=gini_post_tax_diff/gini_post_tax_1994,
    gini_pre_tax_diff=gini_pre_tax_2016-gini_pre_tax_1994,
    gini_pre_tax_change=gini_pre_tax_diff/gini_pre_tax_1994,
    wage_share_diff=wage_share_2016-wage_share_1994,
    wage_share_change=wage_share_diff/wage_share_1994
  ) %>%
  select(
    one_of("cluster", "gini_post_tax_change", 
           "gini_pre_tax_change", "wage_share_change")
    ) %>%
  gather(variable, value, -cluster)
ineq_data_overall

ineq_comparison_plot <- ggplot(ineq_data_overall) +
  geom_bar(aes(x=variable,
               y=value,
               color=cluster,
               fill=cluster),
           stat = "identity", 
           position = "dodge") + 
  coord_flip()

ineq_comparison_plot <- pretty_up_ggplot(ineq_comparison_plot)
ineq_comparison_plot
