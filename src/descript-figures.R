# Creates figures 3 to 6 from the main paper
rm(list = ls())
library(tidyverse)
library(data.table)
library(icaeDesign)

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
set_up_macro_data <- TRUE # Extracts data from package MacroDataR
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

  data.table::fwrite(macro_data, macro_data_file_name)
} else {
  macro_data <- data.table::fread(macro_data_file_name)
}

# Add the clusters to the data=================================================

macro_data$cluster <- NA

for (cl in names(clustering)) {
  macro_data <- macro_data %>%
    mutate(cluster = ifelse(iso3c %in% clustering[[cl]], cl, cluster))
}

# Merge macro data-------------------------------------------------------------

macro_data_agg <- macro_data %>%
  select(-iso3c) %>%
  group_by(year, cluster) %>%
  summarise_all(.funs = c(mean, sd), na.rm = T) %>%
  ungroup()

# Add cumulative data----------------------------------------------------------
macro_data_cumulated <- macro_data %>%
  select(
    iso3c, year, current_account_GDP_ameco,
    gdp_real_pc_ppp, unemp_rate
  ) %>%
  filter(year > 1993, year < 2018) %>%
  group_by(iso3c) %>%
  mutate(
    gdp_real_pc_ppp_growth = (
      gdp_real_pc_ppp - lag(gdp_real_pc_ppp)) / lag(gdp_real_pc_ppp),
    unemp_rate_change = (unemp_rate - lag(unemp_rate)) / lag(unemp_rate)
  ) %>%
  summarise(
    CA_cum = mean(current_account_GDP_ameco, na.rm = T),
    GDPpc_cum = mean(gdp_real_pc_ppp, na.rm = T),
    GDPpc_growth_cum = mean(gdp_real_pc_ppp_growth, na.rm = T),
    unemp_rates = mean(unemp_rate, na.rm = T),
    unemp_rate_cum = mean(unemp_rate_change, na.rm = T),
    unemp_rate_sum = sum(unemp_rate_change, na.rm = T)
  ) %>%
  ungroup()
macro_data_cumulated$cluster <- NA

for (cl in names(clustering)) {
  macro_data_cumulated <- macro_data_cumulated %>%
    mutate(
      cluster = ifelse(iso3c %in% clustering[[cl]], cl, cluster)
    )
}
macro_data_cumulated <- arrange(macro_data_cumulated, cluster) %>%
  mutate(
    cluster = factor(cluster,
      levels = unique(macro_data_cumulated$cluster),
      ordered = T
    )
  )

# Create figures===============================================================

#' Transform plot appearance
#'
#' Transforms plots into a uniform appearance.
#'
#' @param old_plot The original ggplot2 plot.
#' @return An updated ggplot2 object.
pretty_up_ggplot <- function(old_plot,
                             type_x_axis = "continuous") {
  new_plot <- old_plot +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing.x = unit(0.2, "cm")
    )
  if (type_x_axis == "continuous") {
    new_plot <- new_plot +
      scale_x_continuous(expand = c(0, 0))
  }
  return(new_plot)
}

ks <- function(x) {
  scales::number_format(
    accuracy = 1,
    scale = 1 / 1000,
    suffix = "k",
    big.mark = ".",
    decimal.mark = ","
  )(x)
}

fig_width <- 9
fig_height <- 6

first_year <- 1995 # chosen for data availability
last_year <- 2017 # chosen for data availability

# Figure 3: GDP per capita-----------------------------------------------------
fig_gdp_pc <- ggplot(
  filter(macro_data_agg, year < 2018),
  aes(
    x = year,
    y = gdp_real_pc_ppp_fn1,
    color = cluster
  )
) +
  geom_ribbon(
    aes(
      ymin = gdp_real_pc_ppp_fn1 - 0.5 * gdp_real_pc_ppp_fn2,
      ymax = gdp_real_pc_ppp_fn1 + 0.5 * gdp_real_pc_ppp_fn2,
      fill = cluster
    ),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point()

fig_gdp_pc <- pretty_up_ggplot(fig_gdp_pc) +
  ggtitle("Real GDP per capita") +
  scale_x_continuous(limits = c(first_year, last_year), expand = c(0, 0)) +
  scale_y_continuous(labels = ks) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  theme(
    axis.title = element_blank(),
    axis.title.y = element_blank()
  )

fig_gdp_pc

fig_gdp_cum_growth <- ggplot(macro_data_cumulated) +
  geom_bar(aes(
    x = reorder(iso3c, GDPpc_growth_cum),
    y = GDPpc_growth_cum,
    fill = cluster, color = cluster
  ),
  stat = "identity"
  ) +
  ggtitle("Mean GDP per capita growth (1995-2017)") +
  scale_x_discrete(
    limits = c(
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Periphery"]]
      ), GDPpc_growth_cum)$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Flexible labor markets model"]]
        ), GDPpc_growth_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Financel hub"]]
        ), GDPpc_growth_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["High tech model"]]
        ), GDPpc_growth_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Industrial workbench model"]]
        ), GDPpc_growth_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Primary goods model"]]
        ), GDPpc_growth_cum
      )$iso3c
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 100, accuracy = 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.2, "cm")
  )
fig_gdp_cum_growth

gdp_pc_full <- ggpubr::ggarrange(
  fig_gdp_cum_growth, fig_gdp_pc,
  ncol = 2, nrow = 1, common.legend = T, legend = "bottom",
  labels = c("A)", "B)"), font.label = list(face = "bold")
)
ggsave(
  plot = gdp_pc_full,
  filename = "output/fig_3_gdp-growth.pdf",
  width = fig_width * 1.5, height = fig_height
)


# Figure 4: Unemployment rate, 1994 - 2018-------------------------------------

unemp_rate <- ggplot(
  macro_data_agg,
  aes(
    x = year,
    y = unemp_rate_fn1,
    color = cluster
  )
) +
  geom_ribbon(
    aes(
      ymin = unemp_rate_fn1 - 0.5 * unemp_rate_fn2,
      ymax = unemp_rate_fn1 + 0.5 * unemp_rate_fn2,
      fill = cluster
    ),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point()

unemp_rate <- pretty_up_ggplot(unemp_rate) +
  ggtitle("Unemployment rate (1995-2018)") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_x_continuous(
    limits = c(1995, 2018),
    breaks = seq(1995, 2015, 5),
    expand = expand_scale(
      c(0, 0),
      c(0, 0.25)
    )
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  theme(
    axis.title = element_blank()
  )
unemp_rate


unemp_cum <- ggplot(macro_data_cumulated) +
  geom_bar(aes(
    x = reorder(iso3c, unemp_rate_sum),
    y = unemp_rate_cum,
    fill = cluster, color = cluster
  ),
  stat = "identity"
  ) +
  ggtitle("Average change in the unemployment rate (1995-2017)") +
  scale_x_discrete(
    limits = c(
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Industrial workbench model"]]
        ), unemp_rate_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Flexible labor markets model"]]
        ), unemp_rate_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["High tech model"]]
        ), unemp_rate_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Primary goods model"]]
        ), unemp_rate_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Financel hub"]]
        ), unemp_rate_cum
      )$iso3c,
      arrange(
        filter(
          macro_data_cumulated,
          iso3c %in% clustering[["Periphery"]]
        ), unemp_rate_cum
      )$iso3c
    )
  ) +
  scale_y_continuous(
    labels = scales::percent_format(
      scale = 100,
      accuracy = 1
    ),
    breaks = seq(-0.03, 0.05, 0.01)
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.2, "cm")
  )
unemp_cum


unemp_full <- ggpubr::ggarrange(
  unemp_cum, unemp_rate,
  ncol = 2, nrow = 1, common.legend = T, legend = "bottom",
  labels = c("A)", "B)"), font.label = list(face = "bold")
)
ggsave(
  plot = unemp_full,
  filename = "output/fig_4_unemployment.pdf",
  width = fig_width * 1.5, height = fig_height
)

# Figure 5: Current account----------------------------------------------------

fig_current_account_abs <- ggplot(
  macro_data_agg,
  aes(
    x = year,
    y = current_account_GDP_ameco_fn1,
    color = cluster
  )
) +
  geom_ribbon(
    aes(
      ymin = current_account_GDP_ameco_fn1 - 0.5 * current_account_GDP_ameco_fn2,
      ymax = current_account_GDP_ameco_fn1 + 0.5 * current_account_GDP_ameco_fn2,
      fill = cluster
    ),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point()

fig_current_account_abs <- pretty_up_ggplot(fig_current_account_abs) +
  ggtitle("Current Account in % of GDP (1995-2017)") +
  scale_y_continuous(
    limits = c(-20, 11),
    breaks = seq(-20, 10, 5),
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  scale_x_continuous(
    limits = c(1995, 2017),
    breaks = seq(1995, 2015, 5),
    expand = expand_scale(
      c(0, 0),
      c(0, 0.25)
    )
  ) +
  theme(
    axis.title = element_blank()
  )

fig_current_account_abs

fig_current_account_cum <- ggplot(macro_data_cumulated) +
  geom_bar(aes(
    x = reorder(iso3c, CA_cum),
    y = CA_cum,
    fill = cluster, color = cluster
  ),
  stat = "identity"
  ) +
  ggtitle("Average Current Account in % of GDP (1995-2017)") +
  ylab("Average current account") +
  scale_x_discrete(
    limits = c(
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Periphery"]]
      ), CA_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Primary goods model"]]
      ), CA_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Flexible labor markets model"]]
      ), CA_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Industrial workbench model"]]
      ), CA_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["High tech model"]]
      ), CA_cum)$iso3c,
      arrange(filter(
        macro_data_cumulated,
        iso3c %in% clustering[["Financel hub"]]
      ), CA_cum)$iso3c
    )
  ) +
  scale_y_continuous(
    limits = c(-7.1, 7),
    breaks = seq(-6, 6, 2),
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_fill_manual(
    limits = names(unlist(cluster_cols)),
    values = c(unlist(cluster_cols)),
    aesthetics = c("fill", "color")
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 7),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing.x = unit(0.2, "cm")
  )
fig_current_account_cum

fig_current_account_full <- ggpubr::ggarrange(
  fig_current_account_cum, fig_current_account_abs,
  ncol = 2, nrow = 1, common.legend = T, legend = "bottom",
  labels = c("A)", "B)"), font.label = list(face = "bold")
)

ggsave(
  filename = "output/fig_5_current-account.pdf",
  width = fig_width * 1.5, height = fig_height
)


# Figure 6: Inequality comparison----------------------------------------------

#' Create an inequality barplot
#'
#' Takes inequality data and creates a barplot. Will be used to create
#'  similar plots for early, late and overall time period. The plot
#'  will visualize changes frim first to last data point in all
#'  the variables.
#'
#' @param barplot_data The inequality data for the barplot
#' @param time_period A vector with two elements indicating first and
#'  last year considered. Used for the plot title.
#' @return A ggplot2 barplot object.
make_ineq_barplot <- function(barplot_data, time_period, x_axis_range) {
  ineq_comparison_plot <- ggplot(barplot_data) +
    geom_bar(aes(
      x = variable,
      y = value,
      color = cluster,
      fill = cluster
    ),
    stat = "identity",
    position = "dodge"
    ) +
    coord_flip()

  ineq_comparison_plot <- pretty_up_ggplot(ineq_comparison_plot,
    type_x_axis = "discrete"
  ) +
    scale_y_continuous(
      limits = x_axis_range, name = "Change in %",
      labels = scales::percent_format(scale = 1)
    ) +
    theme(
      axis.title.y = element_blank()
    ) +
    ggtitle(
      paste0("Inequality in ", time_period[1], " and ", time_period[2])
    ) +
    scale_fill_icae(palette = "mixed") +
    scale_color_icae(palette = "mixed")

  return(ineq_comparison_plot)
}

#' Get the last characters of a string
#'
#' Returns the last x characters of a string, where x can be specified
#'
#' @param a_string A string
#' @param n_char Specifies how many of the last characters should be returned
#' @return A string with the last \code{n_char} characters of \code{a_string}
get_last_char <- function(a_string, n_char) {
  if (as.integer(n_char) != n_char) {
    warning("n_char is not an integer, will be coerced!")
  }
  n_char <- as.integer(n_char)
  if (nchar(a_string) <= n_char) {
    warning("String shorter of of equal length as n_char. Return full string.")
    return(a_string)
  }
  n_char <- n_char - 1
  last_chars <- substr(
    a_string,
    nchar(a_string) - n_char, nchar(a_string)
  )
  return(last_chars)
}

ineq_data_overall <- macro_data %>%
  filter(year %in% c(1994, 2007, 2008, 2016)) %>%
  select(
    one_of("cluster", "year", "gini_post_tax", "gini_pre_tax", "wage_share")
  ) %>%
  group_by(year, cluster) %>%
  summarise_all(mean, na.rm = T) %>%
  ungroup() %>%
  gather(variable, value, -year, -cluster) %>%
  unite("observation", c("variable", "year")) %>%
  spread(observation, value) %>%
  mutate(
    gini_post_tax_diff_full = gini_post_tax_2016 - gini_post_tax_1994,
    gini_post_tax_change_full = gini_post_tax_diff_full / gini_post_tax_1994 * 100,
    gini_pre_tax_diff_full = gini_pre_tax_2016 - gini_pre_tax_1994,
    gini_pre_tax_change_full = gini_pre_tax_diff_full / gini_pre_tax_1994 * 100,
    wage_share_diff_full = wage_share_2016 - wage_share_1994,
    wage_share_change_full = wage_share_diff_full / wage_share_1994 * 100,
    gini_post_tax_diff_early = gini_post_tax_2007 - gini_post_tax_1994,
    gini_post_tax_change_early = gini_post_tax_diff_early / gini_post_tax_1994 * 100,
    gini_pre_tax_diff_early = gini_pre_tax_2007 - gini_pre_tax_1994,
    gini_pre_tax_change_early = gini_pre_tax_diff_early / gini_pre_tax_1994 * 100,
    wage_share_diff_early = wage_share_2007 - wage_share_1994,
    wage_share_change_early = wage_share_diff_early / wage_share_1994 * 100,
    gini_post_tax_diff_late = gini_post_tax_2016 - gini_post_tax_2008,
    gini_post_tax_change_late = gini_post_tax_diff_late / gini_post_tax_2008 * 100,
    gini_pre_tax_diff_late = gini_pre_tax_2016 - gini_pre_tax_2008,
    gini_pre_tax_change_late = gini_pre_tax_diff_late / gini_pre_tax_2008 * 100,
    wage_share_diff_late = wage_share_2016 - wage_share_2008,
    wage_share_change_late = wage_share_diff_late / wage_share_2008 * 100
  ) %>%
  select(
    one_of(
      "cluster", "gini_post_tax_change_full",
      "gini_pre_tax_change_full", "wage_share_change_full",
      "gini_post_tax_change_early",
      "gini_pre_tax_change_early", "wage_share_change_early",
      "gini_post_tax_change_late",
      "gini_pre_tax_change_late", "wage_share_change_late"
    )
  ) %>%
  gather(variable, value, -cluster)

x_barplot_range <- c(-20, 20)

ineq_plot_overall <- make_ineq_barplot(
  filter(
    ineq_data_overall,
    get_last_char(variable, 4) == "full"
  ),
  c(1994, 2016),
  x_barplot_range
) +
  scale_x_discrete(labels = c("Gini (post)", "Gini (pre)", "Wage share"))
ineq_plot_overall

gini_x_range <- c(1994, 2016)
ineq_dynamics_post <- ggplot(
  macro_data_agg,
  aes(
    x = year,
    y = gini_post_tax_fn1,
    color = cluster
  )
) +
  geom_ribbon(
    aes(
      ymin = gini_post_tax_fn1 - 0.5 * gini_post_tax_fn2,
      ymax = gini_post_tax_fn1 + 0.5 * gini_post_tax_fn2,
      fill = cluster
    ),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point() +
  scale_color_icae(palette = "mixed") +
  scale_fill_icae(palette = "mixed")

ineq_dynamics_post <- pretty_up_ggplot(ineq_dynamics_post) +
  ggtitle("Income inequality (Gini post tax)") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme(
    axis.title = element_blank()
  ) +
  scale_x_continuous(
    limits = gini_x_range,
    breaks = seq(1994, 2016, 2),
    expand = expand_scale(mult = c(0, 0), add = c(0, 0.5))
  ) +
  scale_color_icae(palette = "mixed") +
  scale_fill_icae(palette = "mixed")

ineq_dynamics_post


full_ineq_dynamics_plot_post <- ggpubr::ggarrange(
  ineq_plot_overall, ineq_dynamics_post,
  ncol = 2, legend = "bottom", common.legend = TRUE,
  labels = c("A)", "B)"),
  font.label = list(face = "bold")
)

ggsave(
  filename = "output/fig_6_inquality-dynamics.pdf",
  plot = full_ineq_dynamics_plot_post,
  height = fig_height, width = 1.2 * fig_width
)