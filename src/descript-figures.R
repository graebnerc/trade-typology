# This script creates the descriptive figures in the text
rm(list = ls())
library(tidyverse)
library(data.table)
library(icaeDesign)

icae_public_colors <- c(
  `orange` = "#ff9900",
  `purple` = "#8600b3",
  `dark green` = "#006600",
  `sand` = "#d8c469",
  `dark blue` = "#002b80",
  `dark red` = "#800000"
  )

cluster_cols <- list(
  "High tech" = unname(icae_public_colors["dark blue"]),
  "Periphery" = unname(icae_public_colors["purple"]),
  "UK" = unname(icae_public_colors["dark red"]),
  "Catchup" = unname(icae_public_colors["dark green"]),
  "Primary goods" = unname(icae_public_colors["sand"]),
  "Finance" = unname(icae_public_colors["orange"])
)

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
                                "unemp_rate", 
                                "gdp_real_ppp", "gdp_real_pc_ppp",
                                "gini_post_tax", "gini_pre_tax", "wage_share",
                                "kof_trade_df",  "kof_fin_df", "trade_total_GDP", "trade_exp_GDP", "trade_imp_GDP")
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

clustering <- list(
  "Primary goods" = 
    countrycode::countrycode(
      c("Latvia", "Estonia"),
      "country.name", "iso3c"),
  "Catchup" = countrycode::countrycode(
    c("Slovenia", "Poland","Slovakia","Hungary", "Czech Republic", "Czechia"),
    "country.name", "iso3c"),
  "UK" = countrycode::countrycode(
    c("United Kingdom"),
    "country.name", "iso3c"),
  "Finance" = countrycode::countrycode(
    c("Luxembourg"),
    "country.name", "iso3c"),
  "Periphery" = countrycode::countrycode(
    c("Greece", "Portugal", "Spain", "Italy", "France"),
    "country.name", "iso3c"),
  "High tech" = countrycode::countrycode(
    c("Sweden", "Finland", "Denmark", "Netherlands", 
                  "Belgium", "Germany", "Austria", "Ireland"),
    "country.name", "iso3c")
)


macro_data$cluster <- NA

for (cl in names(clustering)){
  macro_data <- macro_data %>%
    mutate(cluster=ifelse(iso3c %in% clustering[[cl]], cl, cluster))
}

# Add cumulative growth rates--------------------------------------------------
first_year <- 1995 # chosen for data availability
last_year <- 2017 # chosen for data availability
min_CA <- abs(floor(min(macro_data$current_account_GDP_ameco, na.rm=T)))
macro_data_cumul_growth <- data.table(macro_data)
macro_data_cumul_growth <- macro_data_cumul_growth[
  year>=first_year & year<=last_year, 
  .(year, iso3c, gdp_real_ppp, current_account_GDP_ameco, gdp_real_pc_ppp)
  ]
macro_data_cumul_growth[
  , current_account_GDP_ameco_cgrowth:= 
    (current_account_GDP_ameco/first(current_account_GDP_ameco))**
    (1/(year-first(year)))-1, 
  .(iso3c)
  ]
macro_data_cumul_growth[
  , current_account_GDP_ameco_pos:=current_account_GDP_ameco+min_CA
  ][ , current_account_GDP_ameco_base95:=(current_account_GDP_ameco_pos/
                                            first(current_account_GDP_ameco_pos))*100, 
    .(iso3c)
    ]
macro_data_cumul_growth[
  , gdp_real_ppp_cgrowth:= (gdp_real_ppp/first(gdp_real_ppp))**
    (1/(year-first(year)))-1, 
  .(iso3c)
  ][, gdp_real_ppp_base95:=(
    gdp_real_ppp/first(gdp_real_ppp))*100, .(iso3c)
    ]
macro_data_cumul_growth[
  , gdp_real_pc_ppp_cgrowth:= (gdp_real_pc_ppp/first(gdp_real_pc_ppp))**
    (1/(year-first(year)))-1, 
  .(iso3c)
  ][, gdp_real_pc_ppp_base95:=(
    gdp_real_pc_ppp/first(gdp_real_pc_ppp))*100, .(iso3c)
    ]
macro_data_cumul_growth <- macro_data_cumul_growth[
  , .(year, iso3c, current_account_GDP_ameco_cgrowth, 
      gdp_real_ppp_cgrowth, gdp_real_pc_ppp_cgrowth,
      current_account_GDP_ameco_base95,
      gdp_real_ppp_base95, gdp_real_pc_ppp_base95,current_account_GDP_ameco_pos)]

# Merge macro data-------------------------------------------------------------

macro_data_agg <- macro_data %>%
  left_join(., macro_data_cumul_growth, by=c("iso3c", "year")) %>%
  select(-iso3c) %>%
  group_by(year, cluster) %>%
  summarise_all(.funs = c(mean, sd), na.rm=T) %>%
  ungroup()

# Add cumulative data----------------------------------------------------------
# Er meint mit cumulative einfach die Summe von 1994-2016
macro_data_cumulated <- macro_data %>%
  select(iso3c, year, current_account_GDP_ameco, 
         gdp_real_pc_ppp, unemp_rate) %>%
  filter(year>1993, year<2018) %>%
  group_by(iso3c) %>%
  mutate(
    gdp_real_pc_ppp_growth=(gdp_real_pc_ppp-lag(gdp_real_pc_ppp))/lag(gdp_real_pc_ppp)
    )%>%
  summarise(CA_cum=mean(current_account_GDP_ameco, na.rm = T),
            GDPpc_cum=mean(gdp_real_pc_ppp, na.rm = T),
            GDPpc_growth_cum=mean(gdp_real_pc_ppp_growth, na.rm = T),
            unemp_rates=mean(unemp_rate, na.rm = T)) %>%
  ungroup() 
macro_data_cumulated$cluster <- NA

for (cl in names(clustering)){
  macro_data_cumulated <- macro_data_cumulated %>%
    mutate(cluster=ifelse(iso3c %in% clustering[[cl]], cl, cluster))
}
macro_data_cumulated <- arrange(macro_data_cumulated, cluster) %>%
  mutate(cluster=factor(cluster, levels=unique(macro_data_cumulated$cluster), ordered = T)) 

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
          legend.title = element_blank(),
          legend.spacing.x = unit(0.2, "cm")
    )
  if (type_x_axis=="continuous"){
    new_plot <- new_plot +    
      scale_x_continuous(expand = c(0, 0))
  }
  return(new_plot)
}

ks <- function (x) { 
  scales::number_format(accuracy = 1,
                        scale = 1/1000,
                        suffix = "k",
                        big.mark = ".", 
                        decimal.mark = ",")(x) 
}

fig_width <- 9
fig_height <- 6

# Figure 3: GDP per capita-----------------------------------------------------
fig_gdp_pc <- ggplot(filter(macro_data_agg, year<2018), 
                     aes(x=year,
                         y=gdp_real_pc_ppp_fn1,
                         color=cluster)
) + 
  geom_ribbon(
    aes(ymin = gdp_real_pc_ppp_fn1 - 0.5*gdp_real_pc_ppp_fn2, 
        ymax = gdp_real_pc_ppp_fn1 + 0.5*gdp_real_pc_ppp_fn2,
        fill=cluster), 
    alpha=0.5, color=NA
  ) +
  geom_line() + 
  geom_point()

fig_gdp_pc <- pretty_up_ggplot(fig_gdp_pc) +
  ggtitle("Real GDP per capita") + 
  scale_x_continuous(limits = c(first_year, last_year), expand = c(0, 0)) +
  scale_y_continuous(labels = ks) +
  scale_fill_manual(limits = names(unlist(cluster_cols)), 
                    values=c(unlist(cluster_cols)), 
                    aesthetics = c("fill", "color")) +
  theme(
    axis.title = element_blank()
  )

fig_gdp_pc

fig_gdp_cum_growth <- ggplot(macro_data_cumulated) + 
  geom_bar(aes(x=reorder(iso3c, GDPpc_growth_cum), 
               y=GDPpc_growth_cum, 
               fill=cluster, color=cluster), 
           stat = "identity") +
  ggtitle("Mean GDP per capita growth (1995-2017)") + 
  scale_x_discrete(
    limits=c(
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["Periphery"]]), GDPpc_growth_cum)$iso3c,
      arrange(
        filter(macro_data_cumulated, 
               iso3c %in% clustering[["UK"]]), GDPpc_growth_cum)$iso3c,
      arrange(
        filter(macro_data_cumulated, 
               iso3c %in% clustering[["Finance"]]), GDPpc_growth_cum)$iso3c,
      arrange(
        filter(macro_data_cumulated, 
               iso3c %in% clustering[["High tech"]]), GDPpc_growth_cum)$iso3c,
      arrange(
        filter(macro_data_cumulated, 
               iso3c %in% clustering[["Catchup"]]), GDPpc_growth_cum)$iso3c,
      arrange(
        filter(macro_data_cumulated, 
               iso3c %in% clustering[["Primary goods"]]), GDPpc_growth_cum)$iso3c
    )
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1), 
                     expand = c(0, 0)
  ) +
  scale_fill_manual(limits = names(unlist(cluster_cols)), 
                    values=c(unlist(cluster_cols)), 
                    aesthetics = c("fill", "color")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 7),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0.2, "cm")
  )
fig_gdp_cum_growth

gdp_pc_full <- ggpubr::ggarrange(
  fig_gdp_pc, fig_gdp_cum_growth, 
  ncol = 2, nrow = 1, common.legend = T, legend = "bottom", 
  labels = c("A)", "B)"), font.label = list(face="bold")
)
ggsave(plot = gdp_pc_full,
       filename = "output/fig_3_gdp-growth.pdf", 
       width = fig_width*1.5, height = fig_height)


# Figure 4: Unemployment rate, 1994 - 2016-------------------------------------

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
  geom_point() + 
  scale_fill_icae(palette = "mixed") + scale_color_icae(palette = "mixed")

fig_unemployment <- pretty_up_ggplot(fig_unemployment) +
  ggtitle("Unemployment rate") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme(
    axis.title = element_blank()
  )

fig_unemployment

ggsave(filename = "output/fig_4_unemployment.pdf", 
       width = fig_width, height = fig_height)

# Figure 5: Current account----------------------------------------------------

fig_current_account_abs <- ggplot(macro_data_agg, 
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

fig_current_account_abs <- pretty_up_ggplot(fig_current_account_abs) +
  ggtitle("Current Account in % of GDP (1995-2017)") + 
  scale_y_continuous(
    limits = c(-20, 11),
    breaks = seq(-20, 10, 5),
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_fill_manual(limits = names(unlist(cluster_cols)), 
                    values=c(unlist(cluster_cols)), 
                    aesthetics = c("fill", "color")) +
  scale_x_continuous(limits = c(1995, 2017), 
                     breaks = seq(1995, 2015, 5),
                     expand = expand_scale(c(0, 0), 
                                           c(0, 0.25)
                     )
  ) +
  theme(
    axis.title = element_blank()
  )

fig_current_account_abs

fig_current_account_cum <- ggplot(macro_data_cumulated) + 
  geom_bar(aes(x=reorder(iso3c, CA_cum), 
               y=CA_cum, 
               fill=cluster, color=cluster), 
           stat = "identity") +
  ggtitle("Average Current Account in % of GDP (1995-2017)") + 
  ylab("Average current account") +  
  scale_x_discrete(
    limits=c(
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["Periphery"]]), CA_cum)$iso3c,
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["Primary goods"]]), CA_cum)$iso3c,
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["UK"]]), CA_cum)$iso3c,
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["Catchup"]]), CA_cum)$iso3c,
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["High tech"]]), CA_cum)$iso3c,
      arrange(filter(macro_data_cumulated, 
                     iso3c %in% clustering[["Finance"]]), CA_cum)$iso3c)
  ) +
  scale_y_continuous(
    limits = c(-7.1, 7),
    breaks = seq(-6, 6, 2),
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  scale_fill_manual(limits = names(unlist(cluster_cols)), 
                    values=c(unlist(cluster_cols)), 
                    aesthetics = c("fill", "color")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 7),
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0.2, "cm")
  )
fig_current_account_cum

fig_current_account_full <- ggpubr::ggarrange(
  fig_current_account_abs, fig_current_account_cum, 
  ncol = 2, nrow = 1, common.legend = T, legend = "bottom", 
  labels = c("A)", "B)"), font.label = list(face="bold")
)

ggsave(filename = "output/fig_5_current-account.pdf", 
       width = fig_width*1.5, height = fig_height)



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
make_ineq_barplot <- function(barplot_data, time_period, x_axis_range){
  
  ineq_comparison_plot <- ggplot(barplot_data) +
    geom_bar(aes(x=variable,
                 y=value,
                 color=cluster,
                 fill=cluster),
             stat = "identity", 
             position = "dodge") +
    coord_flip()
  
  ineq_comparison_plot <- pretty_up_ggplot(ineq_comparison_plot, 
                                           type_x_axis = "discrete") + 
    # scale_x_discrete(labels=c("Wage share", "Gini (pre)", "Gini (post)")) +
    scale_y_continuous(limits = x_axis_range, name = "Change in %", 
                       labels = scales::percent_format(scale = 1)) +
    theme(
      axis.title.y = element_blank()
    ) + 
    ggtitle(
      paste0("Changes between ", time_period[1], " and ", time_period[2])
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
get_last_char <- function(a_string, n_char){
  if (as.integer(n_char) != n_char){
    warning("n_char is not an integer, will be coerced!")
  }
  n_char <- as.integer(n_char)
  if (nchar(a_string)<=n_char){
    warning("String shorter of of equal length as n_char. Return full string.")
    return(a_string)
  }
  n_char <- n_char - 1
  last_chars <- substr(a_string, 
                       nchar(a_string)-n_char, nchar(a_string)
  )
  return(last_chars)
}

ineq_data_overall <- macro_data %>%
  filter(year %in% c(1994, 2007, 2008, 2016)) %>%
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
    gini_post_tax_diff_full=gini_post_tax_2016-gini_post_tax_1994,
    gini_post_tax_change_full=gini_post_tax_diff_full/gini_post_tax_1994*100,
    gini_pre_tax_diff_full=gini_pre_tax_2016-gini_pre_tax_1994,
    gini_pre_tax_change_full=gini_pre_tax_diff_full/gini_pre_tax_1994*100,
    wage_share_diff_full=wage_share_2016-wage_share_1994,
    wage_share_change_full=wage_share_diff_full/wage_share_1994*100,
    gini_post_tax_diff_early=gini_post_tax_2007-gini_post_tax_1994,
    gini_post_tax_change_early=gini_post_tax_diff_early/gini_post_tax_1994*100,
    gini_pre_tax_diff_early=gini_pre_tax_2007-gini_pre_tax_1994,
    gini_pre_tax_change_early=gini_pre_tax_diff_early/gini_pre_tax_1994*100,
    wage_share_diff_early=wage_share_2007-wage_share_1994,
    wage_share_change_early=wage_share_diff_early/wage_share_1994*100,
    gini_post_tax_diff_late=gini_post_tax_2016-gini_post_tax_2008,
    gini_post_tax_change_late=gini_post_tax_diff_late/gini_post_tax_2008*100,
    gini_pre_tax_diff_late=gini_pre_tax_2016-gini_pre_tax_2008,
    gini_pre_tax_change_late=gini_pre_tax_diff_late/gini_pre_tax_2008*100,
    wage_share_diff_late=wage_share_2016-wage_share_2008,
    wage_share_change_late=wage_share_diff_late/wage_share_2008*100
  ) %>%
  select(
    one_of("cluster", "gini_post_tax_change_full", 
           "gini_pre_tax_change_full", "wage_share_change_full",
           "gini_post_tax_change_early", 
           "gini_pre_tax_change_early", "wage_share_change_early",
           "gini_post_tax_change_late", 
           "gini_pre_tax_change_late", "wage_share_change_late")
    ) %>%
  gather(variable, value, -cluster)
ineq_data_overall

x_barplot_range <- c(-20, 20)

ineq_plot_overall <- make_ineq_barplot(
  filter(ineq_data_overall, 
         get_last_char(variable, 4)=="full"), 
  c(1994, 2016),
  x_barplot_range) + 
  scale_x_discrete(labels=c("Gini (post)", "Gini (pre)", "Wage share")) 
ineq_plot_overall 

ineq_plot_early <- make_ineq_barplot(
  filter(ineq_data_overall, 
         get_last_char(variable, 5)=="early"), 
  c(1994, 2007),
  x_barplot_range) + 
  scale_x_discrete(labels=c("Gini (post)", "Gini (pre)", "Wage share"))
ineq_plot_early

ineq_plot_late <- make_ineq_barplot(
  filter(ineq_data_overall, 
         get_last_char(variable, 4)=="late"), 
  c(2008, 2016),
  c(-4, 4)) + 
  scale_x_discrete(labels=c("Gini (post)", "Gini (pre)", "Wage share"))
ineq_plot_late

full_ineq_plot <- ggpubr::ggarrange(
  ineq_plot_overall, ineq_plot_early, ineq_plot_late, 
  ncol = 3, legend = "bottom", common.legend = TRUE
)

ggsave(filename = "output/fig_6_inquality-changes.pdf",
       plot = full_ineq_plot, 
       height = fig_height, width = 2*fig_width)

# new alternative--------------------------------------------------------------
gini_x_range <- c(1994, 2016)
ineq_dynamics_post <- ggplot(macro_data_agg, 
                             aes(x=year,
                                 y=gini_post_tax_fn1,
                                 color=cluster)
  ) + 
  geom_ribbon(
    aes(ymin = gini_post_tax_fn1 - 0.5*gini_post_tax_fn2, 
        ymax = gini_post_tax_fn1 + 0.5*gini_post_tax_fn2,
        fill=cluster), 
    alpha=0.5, color=NA
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
    expand = expand_scale(mult = c(0, 0), add = c(0, 0.5))) + 
  scale_color_icae(palette = "mixed") +
  scale_fill_icae(palette = "mixed")

ineq_dynamics_post


ineq_dynamics_pre <- ggplot(macro_data_agg, 
                             aes(x=year,
                                 y=gini_pre_tax_fn1,
                                 color=cluster)
) + 
  geom_ribbon(
    aes(ymin = gini_pre_tax_fn1 - 0.5*gini_pre_tax_fn2, 
        ymax = gini_pre_tax_fn1 + 0.5*gini_pre_tax_fn2,
        fill=cluster), 
    alpha=0.5, color=NA
  ) +
  geom_line() + 
  geom_point() + 
  scale_color_icae(palette = "mixed") +
  scale_fill_icae(palette = "mixed")

ineq_dynamics_pre <- pretty_up_ggplot(ineq_dynamics_pre) +
  ggtitle("Income inequality (Gini pre tax)") + 
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1, scale = 1)
  ) +
  theme(
    axis.title = element_blank()
  ) + 
  scale_x_continuous(
    limits = gini_x_range,
    breaks = seq(1994, 2016, 2), 
    expand = expand_scale(mult = c(0, 0), add = c(0, 0.5))) + 
  scale_color_icae(palette = "mixed") +
  scale_fill_icae(palette = "mixed")
ineq_dynamics_pre



full_ineq_dynamics_plot_pre <- ggpubr::ggarrange(
  ineq_plot_overall, ineq_dynamics_pre, 
  ncol = 2, legend = "bottom", common.legend = TRUE
)

ggsave(filename = "output/fig_6n_inquality-changes-pre.pdf",
       plot = full_ineq_dynamics_plot_pre, 
       height = fig_height, width = 1.2*fig_width)



full_ineq_dynamics_plot_post <- ggpubr::ggarrange(
  ineq_plot_overall, ineq_dynamics_post, 
  ncol = 2, legend = "bottom", common.legend = TRUE, 
  labels = c("A)", "B)"), 
  font.label = list(face="bold")
  )

ggsave(filename = "output/fig_6n_inquality-changes-post.pdf",
       plot = full_ineq_dynamics_plot_post, 
       height = fig_height, width = 1.2*fig_width)

# Further ideas for foreign trade component of figure 5:-----------------------
# net foreign wealth: raussuchen

# KOF trade de facto
# 
# kof_fin_df
# trade_total_GDP
# trade_exp_GDP
# trade_imp_GDP
foreign_trade_vars <- c(
  "KOF Trade de facto"="kof_trade_df",  
  "KOF Finance de facto"="kof_fin_df", 
  "Trade to GDP"="trade_total_GDP", 
  "Exports to GDP"="trade_exp_GDP", 
  "Imports to GDP"="trade_imp_GDP"
  )
make_ft_plot <- function(var_name, data_used){
  var_name_mean <- paste0(var_name, "_fn1")
  var_name_sd <- paste0(var_name, "_fn2")
  ft_plot <- ggplot(data_used, 
                             aes(x=year,
                                 y=UQ(as.name(var_name_mean)),
                                 color=cluster)
  ) + 
    geom_ribbon(
      aes(ymin = UQ(as.name(var_name_mean)) - 0.5*UQ(as.name(var_name_sd)), 
          ymax = UQ(as.name(var_name_mean)) + 0.5*UQ(as.name(var_name_sd)),
          fill=cluster), 
      alpha=0.5, color=NA
    ) +
    geom_line() + 
    geom_point() + 
    scale_fill_icae(palette = "mixed") + scale_color_icae(palette = "mixed") +
    ggtitle(names(var_name)) +
    theme_bw() + theme(legend.position = "bottom", panel.border = element_blank(), axis.line = element_line())
  return(ft_plot)
}
ft_plots <- list()
for (va in 1:length(foreign_trade_vars)){
  print(names(foreign_trade_vars)[va])
  ft_plots[[names(foreign_trade_vars)[va]]] <- make_ft_plot(foreign_trade_vars[[va]], macro_data_agg)
}
ft_plots_full <- ggpubr::ggarrange(plotlist = ft_plots, ncol = 3, nrow = 2, legend = "bottom", common.legend = T, labels = "AUTO")
ggsave(plot = ft_plots_full, filename = "output/foreign-trade-figs.pdf", width = fig_width*1.75, height = fig_height*1.5)

# Without LUX
ft_plots <- list()
for (va in 1:length(foreign_trade_vars)){
  print(names(foreign_trade_vars)[va])
  ft_plots[[names(foreign_trade_vars)[va]]] <- make_ft_plot(foreign_trade_vars[[va]], filter(macro_data_agg, cluster != "Finance"))
}
ft_plots_full <- ggpubr::ggarrange(plotlist = ft_plots, ncol = 3, nrow = 2, legend = "bottom", common.legend = T, labels = "AUTO")
ggsave(plot = ft_plots_full, filename = "output/foreign-trade-figs-noLux.pdf", width = fig_width*1.75, height = fig_height*1.5)




ca_cum_plot <- ggplot(macro_data_cumulated) + 
  geom_bar(aes(x=reorder(iso3c, CA_cum), y=CA_cum, fill=cluster, color=cluster), stat = "identity") +
  ggtitle("Mean current account") + 
  scale_y_continuous() + ylab("Average current account") +  
  scale_x_discrete(
    limits=c(
      arrange(filter(ca_cum, iso3c %in% clustering[["Periphery"]]), CA_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Primary goods"]]), CA_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Catchup"]]), CA_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["UK"]]), CA_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["High tech"]]), CA_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Finance"]]), CA_cum)$iso3c)
    ) +
  scale_fill_manual(limits = names(unlist(cluster_cols)), 
                    values=c(unlist(cluster_cols)), 
                    aesthetics = c("fill", "color")) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0.25, "cm")
    )
ca_cum_plot
ggsave("output/cumCA.pdf", 
       height = 6, width = 9)

growth_cum_plot <- ggplot(ca_cum) + 
  geom_bar(aes(x=reorder(iso3c, GDPpc_growth_cum), y=GDPpc_growth_cum, fill=cluster, color=cluster), stat = "identity") +
  scale_fill_icae(palette="mixed") + scale_color_icae(palette="mixed") +
  ggtitle("Mean GDP per capita growth") + 
  scale_x_discrete(
    limits=c(
      arrange(filter(ca_cum, iso3c %in% clustering[["Periphery"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Finance"]]), GDPpc_growth_cum)$iso3c,
    arrange(filter(ca_cum, iso3c %in% clustering[["UK"]]), GDPpc_growth_cum)$iso3c,
    arrange(filter(ca_cum, iso3c %in% clustering[["High tech"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Catchup"]]), GDPpc_growth_cum)$iso3c,
    arrange(filter(ca_cum, iso3c %in% clustering[["Primary goods"]]), GDPpc_growth_cum)$iso3c)
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0.25, "cm")
  )
growth_cum_plot
ggsave("output/cumGrowth.pdf", 
       height = 6, width = 9)

unemp_cum_plot <- ggplot(ca_cum) + 
  geom_bar(aes(x=reorder(iso3c, unemp_rates), y=unemp_rates, fill=cluster, color=cluster), stat = "identity") +
  scale_fill_icae(palette="mixed") + scale_color_icae(palette="mixed") +
  ggtitle("Mean unemployment rate") + 
  scale_x_discrete(
    limits=c(
      arrange(filter(ca_cum, iso3c %in% clustering[["Periphery"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Finance"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["UK"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["High tech"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Catchup"]]), GDPpc_growth_cum)$iso3c,
      arrange(filter(ca_cum, iso3c %in% clustering[["Primary goods"]]), GDPpc_growth_cum)$iso3c)
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title.x = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.spacing.x = unit(0.25, "cm")
  )
unemp_cum_plot
ggsave("output/cumUnempl.pdf", 
       height = 6, width = 9)

