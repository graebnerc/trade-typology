rm(list = ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(haven)
library(factoextra)
library(NbClust)
library(cluster)
# TODO Descriptive descriptions of the various clusters for section 4, just as in JEE
# Function definitions=========================================================
source("src/clustering_functions.R")

# Data preparation=============================================================
source("src/clustering_data.R")

cluster_data <- cluster_data_R_v4

# Cluster implementation=======================================================

n_groups <- 5

clustering_list <- do_clustering(
  cluster_data, # hier muesste R hin alt: cluster_data_DTA_v3_means_normed
  cluster_vars, 
  n_groups)

clustering_dendogram <-  clustering_list$cluster_plot + 
  ggtitle("Clustering Ergebnis (in R gebaute Daten)") +
  xlab("Länder") + ylab("")
clustering_dendogram

ggplot2::ggsave(plot = clustering_dendogram,
                filename = "output/clustering.pdf", 
                width = 8, height = 6)

# Comparison of cluster algorithms=============================================

cluster_comparison_r <- compare_clustering_types(
  raw_dat = cluster_data, 
  clustering_vars = cluster_vars)

write(
  print(
    xtable::xtable(cluster_comparison_r),
    type = "html"
  ), 
  file = "output/cluster_algorithms_comparison.html"
)

# Test for number of clusters==================================================
source("src/nb_clusters.R")

# Illustration of group differences============================================

data_taxonomy <- cluster_data

vars_all <- c(rel_vars, nrel_vars)
sort(vars_all)

clustering <- list(
  "Cluster_1" = c("Latvia", "Estonia"),
  "Cluster_2" = c("Luxembourg"),
  "Cluster_3" = c("United Kingdom", "Ireland", "Hungary"),
  "Cluster_4" = c("Poland", "Greece", "Portugal", "Slovakia", "Spain", 
                  "Italy", "Slovenia", "Czech Republic"),
  "Cluster_5" = c("France", "Sweden", "Finland", "Denmark", "Netherlands", 
                  "Belgium", "Germany", "Austria")
)

# Cluster 1: primary goods (Latvia and Estonia)
#  rents of natural resources are high (aber: vielleicht nur wegen niedrigem GDP?)
#  share of oil exports is high
#  complexity is low
#  low investments in research and development
#  The government expenditure on education is surprisingly high
#  low degree of wage coordination and low government expenditures on social security.
#  very low corporate, estate and all other wealth tax revenues are also remarkable
#  FDI as in high road
cluster_1_vars <- c(
  "res_rents", "oil_exports_share", "primary_exports_share_1",
                    "complexity_harv", "gerd", "gov_exp_educ", "industrial_to_gdp",
                    "coord", "gov_exp_socprtc",
                    "tax_corpcap", "tax_estate_plus_wealth", "size_of_finance"
  )

# Cluster 2: finance (Luxembourg)
#  vast size of its financial sector (the share of the financial sector)
#  share of foreign direct investments 
#  lowest share of primary goods in exports
#  weak regulation  
#  highest degree of economic globalisation in terms of de facto openness as well as in terms of de jure openness. 
#  high corporate tax revenues, and high benefits in the case of unemployment.
cluster_2_vars <- c(
  "kof_econ_defacto", "coal_metal_export_share", "oil_exports_share", 
  "primary_exports_share_1", "res_rents", # TODO Share of coals and metals raus?
  "ict_ksh", "ubr", "gini_market", 
  "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", 
  "size_of_finance", "kof_econ_dejure"
  )

# Cluster 3: deregulated regime (United Kingdom, Ireland and Hungary)
#  deregulated labour market and high income inequality. 
#  43.5 percent of their former net income in case of unemployment and the employment protection (mean value of 1.5) is also very low. 
#  low government expenditures on social security
#  low tax revenues can be described as a strategy aiming to improve price competitiveness.
cluster_3_vars <- c(
  "coal_metal_export_share", 
  "ict_ksh",
  "employment_protect", "gini_market", "ubr"
  )

# Cluster 4: less globalised model (Poland, Greece, Portugal, Slovakia, Spain, Italia, Slovenia and Czech Republic)
#  exact counterpart to cluster 2 (see Table 2)
#  low degree of international integration as reflected by low values in foreign direct investments and the economic globalisation index. 
#  government expenditures on education and investments in research and development are considerably below the other trade regimes. 
#  relatively strict labor market regulation.
cluster_4_vars <- c(
 "kof_econ_defacto", 
 "gov_exp_educ",
 "employment_protect",
 "kof_econ_dejure", "fdi_to_gdp"
 )

# Cluster 5: Technological leaders (France, Sweden, Finland, Denmark, Netherlands, Belgium, Germany and Austria)
#  highest degree of economic complexity
#  high degree of wage coordination and high government expenditures on social security.
#  high investments in research and development
cluster_5_vars <- c(
  "complexity_harv", "gerd", "industrial_to_gdp", 
  "coord", "gov_exp_socprtc"
  )

clustering_vars <- list(
  "C1" = cluster_1_vars,
  "C2" = cluster_2_vars,
  "C3" = cluster_3_vars,
  "C4" = cluster_4_vars,
  "C5" = cluster_5_vars
)

data_taxonomy <- data_taxonomy %>%
  dplyr::mutate(
    cluster=ifelse(
      country %in% clustering[["Cluster_1"]], "C1", 
      ifelse(country %in% clustering[["Cluster_2"]], "C2", 
             ifelse(country %in% clustering[["Cluster_3"]], "C3", 
                    ifelse(country %in% clustering[["Cluster_4"]], "C4", 
                           ifelse(country %in% clustering[["Cluster_5"]], "C5",
                                  NA)))))
    )

# Taxonomy table---------------------------------------------------------------
table_order <- c("kof_econ_defacto", 
                 "coal_metal_export_share",
                 "oil_exports_share",
                 "primary_exports_share_1",
                 "res_rents",
                 "complexity_harv",
                 "industrial_to_gdp",
                 "gerd",
                 "ict_ksh",
                 "gov_exp_educ",
                 "coord",
                 "employment_protect",
                 "ubr", 
                 "gov_exp_socprtc",
                 "gini_market",
                 "tax_corpcap",
                 "tax_estate_plus_wealth",
                 "fdi_to_gdp",
                 "size_of_finance",
                 "kof_econ_dejure")

taxonomy_table_data <- data_taxonomy %>%
  dplyr::select(-country) %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise_all(mean) %>%
  dplyr::ungroup() %>%
  tidyr::gather(variable, value, -cluster) %>%
  tidyr::spread(cluster, value) %>%
  dplyr::slice(match(table_order, variable))
head(taxonomy_table_data)

write(
  print(
    xtable::xtable(taxonomy_table_data),
    type = "html"
  ), 
  file = "output/taxonomy_table.html"
)


# Taxonomy plots---------------------------------------------------------------
for (i in 1:5){
  cluster_considered <- paste0("C", i)
  print(cluster_considered)
  current_plots <- make_plots(data_taxonomy, cluster_considered, clustering_vars)
  ggsave(plot = current_plots, 
         filename = paste0("output/", cluster_considered, "_barplot.pdf"), 
         width = length(current_plots)*2.5, height = 8)
}
