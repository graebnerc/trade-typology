rm(list = ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(haven)
library(factoextra)
library(NbClust)
library(cluster)
library(icaeDesign)

# Function definitions=========================================================
source("src/clustering_functions.R")

# Data preparation=============================================================
source("src/clustering_data.R")

cluster_data <- cluster_data_R_v4

fwrite(cluster_data, "data/clustering_data_used.csv")

# Cluster implementation=======================================================

variables_clustering <- list(
  "endownments" = c(
    "empl_agr", "VA_manufct_gdp", "oil_exports_share", 
    "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "empl_ind", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "average_wages", 
    "adjusted_wage_share"
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", 
    "kof_econ_dejure" 
  )
)

variables_clustering <- unlist(variables_clustering)

n_groups <- 6

clustering_result <- save_dendogram(
  clustering_variables = variables_clustering, 
  number_groups = n_groups
  )

# Comparison of cluster algorithms=============================================

cluster_comparison <- compare_clustering_types(
  raw_dat = cluster_data, 
  clustering_vars = variables_clustering,
  n_groups)

write(
  print(
    xtable::xtable(cluster_comparison),
    type = "html"
  ), 
  file = "output/table_2_cluster_algorithms_comparison.html"
)

# Illustration of group differences============================================

data_taxonomy <- cluster_data

vars_all <- c(rel_vars, nrel_vars)
sort(vars_all)

clustering <- list(
  "Cluster_1" = c("Latvia", "Estonia"),
  "Cluster_2" = c("Slovenia", "Poland","Slovakia","Hungary", "Czech Republic", "Czechia"),
  "Cluster_3" = c("United Kingdom"),
  "Cluster_4" = c("Luxembourg"),
  "Cluster_5" = c("Greece", "Portugal", "Spain", "Italy", "France"),
  "Cluster_6" = c("Sweden", "Finland", "Denmark", "Netherlands", 
                  "Belgium", "Germany", "Austria", "Ireland")
)


data_taxonomy <- data_taxonomy %>%
  dplyr::mutate(
    cluster=ifelse(
      country %in% clustering[["Cluster_1"]], "C1", 
      ifelse(country %in% clustering[["Cluster_2"]], "C2", 
             ifelse(country %in% clustering[["Cluster_3"]], "C3", 
                    ifelse(country %in% clustering[["Cluster_4"]], "C4", 
                           ifelse(country %in% clustering[["Cluster_5"]], "C5",
                                  ifelse(country %in% clustering[["Cluster_6"]], "C6",
                                  NA))))))
    )

# Taxonomy table---------------------------------------------------------------
table_order <- c("empl_agr",
                 "oil_exports_share",
                 "primary_exports_share_1",
                 "res_rents",
                 "VA_manufct_gdp",
                 
                 "complexity_harv",
                 "empl_ind", 
                 "gerd",
                 "ict_ksh",
                 "gov_exp_educ",
                 
                 "coord",
                 "employment_protect",
                 "ubr", 
                 "average_wages",
                 "adjusted_wage_share",
                 
                 "tax_corpcap",
                 "tax_estate_plus_wealth",
                 "fdi_to_gdp",
                 "size_of_finance",
                 "kof_econ_dejure")

taxonomy_table_data <- data_taxonomy %>%
  dplyr::mutate(average_wages=average_wages/1000,
                industrial_to_gdp=industrial_to_gdp*100) %>%
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
  file = "output/table_3_taxonomy_table.html"
)
