rm(list = ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(haven)
library(factoextra)
library(cluster)


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
  dplyr::filter(year>=1994) %>%
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

cluster_data_DTA_v3_normed <- cluster_data_DTA_v3 %>%
  dplyr::mutate(
    zkof_econ_defacto = scale(kof_econ_defacto), # egen zkof_econ_defacto =std( kof_econ_defacto)
    zgov_exp_to_gdp = scale(gov_exp_to_gdp), # egen zgov_exp_to_gdp =std( gov_exp_to_gdp)
    ztax_total = scale(tax_total), # egen ztax_total =std(tax_total)
    zcomplexity_harv = scale(complexity_harv), # egen zcomplexity_harv =std( complexity_harv)
    zindustrial_to_gdp = scale(industrial_to_gdp), # egen zindustrial_to_gdp =std( industrial_to_gdp)
    zgerd = scale(gerd), # egen zgerd =std(gerd)
    zict_ksh = scale(ict_ksh), # egen zict_ksh =std( ict_ksh)
    zgov_exp_educ = scale(gov_exp_educ), # egen zgov_exp_educ =std( gov_exp_educ )
    zadjusted_wage_share = scale(adjusted_wage_share), # egen zadjusted_wage_share =std( adjusted_wage_share )
    zemployment_protect = scale(employment_protect), # egen zemployment_protect =std( employment_protect )
    zubr = scale(ubr), # egen zubr =std( ubr )
    zudens = scale(udens), # egen zudens =std( udens )
    zgini_market = scale(gini_market), # egen zgini_market =std(gini_market )
    ztax_ssc_employer = scale(tax_ssc_employer), # egen ztax_ssc_employer =std( tax_ssc_employer )
    ztax_corpcap = scale(tax_corpcap), # egen ztax_corpcap =std( tax_corpcap )
    ztax_estate_plus_wealth = scale(tax_estate_plus_wealth), # egen ztax_estate_plus_wealth =std( tax_estate_plus_wealth )
    zfdi_to_gdp = scale(fdi_to_gdp), # egen zfdi_to_gdp =std( fdi_to_gdp )
    zsize_of_finance = scale(size_of_finance), # egen zsize_of_finance =std( size_of_finance )
    zkof_econ_dejure = scale(kof_econ_dejure), # egen zkof_econ_dejure =std( kof_econ_dejure )
    zoil_exports_share = scale(oil_exports_share), # egen zoil_exports_share=std( oil_exports_share)
    zprimary_exports_share_1 = scale(primary_exports_share_1), # egen zprimary_exports_share_1=std( primary_exports_share_1)
    zexp_to_gdp = scale(exp_to_gdp), # egen zexp_to_gdp=std( exp_to_gdp)
    zres_rents = scale(res_rents), # egen zres_rents =std( res_rents )
    zcoal_metal_export_share = scale(coal_metal_export_share), # egen zcoal_metal_export_share =std( coal_metal_export_share )
    zgov_exp_socprtc = scale(gov_exp_socprtc), # egen zgov_exp_socprtc =std(gov_exp_socprtc )
    zcoord = scale(coord), # egen zcoord=std(coord)
    zadjcov = scale(adjcov), # egen zadjcov=std(adjcov)
    ztax_income = scale(tax_income), # egen ztax_income =std(tax_income)
    ztax_rev_to_gdp = scale(tax_rev_to_gdp) # egen ztax_rev_to_gdp =std(tax_rev_to_gdp)
  )

# should be the same as:
cluster_data_DTA_normed1994 <- haven::read_dta("data/v34_cluster_mean_1994.dta")

# Cluster implementation=======================================================
n_groups <- 5

cluster_vars <- c("zkof_econ_defacto", "zcoal_metal_export_share", 
                  "zoil_exports_share", "zprimary_exports_share_1", 
                  "zres_rents", "zcomplexity_harv", "zindustrial_to_gdp", 
                  "zgerd", "zict_ksh", "zgov_exp_educ", "zcoord", 
                  "zemployment_protect", "zubr", "zgov_exp_socprtc", 
                  "zgini_market", "ztax_corpcap", "ztax_estate_plus_wealth", 
                  "zfdi_to_gdp", "zsize_of_finance", "zkof_econ_dejure")

#' Clustering
#' 
#' Conducts the clustering
#' @param data_file The file for the clustering, must not contain NA
#' @param clustering_vars Should contain all the variables to be used
#'   in the clustering as strings
#' @param nb_groups The number of groups to be highlighted in the plot
#' @return List with clustering object, the data used, and the plot.
do_clustering <- function(data_file, clustering_vars, nb_groups){
  cluster_data <- data_file %>%
    dplyr::select(one_of("country", clustering_vars)) %>%
    dplyr::mutate(country=countrycode(country, 
                                      "country.name", "country.name.de"))
  
  cluster_data <- as.data.frame(cluster_data)
  rownames(cluster_data) <- cluster_data$country
  
  cluster_data <- select(cluster_data, -country)
  clustering_object <- cluster_data %>%
    cluster::agnes(method = "ward") # Compute hierachical clustering
  
  
  cluster_plot <- factoextra::fviz_dend(clustering_object, 
                                        k = nb_groups, 
                                        cex = 0.75, # label size
                                        rect = TRUE, # Add rectangle around groups
                                        rect_fill = TRUE,
                                        color_labels_by_k = TRUE, # color labels by groups
                                        k_colors = RColorBrewer::brewer.pal(
                                          n_groups, "Dark2"),
                                        rect_border = RColorBrewer::brewer.pal(
                                          n_groups, "Dark2"),
                                        horiz = TRUE
  ) + theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  return_list <- list(
    "cluster_obj" = clustering_object,
    "cluster_data" = cluster_data,
    "cluster_plot" = cluster_plot
  )
  return(return_list)
  
}

replication_dennis <- do_clustering(cluster_data_DTA_normed1994, 
                                    cluster_vars, 
                                    n_groups)

replication_dennis_plot <-  replication_dennis$cluster_plot + 
  ggtitle("Clustering Ergebnis in R") +
  xlab("Länder") + ylab("")

replication_dennis_plot

# sub_grp <- cutree(as.hclust(clustering_object), k = n_groups)
ggplot2::ggsave(filename = "output/clustering_R.pdf", 
                width = 7, height = 6)

# TODO nochmal genau durchgehen: geht es auch ohne die mittelwerte