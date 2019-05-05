rm(list = ls())
library(countrycode)
library(data.table)
library(tidyverse)
library(haven)
library(factoextra)
library(cluster)
# TODO Descriptive descriptions of the various clusters for section 4, just as in JEE
# Function definitions=========================================================
source("src/clustering_functions.R")

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
# Process data: Select countries-----------------------------------------------
drop_countries <- c("Canada", "Mexico", "New Zealand", "Turkey", "Switzerland",
                    "United States", "Iceland", "Australia", "Korea", "Norway",
                    "Japan", "Bulgaria", "Slovak Republic", "Lithuania")
drop_countries <- countrycode::countrycode(drop_countries, 
                                           "country.name", "un")

cluster_data_DTA_v2 <- cluster_data_DTA %>%
  dplyr::filter(!un_ccode %in% drop_countries,
                !is.na(un_ccode))

# Make means-------------------------------------------------------------------
# hier werden die mittelwerte genommen
cluster_data_DTA_v3_means <- cluster_data_DTA_v2 %>%
  dplyr::filter(year>=1994) %>%
  dplyr::select(-one_of("v1", "x", "year")) %>%
  dplyr::group_by(un_ccode) %>%
  dplyr::summarise_all(mean, na.rm=T) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country=countrycode(un_ccode, "un", "country.name"))

# Process data: z standartization----------------------------------------------

cluster_data_DTA_v3_means_normed <- cluster_data_DTA_v3_means %>%
  dplyr::mutate(
    zkof_econ_defacto = scale(kof_econ_defacto)[,1], # egen zkof_econ_defacto =std( kof_econ_defacto)
    zgov_exp_to_gdp = scale(gov_exp_to_gdp)[,1], # egen zgov_exp_to_gdp =std( gov_exp_to_gdp)
    ztax_total = scale(tax_total)[,1], # egen ztax_total =std(tax_total)
    zcomplexity_harv = scale(complexity_harv)[,1], # egen zcomplexity_harv =std( complexity_harv)
    zindustrial_to_gdp = scale(industrial_to_gdp)[,1], # egen zindustrial_to_gdp =std( industrial_to_gdp)
    zgerd = scale(gerd)[,1], # egen zgerd =std(gerd)
    zict_ksh = scale(ict_ksh)[,1], # egen zict_ksh =std( ict_ksh)
    zgov_exp_educ = scale(gov_exp_educ)[,1], # egen zgov_exp_educ =std( gov_exp_educ )
    zadjusted_wage_share = scale(adjusted_wage_share)[,1], # egen zadjusted_wage_share =std( adjusted_wage_share )
    zemployment_protect = scale(employment_protect)[,1], # egen zemployment_protect =std( employment_protect )
    zubr = scale(ubr)[,1], # egen zubr =std( ubr )
    zudens = scale(udens), # egen zudens =std( udens )
    zgini_market = scale(gini_market)[,1], # egen zgini_market =std(gini_market )
    ztax_ssc_employer = scale(tax_ssc_employer)[,1], # egen ztax_ssc_employer =std( tax_ssc_employer )
    ztax_corpcap = scale(tax_corpcap)[,1], # egen ztax_corpcap =std( tax_corpcap )
    ztax_estate_plus_wealth = scale(tax_estate_plus_wealth)[,1], # egen ztax_estate_plus_wealth =std( tax_estate_plus_wealth )
    zfdi_to_gdp = scale(fdi_to_gdp)[,1], # egen zfdi_to_gdp =std( fdi_to_gdp )
    zsize_of_finance = scale(size_of_finance)[,1], # egen zsize_of_finance =std( size_of_finance )
    zkof_econ_dejure = scale(kof_econ_dejure)[,1], # egen zkof_econ_dejure =std( kof_econ_dejure )
    zoil_exports_share = scale(oil_exports_share)[,1], # egen zoil_exports_share=std( oil_exports_share)
    zprimary_exports_share_1 = scale(primary_exports_share_1)[,1], # egen zprimary_exports_share_1=std( primary_exports_share_1)
    zexp_to_gdp = scale(exp_to_gdp)[,1], # egen zexp_to_gdp=std( exp_to_gdp)
    zres_rents = scale(res_rents)[,1], # egen zres_rents =std( res_rents )
    zcoal_metal_export_share = scale(coal_metal_export_share)[,1], # egen zcoal_metal_export_share =std( coal_metal_export_share )
    zgov_exp_socprtc = scale(gov_exp_socprtc)[,1], # egen zgov_exp_socprtc =std(gov_exp_socprtc )
    zcoord = scale(coord)[,1], # egen zcoord=std(coord)
    zadjcov = scale(adjcov)[,1], # egen zadjcov=std(adjcov)
    ztax_income = scale(tax_income)[,1], # egen ztax_income =std(tax_income)
    ztax_rev_to_gdp = scale(tax_rev_to_gdp)[,1] # egen ztax_rev_to_gdp =std(tax_rev_to_gdp)
  )

# should be the same as:
cluster_data_DTA_normed1994 <- haven::read_dta("data/v34_cluster_mean_1994.dta")

# Cluster implementation=======================================================

# cluster_data_DTA_v3_means_normed
# cluster_data_DTA_normed1994

n_groups <- 5

cluster_vars <- c("zkof_econ_defacto", "zcoal_metal_export_share", 
                  "zoil_exports_share", "zprimary_exports_share_1", 
                  "zres_rents", "zcomplexity_harv", "zindustrial_to_gdp", 
                  "zgerd", "zict_ksh", "zgov_exp_educ", "zcoord", 
                  "zemployment_protect", "zubr", "zgov_exp_socprtc", 
                  "zgini_market", "ztax_corpcap", "ztax_estate_plus_wealth", 
                  "zfdi_to_gdp", "zsize_of_finance", "zkof_econ_dejure")

replication_dta <- do_clustering(
  cluster_data_DTA_normed1994, 
  cluster_vars, 
  n_groups, clustering_method = "single")

replication_r <- do_clustering(
  cluster_data_DTA_v3_means_normed, 
  cluster_vars, 
  n_groups)
# TODO Die beiden sind unterschiedlich, aber es ist nicht klar warum. Scheint mit unterschieldicher Skalierung zu tun haben...
replication_dennis_plot_dta <-  replication_dta$cluster_plot + 
  ggtitle("Clustering Ergebnis in R (dta file)") +
  xlab("Länder") + ylab("")
replication_dennis_plot_dta

replication_dennis_plot_R <-  replication_r$cluster_plot + 
  ggtitle("Clustering Ergebnis in R (Werte in R normalisiert)") +
  xlab("Länder") + ylab("")
replication_dennis_plot_R

replication_full <- ggpubr::ggarrange(
  replication_dennis_plot_dta, replication_dennis_plot_R,
  nrow = 1, ncol = 2
)

ggplot2::ggsave(plot = replication_full,
                filename = "output/clustering_R.pdf", 
                width = 12, height = 6)

# Comparison of cluster algorithms=============================================

# The clustering based on the original dta file--------------------------------
cluster_comparison_dta <- compare_clustering_types(
  raw_dat = cluster_data_DTA_normed1994, 
  clustering_vars = cluster_vars)

write(
  print(
    xtable::xtable(cluster_comparison_dta),
    type = "html"
  ), 
  file = "output/cluster_comparison_dta.html"
)

# The clustering based on the re-created data set------------------------------
cluster_comparison_r <- compare_clustering_types(
  raw_dat = cluster_data_DTA_v3_means_normed, 
  clustering_vars = cluster_vars)

write(
  print(
    xtable::xtable(cluster_comparison_r),
    type = "html"
  ), 
  file = "output/cluster_comparison_r.html"
)

# Visualization of group differences===========================================

# Cluster 1: primary goods (Latvia and Estonia)
#  rents of natural resources are high (aber: vielleicht nur wegen niedrigem GDP?)
#  share of oil exports is high
#  complexity is low
#  low investments in research and development
#  The government expenditure on education is surprisingly high
#  low degree of wage coordination and low government expenditures on social security.
#  very low corporate, estate and all other wealth tax revenues are also remarkable
#  FDI as in high road

# Cluster 2: finance (Luxembourg)
#  vast size of its financial sector (the share of the financial sector)
#  share of foreign direct investments 
#  lowest share of primary goods in exports
#  weak regulation boosting 
#  highest degree of economic globalisation in terms of de facto openness as well as in terms of de jure openness. 
#  high corporate tax revenues, and high benefits in the case of unemployment.

# Cluster 3: deregulated regime (United Kingdom, Ireland and Hungary)
#  deregulated labour market and high income inequality. 
#  43.5 percent of their former net income in case of unemployment and the employment protection (mean value of 1.5) is also very low. 
#  low government expenditures on social security
#  low tax revenues can be described as a strategy aiming to improve price competitiveness.

# Cluster 4: less globalised model (Poland, Greece, Portugal, Slovakia, Spain, Italia, Slovenia and Czech Republic)
#  exact counterpart to cluster 2 (see Table 2)
#  low degree of international integration as reflected by low values in foreign direct investments and the economic globalisation index. 
#  government expenditures on education and investments in research and development are considerably below the other trade regimes. 
#  relatively strict labor market regulation.

# Cluster 5: Technological leaders (France, Sweden, Finland, Denmark, Netherlands, Belgium, Germany and Austria)
#  highest degree of economic complexity
#  high degree of wage coordination and high government expenditures on social security.
#  high investments in research and development

