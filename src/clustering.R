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
) %>%
  dplyr::select(-exp_to_gdp)
ictwss <- haven::read_dta("data/ictwss_short.dta") %>%
  dplyr::mutate(
    country=countrycode::countrycode(country, "country.name", "iso3c"),
    year=as.double(year),
    adjcov=as.double(adjcov),
    coord=as.double(coord)
  ) 

missing_vars <- c(
  "kof_econ_defacto", "gov_exp_to_gdp", "tax_total", "complexity_harv", 
  "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ",  "tax_rev_to_gdp",
  "adjusted_wage_share", "employment_protect", "ubr", "udens", "gini_market",
  "tax_ssc_employer", "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", 
  "size_of_finance", "kof_econ_dejure", "exp_to_gdp", "res_rents", 
  "gov_exp_socprtc", "tax_income"
)

macro_data <- data.table::fread("data/Macro_data_trade-v34.csv") %>%
  select(one_of("Year", "Country", missing_vars)) %>%
  dplyr::mutate(
    Country=countrycode::countrycode(Country, "country.name", "iso3c")
    )

cluster_data_R_v1 <- dplyr::full_join(ictwss, endownments, 
                                   by=c("country"="location_code", "year")) %>%
  dplyr::left_join(., macro_data, 
                   by=c("country"="Country", "year"="Year")) %>%
  dplyr::mutate(
    un_ccode=countrycode::countrycode(country, "iso3c", "un")
    )

cluster_data_DTA_v1 <- haven::read_dta("data/v34_cluster.dta")

# TODO cluster_data_DTA weist deutlich mehr variablen auf als sich aus dem do file ergibt. Warum?

# Process data=================================================================
# Process data: Select countries-----------------------------------------------
drop_countries <- c("Canada", "Mexico", "New Zealand", "Turkey", "Switzerland",
                    "United States", "Iceland", "Australia", "Korea", "Norway",
                    "Japan", "Bulgaria", "Slovak Republic", "Lithuania")
drop_countries <- countrycode::countrycode(drop_countries, 
                                           "country.name", "un")

cluster_data_DTA_v2 <- cluster_data_DTA_v1 %>%
  dplyr::filter(!un_ccode %in% drop_countries,
                !is.na(un_ccode))

cluster_data_R_v2 <- cluster_data_R_v1 %>%
  dplyr::filter(!un_ccode %in% drop_countries,
                !is.na(un_ccode))

# Make means-------------------------------------------------------------------
# hier werden die mittelwerte genommen
cluster_data_DTA_v3 <- cluster_data_DTA_v2 %>%
  dplyr::filter(year>=1994) %>%
  dplyr::select(-one_of("v1", "x", "year")) %>%
  dplyr::group_by(un_ccode) %>%
  dplyr::summarise_all(mean, na.rm=T) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country=countrycode(un_ccode, "un", "country.name"))

cluster_data_R_v3 <- cluster_data_R_v2 %>%
  dplyr::filter(year>=1994) %>%
  dplyr::select(-one_of("year")) %>%
  dplyr::group_by(un_ccode) %>%
  dplyr::summarise_all(mean, na.rm=T) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country=countrycode(un_ccode, "un", "country.name"))

# Process data: z standartization----------------------------------------------
# TODO exp_to_gdp ist nicht mehr drinnen, ist das nicht komisch?
# TODO maybe remove all nrel_vars?
rel_vars <- c("kof_econ_defacto", "coal_metal_export_share", 
              "oil_exports_share", "primary_exports_share_1", 
              "res_rents", "complexity_harv", "industrial_to_gdp", 
              "gerd", "ict_ksh", "gov_exp_educ", "coord", 
              "employment_protect", "ubr", "gov_exp_socprtc", 
              "gini_market", "tax_corpcap", "tax_estate_plus_wealth", 
              "fdi_to_gdp", "size_of_finance", "kof_econ_dejure"
              ) 
# wird unten nicht mehr verwendet, aber war ursprünglich drinnen:
nrel_vars <- c("exp_to_gdp", "gov_exp_to_gdp", "tax_total", 
               "adjusted_wage_share", "udens", "tax_ssc_employer", 
               "adjcov", "tax_income", "tax_rev_to_gdp")

cluster_data_DTA_v4 <- cluster_data_DTA_v3 %>%
  select(one_of("country", rel_vars, nrel_vars)) %>%
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

cluster_data_R_v4 <- cluster_data_R_v3 %>%
  select(one_of("country", rel_vars, nrel_vars)) %>%
  dplyr::mutate(
    ##zkof_econ_defacto = scale(kof_econ_defacto)[,1], # egen zkof_econ_defacto =std( kof_econ_defacto)
    ##zgov_exp_to_gdp = scale(gov_exp_to_gdp)[,1], # egen zgov_exp_to_gdp =std( gov_exp_to_gdp)
    ##ztax_total = scale(tax_total)[,1], # egen ztax_total =std(tax_total)
    zcomplexity_harv = scale(complexity_harv)[,1], # egen zcomplexity_harv =std( complexity_harv)
    ##zindustrial_to_gdp = scale(industrial_to_gdp)[,1], # egen zindustrial_to_gdp =std( industrial_to_gdp)
    ##zgerd = scale(gerd)[,1], # egen zgerd =std(gerd)
    ###zict_ksh = scale(ict_ksh)[,1], # egen zict_ksh =std( ict_ksh)
    zgov_exp_educ = scale(gov_exp_educ)[,1], # egen zgov_exp_educ =std( gov_exp_educ )
    ##zadjusted_wage_share = scale(adjusted_wage_share)[,1], # egen zadjusted_wage_share =std( adjusted_wage_share )
    zemployment_protect = scale(employment_protect)[,1], # egen zemployment_protect =std( employment_protect )
    ##zubr = scale(ubr)[,1], # egen zubr =std( ubr )
    ##zudens = scale(udens), # egen zudens =std( udens )
    ##zgini_market = scale(gini_market)[,1], # egen zgini_market =std(gini_market )
    ##ztax_ssc_employer = scale(tax_ssc_employer)[,1], # egen ztax_ssc_employer =std( tax_ssc_employer )
    ##ztax_corpcap = scale(tax_corpcap)[,1], # egen ztax_corpcap =std( tax_corpcap )
    ##ztax_estate_plus_wealth = scale(tax_estate_plus_wealth)[,1], # egen ztax_estate_plus_wealth =std( tax_estate_plus_wealth )
    ##zfdi_to_gdp = scale(fdi_to_gdp)[,1], # egen zfdi_to_gdp =std( fdi_to_gdp )
    zsize_of_finance = scale(size_of_finance)[,1], # egen zsize_of_finance =std( size_of_finance )
    ##zkof_econ_dejure = scale(kof_econ_dejure)[,1], # egen zkof_econ_dejure =std( kof_econ_dejure )
    zoil_exports_share = scale(oil_exports_share)[,1], # egen zoil_exports_share=std( oil_exports_share)
    zprimary_exports_share_1 = scale(primary_exports_share_1)[,1], # egen zprimary_exports_share_1=std( primary_exports_share_1)
    zexp_to_gdp = scale(exp_to_gdp)[,1], # egen zexp_to_gdp=std( exp_to_gdp)
    zres_rents = scale(res_rents)[,1], # egen zres_rents =std( res_rents )
    zcoal_metal_export_share = scale(coal_metal_export_share)[,1], # egen zcoal_metal_export_share =std( coal_metal_export_share )
    zgov_exp_socprtc = scale(gov_exp_socprtc)[,1], # egen zgov_exp_socprtc =std(gov_exp_socprtc )
    zcoord = scale(coord)[,1], # egen zcoord=std(coord)
    zadjcov = scale(adjcov)[,1], # egen zadjcov=std(adjcov)
    ##ztax_income = scale(tax_income)[,1], # egen ztax_income =std(tax_income)
    ztax_rev_to_gdp = scale(tax_rev_to_gdp)[,1] # egen ztax_rev_to_gdp =std(tax_rev_to_gdp)
  )

# data overview:
cluster_vars <- paste0("z", rel_vars)

cluster_data_DTA_pre_proc <- haven::read_dta(
  "data/v34_cluster_mean_1994.dta"
  ) %>%
  dplyr::select(one_of("country", rel_vars, cluster_vars))
cluster_data_DTA_post_proc <- cluster_data_DTA_v4
cluster_data_R <- cluster_data_R_v4

# Cluster implementation=======================================================

n_groups <- 5

replication_dta_pre <- do_clustering(
  cluster_data_DTA_pre_proc, 
  cluster_vars, 
  n_groups, clustering_method = "ward")

replication_dta_post <- do_clustering(
  cluster_data_DTA_post_proc, 
  cluster_vars, 
  n_groups, clustering_method = "ward")

replication_r <- do_clustering(
  cluster_data_R, # hier muesste R hin alt: cluster_data_DTA_v3_means_normed
  cluster_vars, 
  n_groups)


# TODO Die beiden sind unterschiedlich, aber es ist nicht klar warum. Scheint mit unterschieldicher Skalierung zu tun haben...
replication_dennis_plot_dta_pre <-  replication_dta_pre$cluster_plot + 
  ggtitle("Clustering Ergebnis (dta, Dennis' Aufbereitung") +
  xlab("Länder") + ylab("")
replication_dennis_plot_dta_pre

replication_dennis_plot_dta_pre <- replication_dta_post$cluster_plot + 
  ggtitle("Clustering Ergebnis (dta, Dennis' Aufbereitung") +
  xlab("Länder") + ylab("")
replication_dennis_plot_dta_pre

replication_dennis_plot_R <-  replication_r$cluster_plot + 
  ggtitle("Clustering Ergebnis (in R gebaute Daten)") +
  xlab("Länder") + ylab("")
replication_dennis_plot_R

replication_full <- ggpubr::ggarrange(
  replication_dennis_plot_dta_pre, replication_dennis_plot_dta_pre,
  replication_dennis_plot_R,
  nrow = 1, ncol = 3
)

ggplot2::ggsave(plot = replication_full,
                filename = "output/clustering_R.pdf", 
                width = 16, height = 6)

# Comparison of cluster algorithms=============================================

# The clustering based on dta data and processing in R-------------------------
cluster_comparison_dta_post <- compare_clustering_types(
  raw_dat = cluster_data_DTA_post_proc, 
  clustering_vars = cluster_vars)

write(
  print(
    xtable::xtable(cluster_comparison_dta_post),
    type = "html"
  ), 
  file = "output/cluster_comparison_dta_post.html"
)

# The clustering based on processed dta data-----------------------------------
cluster_comparison_dta_pre <- compare_clustering_types(
  raw_dat = cluster_data_DTA_pre_proc, 
  clustering_vars = cluster_vars)

write(
  print(
    xtable::xtable(cluster_comparison_dta_pre),
    type = "html"
  ), 
  file = "output/cluster_comparison_dta_pre.html"
)

# The clustering based on the re-created data set------------------------------
cluster_comparison_r <- compare_clustering_types(
  raw_dat = cluster_data_R, # hier muesste R hin alt: cluster_data_DTA_v3_means_normed
  clustering_vars = cluster_vars)

write(
  print(
    xtable::xtable(cluster_comparison_r),
    type = "html"
  ), 
  file = "output/cluster_comparison_r.html"
)

# Visualization of group differences===========================================

head(cluster_data_DTA_pre_proc) # DTA data (pre-processed)
head(cluster_data_DTA_post_proc) # DTA data (post-processed)
head(cluster_data_R) # R data 
data_taxonomy <- cluster_data_DTA_pre_proc

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
cluster_1_vars <- c("res_rents", "oil_exports_share", "complexity_harv", 
                    "gerd", "gov_exp_educ", "coord", "tax_corpcap", 
                    "tax_estate_plus_wealth", "fdi_to_gdp")

# Cluster 2: finance (Luxembourg)
#  vast size of its financial sector (the share of the financial sector)
#  share of foreign direct investments 
#  lowest share of primary goods in exports
#  weak regulation  
#  highest degree of economic globalisation in terms of de facto openness as well as in terms of de jure openness. 
#  high corporate tax revenues, and high benefits in the case of unemployment.
cluster_2_vars <- c("size_of_finance", "fdi_to_gdp", "primary_exports_share_1",
                    "employment_protect", "kof_econ_defacto", 
                    "kof_econ_dejure", "tax_corpcap", "ubr")

# Cluster 3: deregulated regime (United Kingdom, Ireland and Hungary)
#  deregulated labour market and high income inequality. 
#  43.5 percent of their former net income in case of unemployment and the employment protection (mean value of 1.5) is also very low. 
#  low government expenditures on social security
#  low tax revenues can be described as a strategy aiming to improve price competitiveness.
cluster_3_vars <- c("employment_protect", "gini_market", "ubr",
                    "gov_exp_socprtc","tax_income", "tax_rev_to_gdp")

# Cluster 4: less globalised model (Poland, Greece, Portugal, Slovakia, Spain, Italia, Slovenia and Czech Republic)
#  exact counterpart to cluster 2 (see Table 2)
#  low degree of international integration as reflected by low values in foreign direct investments and the economic globalisation index. 
#  government expenditures on education and investments in research and development are considerably below the other trade regimes. 
#  relatively strict labor market regulation.
cluster_4_vars <- c("fdi_to_gdp", "kof_econ_defacto", "kof_econ_dejure", 
                    "gerd", "gov_exp_educ", "employment_protect")

# Cluster 5: Technological leaders (France, Sweden, Finland, Denmark, Netherlands, Belgium, Germany and Austria)
#  highest degree of economic complexity
#  high degree of wage coordination and high government expenditures on social security.
#  high investments in research and development
cluster_5_vars <- c("complexity_harv", "coord", "gov_exp_socprtc", "gerd")

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

# Cluster 1: Taxonomy plot-----------------------------------------------------

#' Setup data for cluster taxonomy
#' 
#' Takes the taxonomy data and returns a data frame that can be used
#'   to create figures illustrating the differences among clusters.
#'   
#' @param data_used The data used
#' @param cluster considered The name of the cluster (e.g. C1 or C2); must be
#'   a character with a leading C, as in data_used.
#' @param cluster_variables A list with information about the variables to 
#'   be included in the final data. Names of the list should be clusters as 
#'   in \code{cluster}, the items the names of the variables as strings.
#' @return A data table with the data as to be used by ggplot2.
setup_taxonomy_data <- function(data_used,
                                cluster_considered, 
                                cluster_variables){
  if (!cluster_considered %in% data_used[["cluster"]]){
    stop("Cluster considered not present in data set!")
  }
  cluster_data <- data_used %>%
    dplyr::select(one_of("country", "cluster", 
                         cluster_variables[[cluster_considered]])) %>%
    dplyr::mutate(cluster = ifelse(cluster == cluster_considered, 
                            cluster_considered, "Rest")) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_if(is.numeric, mean, na.rm=TRUE) %>%
    dplyr::ungroup()
  # TODO check for NA
  return(cluster_data)
}

cluster_1_data <- setup_taxonomy_data(data_taxonomy, "C1", clustering_vars)
cluster_2_data <- setup_taxonomy_data(data_taxonomy, "C2", clustering_vars)
cluster_3_data <- setup_taxonomy_data(data_taxonomy, "C3", clustering_vars)
cluster_4_data <- setup_taxonomy_data(data_taxonomy, "C4", clustering_vars)
cluster_5_data <- setup_taxonomy_data(data_taxonomy, "C5", clustering_vars)


#' @param return_full_plot If TRUE (the default) function combines the single
#'  plots into one full plot (using \code{ggpubr:ggarrange}. If FALSE a list of
#'  single plots is returned.)
make_plots <- function(data_used,
                       cluster_considered, 
                       cluster_variables,
                       variable_subset=FALSE,
                       return_full_plot=TRUE){
  if (!(FALSE %in% variable_subset)){
    cluster_variables <- variable_subset
  }
  
  cluster_data <-  setup_taxonomy_data(data_used, 
                                       cluster_considered, 
                                       cluster_variables)
  
  plots_to_do <- names(cluster_data)[2:length(names(cluster_data))]
  final_plots <- list()
  
  for (p in plots_to_do){
    print(p)
    final_plots[[p]] <- ggplot(cluster_data,
                               aes_string(x="cluster", 
                                          y=p)) +
      geom_bar(stat = "identity") +
      ggtitle(p) + 
      scale_y_continuous(expand = c(0, 0))
      theme_bw() + 
      theme(panel.border = element_blank(),
            axis.line = element_line())
  }
  if (return_full_plot){
    full_plot <- ggpubr::ggarrange(
      plotlist = final_plots, 
      ncol = length(names(final_plots)))
    return(full_plot)
  } else {
    return(final_plots)
  }
}

c1_plots <- make_plots(data_taxonomy, "C1", clustering_vars)
ggsave(plot = c1_plots, filename = "output/TEST.pdf", width = length(c1_plots)*2.5, height = 8)

c1plot <- ggplot(cluster_1_data, aes_string(x="cluster", y="res_rents")) + 
  geom_bar(stat = "identity")
c1plot

c1plot <- ggplot(gather(cluster_1_data, variable, value, -cluster), 
                 aes_string(
                   x="variable", y="value", 
                   group="cluster", color="cluster", fill="cluster")
                 ) + 
  geom_bar(
    stat = "identity",
    position=position_dodge()
    ) + 
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste0("Cluster 1", " compared to the rest")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()
        )
c1plot

# besser alles einzeln mit for loop, breite dann mit names(data)*3oder so machen
# gather(variable, mean_value, -c_group)
head(macro_data_agg_core_mean)

complexity <- make_single_plot(macro_data_core_peri_sum,
                               "Complexity",
                               # y_limit = c(40, 65),
                               leg_pos = "none",
                               y_label = "Economic complexity index"
)
complexity 
