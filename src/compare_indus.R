actually_used <- fread("data/clustering_data_used.csv")
dennis_data <-  data.table::fread("data/Clustering_data_Dennis_new.csv") %>%
  select(one_of("year", "country", "ind_output_meur", "industrial_production_excl_const", "gdp_curr_euro","industrial_to_gdp")) %>%
  mutate(industrial_to_gdp_test=industrial_production_excl_const/gdp_curr_euro,
         industrial_to_gdp_test_meur=ind_output_meur/gdp_curr_euro,
         country=countrycode(country, "country.name", "iso3c"))


indus_comp <- cluster_data_R_v3 %>%
  select(one_of("country", "year", "VA_manufct_gdp", "VA_industry_gdp")) %>%
  left_join(., dennis_data, by=c("country", "year"))
indus_comp <- as.data.table(indus_comp)





empl_ind <- clustering_result
industrial_to_gdp <- clustering_result
VA_industry_gdp <- clustering_result

industrial_to_gdp <- industrial_to_gdp + ggtitle("industrial_to_gdp (können wir nicht nehmen)")
empl_ind <- empl_ind + ggtitle("Employment in industry instead of industrial_to_gdp")
VA_industry_gdp <- VA_industry_gdp + ggtitle("Share of industriy VA in GDP instead of industrial_to_gdp")

indus_comparison <- ggpubr::ggarrange(industrial_to_gdp, empl_ind, VA_industry_gdp, ncol = 3)
ggsave(plot = indus_comparison, filename = "output/cluster_comparison.pdf", height = 8, width = 16)
