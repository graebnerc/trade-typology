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
  "KOF_econ_defacto", "gov_exp_to_GDP", "tax_total", "complexity_harv", 
  "industrial_to_gdp", "GERD", "ict_ksh", "gov_exp_educ",  "tax_rev_to_gdp",
  "adjusted_wage_share", "employment_protect", "ubr", "udens", "tax_income",
  "tax_ssc_employer", "tax_corpcap", "Tax_Estate_plus_Wealth", "fdi_to_gdp", 
  "size_of_finance", "KOF_econ_dejure", "exp_to_gdp", "gov_exp_socprtc"
)

new_macro_data <- data.table::fread("data/Clustering_data_Dennis_new.csv") %>%
  select(one_of("year", "country", missing_vars)) %>%
  dplyr::mutate(
    country=countrycode::countrycode(country, "country.name", "iso3c")
  ) %>%
  dplyr::rename(
    gerd=GERD,
    kof_econ_dejure=KOF_econ_dejure,
    kof_econ_defacto=KOF_econ_defacto,
    gov_exp_to_gdp=gov_exp_to_GDP,
    tax_estate_plus_wealth=Tax_Estate_plus_Wealth
  )

brand_new_macro_data <- MacroDataR::macro_data
brand_new_macro_data <- brand_new_macro_data %>%
  select(one_of("iso3c", "year", "gini_post_tax", "gini_pre_tax", "wage_share",
                "empl_ind", "empl_agr", "empl_serv", "empl_self", "unemp_youth_neet",
                "VA_industry_gdp", "VA_manufct_gdp", "average_wages")) %>%
  rename(country=iso3c) %>%
  filter(country %in% unique(new_macro_data$country))

new_macro_data <- left_join(new_macro_data, brand_new_macro_data, 
                            by = c("country", "year"))

cluster_data_R_v1 <- dplyr::full_join(ictwss, endownments, 
                                      by=c("country"="location_code", "year")) %>%
  dplyr::left_join(., new_macro_data, 
                   by=c("country", "year")) %>%
  dplyr::mutate(
    un_ccode=countrycode::countrycode(country, "iso3c", "un")
  )

# Process data=================================================================
# Process data: Select countries-----------------------------------------------
drop_countries <- c("Canada", "Mexico", "New Zealand", "Turkey", "Switzerland",
                    "United States", "Iceland", "Australia", "Korea", "Norway",
                    "Japan", "Bulgaria", "Lithuania")
drop_countries <- countrycode::countrycode(drop_countries, 
                                           "country.name", "un")

cluster_data_R_v2 <- cluster_data_R_v1 %>%
  dplyr::filter(!un_ccode %in% drop_countries,
                !is.na(un_ccode))

# Make means-------------------------------------------------------------------

cluster_data_R_v3 <- cluster_data_R_v2 %>%
  dplyr::filter(year>=1994) %>%
  dplyr::select(-one_of("year")) %>%
  dplyr::group_by(un_ccode) %>%
  dplyr::summarise_all(mean, na.rm=T) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country=countrycode(un_ccode, "un", "country.name"))

# Process data: z standartization----------------------------------------------
# TODO exp_to_gdp ist nicht mehr drinnen, ist das nicht komisch?
rel_vars <- c("kof_econ_defacto", "coal_metal_export_share", 
              "oil_exports_share", "primary_exports_share_1", 
              "res_rents", "complexity_harv", "industrial_to_gdp", 
              "gerd", "ict_ksh", "gov_exp_educ", "coord", 
              "employment_protect", "ubr", "gov_exp_socprtc", 
              "gini_pre_tax", "tax_corpcap", "tax_estate_plus_wealth", 
              "fdi_to_gdp", "size_of_finance", "kof_econ_dejure",
              "gini_post_tax", "wage_share", "empl_ind", "empl_agr", 
              "empl_serv", "empl_self", "unemp_youth_neet",
              "VA_industry_gdp", "VA_manufct_gdp", "average_wages"
) 
# wird unten nicht mehr verwendet, aber war ursprünglich drinnen:
nrel_vars <- c("exp_to_gdp", "gov_exp_to_gdp", "tax_total", 
               "adjusted_wage_share", "udens", "tax_ssc_employer", 
               "adjcov", "tax_income", "tax_rev_to_gdp")

cluster_data_R_v4 <- cluster_data_R_v3 %>%
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
    zgini_pre_tax = scale(gini_pre_tax)[,1], # egen zgini_pre_tax =std(gini_pre_tax )
    zgini_post_tax = scale(gini_post_tax)[,1],
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
    ztax_rev_to_gdp = scale(tax_rev_to_gdp)[,1], # egen ztax_rev_to_gdp =std(tax_rev_to_gdp)
    zwage_share = scale(wage_share)[,1],
    zempl_ind = scale(empl_ind)[,1], 
    zempl_agr = scale(empl_agr)[,1],
    zempl_serv = scale(empl_serv)[,1],
    zempl_self = scale(empl_self)[,1],
    zunemp_youth_neet = scale(unemp_youth_neet)[,1],
    zVA_industry_gdp = scale(VA_industry_gdp)[,1],
    zVA_manufct_gdp = scale(VA_manufct_gdp)[,1],
    zaverage_wages = scale(average_wages)[,1]
  )

# data overview:
cluster_vars <- paste0("z", rel_vars)