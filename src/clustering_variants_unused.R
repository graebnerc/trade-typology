# Variable selection 1---------------------------------------------------------
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "coal_metal_export_share", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "gini_pre_tax"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v1"

v1_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 2---------------------------------------------------------
# Just as 1 but this time with post tax gini
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "coal_metal_export_share", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "gini_post_tax"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v2(post_tax_gini)"

v2_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 3---------------------------------------------------------
# Just as 1 but this time with wage_share instead of gini for labor market institutions
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "coal_metal_export_share", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "wage_share"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v3(wage_share_instead_gini)"

v3_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 4---------------------------------------------------------
# As 3, but with emploment in the agricultural sector instead of coal exports for dimension 1
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "empl_agr", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "wage_share"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v4(v3_empl_agri_instead_coal)"

v4_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 5---------------------------------------------------------
# As 3, but with emploment in the service sector instead of coal exports for dimension 1 (should be lower for those with many endownments)
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "empl_serv", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "wage_share"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v5(v3_empl_serv_instead_coal)"

v5_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 6---------------------------------------------------------
# As 3, but with emploment in the indus sector instead of coal exports for dimension 1 (should be lower for those with many endownments)
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "empl_ind", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "wage_share"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v6(v3_empl_indus_instead_coal)"

v6_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 7---------------------------------------------------------
# As 3, but with VA in manufacturing instead of coal exports for dimension 1 (should be lower for those with many endownments)
variables_clustering <- list(
  "endownments" = c(
    "kof_econ_defacto", "VA_manufct_gdp", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "gov_exp_socprtc", "wage_share"  
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 5
current_version <- "v7(v3_VA_manuf_instead_coal)"

v7_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)


# Variable selection 8---------------------------------------------------------
# As discussed
variables_clustering <- list(
  "endownments" = c(# empl_agr # VA_manufct_gdp
    "empl_agr", "VA_manufct_gdp", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "average_wages", "wage_share"   # TODO Add av wages
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 6
current_version <- "v8(discussed)"

v8_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)

# Variable selection 9---------------------------------------------------------
# As discussed
variables_clustering <- list(
  "endownments" = c(# empl_agr # VA_manufct_gdp
    "empl_agr", "VA_manufct_gdp", "oil_exports_share", "primary_exports_share_1", "res_rents"
  ),
  "capabilities" = c(
    "complexity_harv", "industrial_to_gdp", "gerd", "ict_ksh", "gov_exp_educ"
  ),
  "labor_market" = c(
    "coord", "employment_protect", "ubr", "average_wages", "gini_post_tax"   # TODO Add av wages
  ),
  "regulation" = c(
    "tax_corpcap", "tax_estate_plus_wealth", "fdi_to_gdp", "size_of_finance", "kof_econ_dejure" 
  )
)
variables_clustering <- unlist(variables_clustering)

n_groups <- 6
current_version <- "v9(gini_instead_wage_share)"

v9_clustering <- save_dendogram(clustering_variables = variables_clustering, 
                                vers = current_version, number_groups = n_groups)
