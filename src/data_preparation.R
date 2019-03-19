# 15.03.2019: Create new data for endownment dimension
rm(list = ls())
library(countrycode)
library(data.table)
library(WDI)
update_data <- F

countries_considered <- strsplit(
  "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT", 
  ", ")[[1]]

# World Bank data on natural resource rents====================================
# https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs

nat_res_rents_file_name <- "data/wb_nat_resource_rents.csv"
if (update_data){
  nat_res_rents_raw <- as.data.table(WDI::WDI(country = countries_considered, 
                                              indicator = "ny.gdp.totl.rt.zs", 
                                              start = 1962, end = 2016))
  data.table::fwrite(nat_res_rents_raw, nat_res_rents_file_name)
} else {# TODO Test whether file exists
  nat_res_rents_raw <- data.table::fread(nat_res_rents_file_name)
}

nat_res_rents <- nat_res_rents_raw[, res_rents:=ny.gdp.totl.rt.zs
                                   ][, iso2c:=countrycode(iso2c, "iso2c", "iso3c")
                                     ][, .(iso2c, year,res_rents)]

# Exports to GDP===============================================================
# https://data.worldbank.org/indicator/ne.trd.gnfs.zs

exp_to_gdp_file_name <- "data/wb_exp_to_gdp.csv"
if (update_data){
  exp_to_gdp_raw <- as.data.table(WDI::WDI(country = countries_considered, 
                                           indicator = "ne.trd.gnfs.zs", 
                                           start = 1962, end = 2016))
  data.table::fwrite(exp_to_gdp_raw, exp_to_gdp_file_name)
} else {# TODO Test whether file exists
  exp_to_gdp_raw <- data.table::fread(exp_to_gdp_file_name)
}

exp_to_gdp <- exp_to_gdp_raw[, exp_to_gdp:=ne.trd.gnfs.zs
                             ][, iso2c:=countrycode(iso2c, "iso2c", "iso3c")
                               ][, .(iso2c, year, exp_to_gdp)]

# Get export data from MIT=====================================================
# https://atlas.media.mit.edu/en/resources/data/
export_data_mit_file_name <- "data/mit_export_data.fst"
if (update_data){
  # TODO Implement download of data
  web_link <- "https://atlas.media.mit.edu/static/db/raw/year_origin_sitc_rev2.tsv.bz2"
  web_link_countries <- "https://atlas.media.mit.edu/static/db/raw/country_names.tsv.bz2"
  export_data_file_name_web <- "data/year_origin_sitc_rev2.tsv.bz2"
  country_file_web <- "data/country_names.tsv.bz2"
  mit_country_names <- as.data.frame(fread(country_file_web))
  export_data_mit_raw <- fread(export_data_file_name_web,
                               colClasses = c("double", rep("character", 2), rep("double", 4)),
                               select = c("year", "origin", "sitc", "export_val"))
  export_data_mit_raw[, origin:=countrycode(origin, "id_3char", "name", 
                                                  custom_dict = mit_country_names)
                      ][, origin:=countrycode(origin, "country.name", "iso2c")]
  export_data_mit_raw <- export_data_mit_raw[origin %in% countries_considered]
  fst::write.fst(x = export_data_mit_raw, path = export_data_mit_file_name, compress = 100)
} else{
  export_data_mit_raw <- fst::read.fst(export_data_mit_file_name, as.data.table = T)
}


# Get export data from Harvard=================================================
# http://atlas.cid.harvard.edu/downloads

export_data_file_name <- "data/hrvd_complexity_atlas.fst"
if (update_data){
  web_link <- "https://intl-atlas-downloads.s3.amazonaws.com/country_sitcproduct4digit_year.csv.zip"
  export_data_raw <- fread(cmd = paste0("curl ", web_link, " | funzip"),
                           colClasses = c(rep("double", 11), rep("character", 4)), 
                           select = c("year", "export_value", "location_code", "sitc_product_code"))
  export_data_raw <- export_data_raw[location_code%in%countrycode(countries_considered, "iso2c", "iso3c")]
  fst::write.fst(x = export_data_raw, path = export_data_file_name, compress = 100)
} else{
  export_data_raw <- fst::read.fst(export_data_file_name, as.data.table = T) #colClasses = c(rep("double", 2), rep("character", 2)))
}
export_data_raw[, total_exports:=sum(export_value, na.rm = T), 
                .(location_code, year)]

# Oil shares of total exports=================================

# For SITC codes see: 
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50262/Search-SITC-code-description
oil_codes <- c("33", "34") 
# 33	Petroleum, petroleum products and related materials 
# 34	Gas, natural and manufactured
oil_exports <- copy(export_data_raw)
oil_exports[, sitc_red:=substr(sitc_product_code, 1, 2)
                           ][sitc_red%in%oil_codes, 
                             oil_export:=sum(export_value, na.rm = T), 
                             .(year, location_code)
                             ][, oil_exports_share:=oil_export/total_exports]
oil_exports <- unique(oil_exports[!is.na(oil_exports_share)
                           ][, .(year, location_code, oil_exports_share)])
head(oil_exports)

# Test with dplyr -------------------------------------------------------------
test_oil_exports <- copy(export_data_raw)
test_oil_exports <- test_oil_exports %>%
  dplyr::filter(location_code=="AUT" & year==1989) %>%
  dplyr::mutate(sitc2=substr(sitc_product_code, 1, 2)) %>%
  dplyr::filter(sitc2 %in% oil_codes) %>%
  dplyr::mutate(oil_exp_share=sum(export_value, na.rm = T)/total_exports)

# Coal and metal share of total exports========================================

# For SITC codes see: 
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50262/Search-SITC-code-description
coal_and_metal_codes_2 <- c("32", "35", "28", "68", "97")
# 32	Coal, coke and briquettes
# 35	Electric current
# 28	Metalliferous ores and metal scrap
# 68	Non-ferrous metals
# 97	Gold, non-monetary (excluding gold ores and concentrates)

coal_and_metal_codes_4 <- c("5224", "5231", "5232", "5233")
# 5224	Metallic oxides of zinc, iron, lead, chromium etc
# 5231	Metallic salts and peroxysalts of inorganic acids
# 5232	Metallic salts and peroxysalts of inorganic acids
# 5233	Salts of metallic acids; compounds of precious metals

# Eher nicht:
# 69	Manufactures of metals, nes
# 691	Structures and parts, nes, of iron, steel or aluminium
coal_metal_shares <- copy(export_data_raw)
coal_metal_shares[, sitc2:=substr(sitc_product_code, 1, 2)
                  ][, sitc4:=sitc_product_code
                    ][, coal_metal:=ifelse(
                      sitc2 %in% coal_and_metal_codes_2 | 
                        sitc4 %in% coal_and_metal_codes_4, 
                      TRUE, FALSE)]
coal_metal_shares[coal_metal==TRUE,
                  coal_metal_exports:=sum(export_value, na.rm = T), 
                  .(year, location_code)
                  ][ , coal_metal_export_share:=coal_metal_exports/total_exports]
coal_metal_shares <- unique(coal_metal_shares[!is.na(coal_metal_export_share), 
                                              .(year, location_code, coal_metal_export_share)])
head(coal_metal_shares)

# Test with dplyr -------------------------------------------------------------
test_coal_metal_shares <- copy(export_data_raw)
test_coal_metal_shares <- test_coal_metal_shares %>%
  dplyr::filter(location_code=="AUT" & year==1988) %>%
  dplyr::mutate(sitc2=substr(sitc_product_code, 1, 2),
                sitc4=substr(sitc_product_code, 1, 4)) %>%
  dplyr::filter(sitc2 %in% coal_and_metal_codes_2 | sitc4 %in% coal_and_metal_codes_4) %>%
  dplyr::mutate(coal_exp_share=sum(export_value, na.rm = T)/total_exports)

# Share of primary exports=====================================================

primary_goods_codes_1 <- c("0", "1", "2", "4")
primary_goods_codes_2 <- c(primary_goods_codes_1, "3")
# In jedem Fall:
# 0	Food and live animals chiefly for food
# 2	Crude materials, inedible, except fuels
# 1	Beverages and tobacco
# 4	Animal and vegetable oils, fats and waxes
# Unklar:
# 3	Mineral fuels, lubricants and related materials
# TODO das stimmt noch nicht: nicht die subsets genommen, und gibt summe>100
primary_exports_data <- copy(export_data_raw)
primary_exports_data[, sitc_main:=substr(sitc_product_code, 1, 1)
                     ][sitc_main%in%primary_goods_codes_1, 
                       export_primary_1:=sum(export_value, na.rm = T), 
                       .(year, location_code)
                       ][sitc_main%in%primary_goods_codes_2, 
                         export_primary_2:=sum(export_value, na.rm = T), 
                         .(year, location_code)
                       ][, primary_exports_share_1:=export_primary_1/total_exports
                         ][, primary_exports_share_2:=export_primary_2/total_exports]
primary_exports_data <-  unique(
  primary_exports_data[!is.na(primary_exports_share_1) & !is.na(primary_exports_share_2), 
                       .(year, location_code, primary_exports_share_1, primary_exports_share_2)]
  )

# Test with dplyr -------------------------------------------------------------
head(primary_exports_data)
test_primary_goods_shares <- copy(export_data_raw)
test_primary_goods_shares <- test_primary_goods_shares %>%
  dplyr::filter(location_code=="AUT" & year==1988) %>%
  dplyr::mutate(sitc1=substr(sitc_product_code, 1, 1)) %>%
  dplyr::filter(sitc1 %in% primary_goods_codes_2) %>%
  dplyr::mutate(primary_exp_share=sum(export_value, na.rm = T)/total_exports)

# Merging data=================================================================

head(nat_res_rents)
head(exp_to_gdp)
head(primary_exports_data)
head(oil_exports)
head(coal_metal_shares)
full_data <- dplyr::full_join(coal_metal_shares, oil_exports, 
                              by=c("year", "location_code")) %>%
  dplyr::full_join(., primary_exports_data, by=c("year", "location_code")) %>%
  dplyr::full_join(., exp_to_gdp, by=c("year", "location_code"="iso2c")) %>%
  dplyr::full_join(., nat_res_rents, by=c("year", "location_code"="iso2c"))
fwrite(full_data, "data/dimension_endownment_data.csv")
