# 15.03.2019: Create new data for endownment dimension
rm(list = ls())
library(countrycode)
library(data.table)
library(WDI)
update_data <- T

countries_considered <- strsplit(
  "LU, SE, FI, DK, FR, NL, BE, SI, DE, AT, LV, EE, SK, CZ, PL, HU, GB, IE, PT, GR, ES, IT", 
  ", ")[[1]]

# World Bank data on natural resource rents====================================
# https://data.worldbank.org/indicator/ny.gdp.totl.rt.zs
nat_res_rents_file_name <- "data/wb_nat_resource_rents.csv"
if (update_data){
  nat_res_rents_raw <- as.data.table(WDI::WDI(country = countries_considered, 
                   indicator = "ny.gdp.totl.rt.zs"))
  data.table::fwrite(nat_res_rents_raw, nat_res_rents_file_name)
} else {# TODO Test whether file exists
  nat_res_rents_raw <- data.table::fread(nat_res_rents_file_name)
}

nat_res_rents <- nat_res_rents_raw[, res_rents:=ny.gdp.totl.rt.zs
                                   ][, .(iso2c, year,res_rents)]

# Exports to GDP===============================================================
# https://data.worldbank.org/indicator/ne.trd.gnfs.zs
exp_to_gdp_file_name <- "data/wb_exp_to_gdp.csv"
if (update_data){
  exp_to_gdp_raw <- as.data.table(WDI::WDI(country = countries_considered, 
                                              indicator = "ne.trd.gnfs.zs"))
  data.table::fwrite(exp_to_gdp_raw, exp_to_gdp_file_name)
} else {# TODO Test whether file exists
  exp_to_gdp_raw <- data.table::fread(exp_to_gdp_file_name)
}

exp_to_gdp <- exp_to_gdp_raw[, exp_to_gdp:=ne.trd.gnfs.zs
                             ][, .(iso2c, year, exp_to_gdp)]

# Coals, Metal and Oil shares of total exports=================================
# For SITC codes see: 
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50262/Search-SITC-code-description
oil_codes <- c("33", "34") # 33	Petroleum, petroleum products and related materials and 34	Gas, natural and manufactured
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
coal_and_metal_codes_5 <- c("52217")
# 52217 Alkali and alkaline-earth metals; rare earth metals
# Eher nicht:
# 69	Manufactures of metals, nes
# 691	Structures and parts, nes, of iron, steel or aluminium

export_data_file_name <- "data/hrvd_complexity_atlas.csv.gz"
if (update_data){
  web_link <- "https://intl-atlas-downloads.s3.amazonaws.com/country_sitcproduct4digit_year.csv.zip"
  export_data_raw <- fread(cmd = paste0("curl ", web_link, " | funzip"),
                           colClasses = c(rep("double", 11), rep("character", 4)), 
                           select = c("year", "export_value", "location_code", "sitc_product_code"))
  export_data_raw <- export_data_raw[location_code%in%countrycode(countries_considered, "iso2c", "iso3c")]
  readr::write_csv(export_data_raw, gzfile(export_data_file_name))
} else{
  export_data_raw <- fread(cmd = paste0("gunzip -c ", export_data_file_name), 
                           colClasses = c(rep("double", 2), rep("character", 2)))
}

export_data <- export_data_raw[, 
                               total_exports:=sum(export_value, na.rm = T), 
                               .(location_code, year)]

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

primary_exports_data <- export_data[, sitc_main:=substr(sitc_product_code, 1, 1)
                                    ][, export_primary:=sum(export_value, na.rm = T), 
                                      .(year, location_code, sitc_main)
                                      ][, primary_exports_share:=export_primary/total_exports]
primary_exports_data <-  primary_exports_data[, 
                                              .(year, location_code, primary_exports_share)]
# Merging data=================================================================
nat_res_rents
exp_to_gdp
primary_exports_data