*.* Daten importieren. Import text data from spread sheet. automatecally determine delimiter. Dann destring (convert string variables to nomeric, und convert nonnumeric strings to missing values)
insheet using "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\dimension_endownment_data_MIT_2.csv"
gen country=""
replace country="Austria" if location_code=="AUT"
replace country="Belgium" if location_code=="BEL"
replace country="Germany" if location_code=="DEU"
replace country="Spain" if location_code=="ESP"
replace country="Finland" if location_code=="FIN"
replace country="France" if location_code=="FRA"
replace country="United Kingdom" if location_code=="GBR"
replace country="Greece" if location_code=="GRC"
replace country="Hungary" if location_code=="HUN"
replace country="Ireland" if location_code=="IRL"
replace country="Italy" if location_code=="ITA"
replace country="Netherlands" if location_code=="NLD"
replace country="Poland" if location_code=="POL"
replace country="Portugal" if location_code=="PRT"
replace country="Sweden" if location_code=="SWE"
replace country="Estonia" if location_code=="EST"
replace country="Latvia" if location_code=="LVA"
replace country="Slovenia" if location_code=="SVN"
replace country="Czech Republic" if location_code=="CZE"
replace country="Slovakia" if location_code=="SVK"
replace country="Luxembourg" if location_code=="LUX"

*.* Neu abspeichern
. save "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\endownments.dta"
> , replace
file C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\endownments.dta sav
> ed


*.* Datensätze zusammenspielen. Achtung 2017 muss gelöchst werden weil nur für endownments daten vorhanden.
merge 1:1 country year using "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\endownments.dta", update
merge 1:1 country year using "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\ictwss_short.dta"
destring adjcov coord, replace

*.* neu abspeichern
save "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\v34_cluster.dta"

*.* Mittelwerte pro Land erzeugen 
Maximalvariante
collapse (mean) coal_metal_export_share oil_exports_share primary_exports_share_1 exp_to_gdp trade_to_gdp kof_econ_defacto  res_rents complexity_harv industrial_to_gdp gerd ict_ksh gov_exp_educ adjusted_wage_share employment_protect ubr udens gini_market gov_exp_socprtc tax_ssc_employer tax_corpcap tax_estate_plus_wealth fdi_to_gdp size_of_finance kof_econ_dejure adjcov coord tax_income tax_total tax_rev_to_gdp gov_exp_to_gdp , by (country un_ccode)

*.* Nicht volläständige Länder löschen

	drop if country=="Canada"
	drop if country=="Mexico"
	drop if country=="New Zealand"
	drop if country=="Turkey"
	drop if country=="United States"
	drop if country=="Iceland"
	drop if country=="Australia"
	drop if country=="Korea"
	drop if country=="Switzerland"
	drop if country=="Norway"
	drop if country=="Japan"
	drop if country=="Bulgaria"
	drop if country=="Slovak Republic"
	 drop if country=="Lithuania"

*.* Z-Standardisieren für jede variabel

 egen zkof_econ_defacto =std( kof_econ_defacto)
 egen zgov_exp_to_gdp =std( gov_exp_to_gdp)
 egen ztax_total =std(tax_total)
 egen zcomplexity_harv =std( complexity_harv)
 egen zindustrial_to_gdp =std( industrial_to_gdp)
 egen zgerd =std(gerd)
 egen zict_ksh =std( ict_ksh)
 egen zgov_exp_educ =std( gov_exp_educ )
 egen zadjusted_wage_share =std( adjusted_wage_share )
 egen zemployment_protect =std( employment_protect )
 egen zubr =std( ubr )
 egen zudens =std( udens )
 egen zgini_market =std(gini_market )
 egen ztax_ssc_employer =std( tax_ssc_employer )
 egen ztax_corpcap =std( tax_corpcap )
 egen ztax_estate_plus_wealth =std( tax_estate_plus_wealth )
 egen zfdi_to_gdp =std( fdi_to_gdp )
 egen zsize_of_finance =std( size_of_finance )
 egen zkof_econ_dejure =std( kof_econ_dejure )
 egen zoil_exports_share=std( oil_exports_share)
 egen zprimary_exports_share_1=std( primary_exports_share_1)
 egen zexp_to_gdp=std( exp_to_gdp)
 egen zres_rents =std( res_rents )
 egen zcoal_metal_export_share =std( coal_metal_export_share )
 egen zgov_exp_socprtc =std(gov_exp_socprtc )
 egen zcoord=std(coord)
 egen zadjcov=std(adjcov)
 egen ztax_income =std(tax_income)
 egen ztax_rev_to_gdp =std(tax_rev_to_gdp)

*.* Daten neu abspeichern
. save "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\v34_cluster_mean
> .dta"
file C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\v34_cluster_mean.dt
> a saved

*.* Variante ab 1994
 drop if year<1994
 drop if year>2016
