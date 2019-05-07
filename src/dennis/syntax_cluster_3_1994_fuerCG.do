*.* Variante mit Social security statt udens coord und statt ztax_ssc_employercorpcap auch gut, auch theoretisch macht coord für die Handelspolitik mehr Sinn als UD, corpcap leichter erklärbar-->finale Version.
cluster wardslinkage zkof_econ_defacto zcoal_metal_export_share zoil_exports_share zprimary_exports_share_1 zres_rents zcomplexity_harv zindustrial_to_gdp zgerd zict_ksh zgov_exp_educ zcoord zemployment_protect zubr zgov_exp_socprtc zgini_market ztax_corpcap ztax_estate_plus_wealth zfdi_to_gdp zsize_of_finance zkof_econ_dejure , measure(L2squared) name(trade)
cluster dendrogram trade, horizontal labels (country)

*.* Bestimmung der Cluster
cluster stop trade, rule(calinski) groups(2/20)
 cluster stop trade, rule(duda) groups(2/20)


*.* Erzeugung Agglomerationsschema neu
gsort - trade_hgt
gen increase = trade_hgt - trade_hgt[_n+1]
list trade* increase in 1/20,clean
gen rincrease = increase / trade_hgt[_n+1]
list trade* increase rincrease in 1/20,clean
gen rincrease2 = increase / trade_hgt[_n]

*.* Cluster_5solution erzeugen 
gen cluster_5solution = 2 if country=="Luxembourg"
replace cluster_5solution=4 if country=="Portugal"
replace cluster_5solution=4 if country=="Italy"
replace cluster_5solution=4 if country=="Spain"
replace cluster_5solution=4 if country=="Greece"
replace cluster_5solution=1 if country=="Latvia"
replace cluster_5solution=1 if country=="Estonia"
replace cluster_5solution=3 if country=="Hungary"
replace cluster_5solution=4 if country=="Poland"
replace cluster_5solution=4 if country=="Slovakia"
replace cluster_5solution=4 if country=="Czech Republic"
replace cluster_5solution=3 if country=="United Kingdom"
replace cluster_5solution=3 if country=="Ireland"
replace cluster_5solution=5 if country=="Austria"
replace cluster_5solution=5 if country=="Germany"
replace cluster_5solution=5 if country=="Finland"
replace cluster_5solution=5 if country=="Denmark"
replace cluster_5solution=5 if country=="Sweden"
replace cluster_5solution=5 if country=="Belgium"
replace cluster_5solution=5 if country=="Netherlands"
replace cluster_5solution=4 if country=="Slovenia"
replace cluster_5solution=5 if country=="France"

*.* Cluster_4solution erzeugen 
gen cluster_4solution = 2 if country=="Luxembourg"
replace cluster_4solution=3 if country=="Portugal"
replace cluster_4solution=3 if country=="Italy"
replace cluster_4solution=3 if country=="Spain"
replace cluster_4solution=3 if country=="Greece"
replace cluster_4solution=1 if country=="Latvia"
replace cluster_4solution=1 if country=="Estonia"
replace cluster_4solution=3 if country=="Hungary"
replace cluster_4solution=3 if country=="Poland"
replace cluster_4solution=3 if country=="Slovakia"
replace cluster_4solution=3 if country=="Czech Republic"
replace cluster_4solution=3 if country=="United Kingdom"
replace cluster_4solution=3 if country=="Ireland"
replace cluster_4solution=4 if country=="Austria"
replace cluster_4solution=4 if country=="Germany"
replace cluster_4solution=4 if country=="Finland"
replace cluster_4solution=4 if country=="Denmark"
replace cluster_4solution=4 if country=="Sweden"
replace cluster_4solution=4 if country=="Belgium"
replace cluster_4solution=4 if country=="Netherlands"
replace cluster_4solution=3 if country=="Slovenia"
replace cluster_4solution=4 if country=="France"

*.* Neu abspeichern
. save "C:\ÖNB-Projekt\Daten\v34_cluster.dta"
file C:\ÖNB-Projekt\Daten\v34_cluster.dta saved

*.* Nur für ab 1994
drop if year<1994

*.* Neu abspeichern
save "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\v34_cluster_1994.dta"

*.*cluster beschreiben
	by cluster_5solution, sort : summarize kof_econ_defacto coal_metal_export_share oil_exports_share primary_exports_share_1 res_rents complexity_harv industrial_to_gdp gerd ict_ksh gov_exp_educ coord employment_protect ubr gov_exp_socprtc gini_market tax_corpcap tax_estate_plus_wealth fdi_to_gdp size_of_finance kof_econ_dejure
	by cluster_4solution, sort : summarize kof_econ_defacto coal_metal_export_share oil_exports_share primary_exports_share_1 res_rents complexity_harv industrial_to_gdp gerd ict_ksh gov_exp_educ coord employment_protect ubr gov_exp_socprtc gini_market tax_corpcap tax_estate_plus_wealth fdi_to_gdp size_of_finance kof_econ_dejure


*.* Cluster benennen
gen cluster_names=""
replace cluster_names="primary goods" if cluster_5solution==1
replace cluster_names="finance" if cluster_5solution==2
replace cluster_names="flexible labour market" if cluster_5solution==3
replace cluster_names="less globalised" if cluster_5solution==4
replace cluster_names="high_road" if cluster_5solution==5

*.* Darstellung Entwicklung einzelner Variable
collapse (mean)  adjusted_wage_share gini_market gdp_growth unemployment_rate exp_to_gdp imp_to_gdp exp_minus_imp min_wage trade_to_gdp gov_party unemp_longterm wvs_hate_democ wvs_like_leader goodsexports exportsgoodsservices  labqlt_g  tfpg, by(cluster_5solution year)
 save "C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\v34_cluster_entwicklung.dta", replace
file C:\Users\Dennis Tamesberger\OneDrive\ÖNB-Projekt\Daten\v34_cluster_entwick
> lung.dta saved


*.* Entwicklung ab 1994
drop if year<1994
collapse (mean) gdp_growth, by (cluster_5solution year)
xtset cluster_5solution year
xtline gdp_growth, overlay legend (label(1 "primary goods") label(2 "finance")label(3 "flexible labour market")label(4 "less globalised")label(5 "high road")) ytitle (GDP growth)

collapse (mean)  tfpg , by (cluster_5solution year)
xtset cluster_5solution year
xtline tfpg, overlay legend (label(1 "primary goods") label(2 "finance")label(3 "flexible labour market")label(4 "less globalised")label(5 "high road")) ytitle (Total Factor Productivity growth)

drop if year<1994
(1122 observations deleted)
collapse (mean) unemployment_rate, by (cluster_5solution year)
xtset cluster_5solution year
xtline unemployment_rate, overlay legend (label(1 "primary goods") label(2 "finance")label(3 "flexible labour market")label(4 "less globalised")label(5 "high road")) ytitle (Unemployment rate)

collapse (mean)  exp_to_gdp , by (cluster_5solution year)
xtset cluster_5solution year
xtline  exp_to_gdp , overlay legend (label(1 "primary goods") label(2 "finance")label(3 "flexible labour market")label(4 "less globalised")label(5 "high road")) ytitle ( exp_to_gdp )

collapse (mean) exp_minus_imp, by ( cluster_5solution year)
xtset cluster_5solution year
xtline  exp_minus_imp , overlay legend (label(1 "primary goods") label(2 "finance")label(3 "flexible labour market")label(4 "less globalised")label(5 "high road")) ytitle ( NET Exports / GDP )

collapse (mean) current_account_balance_to_gdp , by (cluster_5solution year)
xtset cluster_5solution year
xtline current_account, overlay legend (label(1 "primary goods") label(2 "finance")label(3 "flexible labour market")label(4 "less globalised")label(5 "high road")) ytitle (Current account balance to GDP)




