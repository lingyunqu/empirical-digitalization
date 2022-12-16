* a sample analysis job
use digitalization.dta
* set fixed effect
xtset id year



* digitalization indicator construction.
** variable list: hardward & softward & availability rate
** PCA
factortest software_enterprise_1000 info_enterprise_1000 service_enterprise_1000 software_enterprise_per_1000 info_employee_per_1000 ser_employee_per_1000 soft_sell_pctg info_sell_pctg ser_sell_pctg soft_add_pctg info_add_pctg ser_add_pctg internet smartphone computer
factor software_enterprise_1000 info_enterprise_1000 service_enterprise_1000 software_enterprise_per_1000 info_employee_per_1000 ser_employee_per_1000 soft_sell_pctg info_sell_pctg ser_sell_pctg soft_add_pctg info_add_pctg ser_add_pctg internet smartphone computer, pcf
rotate
predict f1 f2
* f1：0.6601/0.8141=0.81083405, f2: 0.154/0.8141=0.18916595
gen digital = 0.81083405*f1+0.18916595*f2

*clear empty rows
drop if district == .

* spillover_11: Using three continuous years for calculating the direction of the directed arrow in the given year and the PC (Peter and Clark) algorithm, create the digitalization spillover index, one per country per year ranging from 2013 to 2019. For each base year, the binary variable digitalization spillover index 〖Spillover〗_(i,j) is given value 1 if the prefecture j experiences digitalization spillover in year i, and other prefectures, both spill-in and without effects, are given value -1. 
*  only keep the directed arrows statistically significant at 1%.

*--------
*baseline analysis
*--------
**GDP per capita ~ spillover: impact of digitalization spillover to GDP per capita
xtreg log_productionthousand_per_c spillover_11 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand,fe
** (-), statistically significant
est store baseline1

**Overall GDP ~ spillover: impact of digitalization spillover to Overall GDP 
xtreg log_productionmillionen spillover_11 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand,fe
** (-), statistically significant
est store baseline2
esttab baseline1 baseline2 using baseline.rtf, b(%12.3f) se(%12.3f) nogap compress s(year N r2) star(* 0.1 ** 0.05 *** 0.01)

*--------
*Robust test: change explained variable
*--------
*GDP per capita ~ digital
xtreg log_productionthousand_per_c digital lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand,fe
** (+), statistically significant
est store robust1

*Overall GDP ~ digital
xtreg log_productionmillionen digital lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand,fe
** (+), statistically significant
est store robust2
esttab robust1 robust2 using robust.rtf, b(%12.3f) se(%12.3f) nogap compress s(year N r2) star(* 0.1 ** 0.05 *** 0.01)

*--------
* alternative mechanism
*--------
*GDP per capita ~ population aging: 
*diff_region_80_over: the difference between the over-80-year-old population proportions of the prefecture and the regional average.
gen inter_80= spillover_11*Diff80
xtreg log_productionthousand_per_c spillover_11 Diff80 inter_80 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand,fe
*spillover (-), aging (-), interaction(-). statistically significant.
est store mechanism1

*GDP per capita ~ industrial structure: 
*service: the ratio of entertainment and life-related service industry production to total GDP.
gen inter_life= spillover_11*service
xtreg log_productionthousand_per_c spillover_11 service inter_life lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand,fe
*spillover (-), aging (-), interaction(+). statistically significant.
est store mechanism2
esttab mechanism1 mechanism2 using alternativem.rtf, b(%12.3f) se(%12.3f) nogap compress s(year N r2) star(* 0.1 ** 0.05 *** 0.01)

*--------
* Heterogeneity Analysis 1: geographic location
*--------
** gen variable divided by geographic location
gen sea=0
replace sea = 1 if name_kanji=="北海道"|name_kanji=="青森県"|name_kanji=="秋田県"|name_kanji=="山形県"|name_kanji=="新潟県"|name_kanji=="富山県"|name_kanji=="石川県"|name_kanji=="福井県"|name_kanji=="京都府"|name_kanji=="兵庫県"|name_kanji=="鳥取県"|name_kanji=="島根県"|name_kanji=="山口県"|name_kanji=="福岡県"|name_kanji=="佐賀県"|name_kanji=="長崎県"

**GDP per capita ~ not facing sea
xtreg log_productionthousand_per_c spillover_11 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand if sea==0,fe
** not significant
est store sea0

**GDP per capita ~ facing sea
xtreg log_productionthousand_per_c spillover_11 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand if sea==1,fe
** (-) significant
est store sea1
esttab sea0 sea1 using hetero_sea.rtf, b(%12.3f) se(%12.3f) nogap compress s(year N r2) star(* 0.1 ** 0.05 *** 0.01)

* Heterogeneity Analysis 2: gdp
bys year: egen gdpm=mean(log_productionmillionen)
gen gdp_01 = (log_productionmillionen> gdpm) if gdpm!=.

**GDP per capita ~ spillover | high gdp
xtreg log_productionthousand_per_c spillover_11 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand if gdp_01 ==1,fe
* (-) significant
est store gdp0

**GDP per capita ~ spillover | low gdp
xtreg log_productionthousand_per_c spillover_11 lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand if gdp_01 ==0,fe
* not significant
est store gdp1
esttab gdp0 gdp1 using gdp.rtf, b(%12.3f) se(%12.3f) nogap compress s(year N r2) star(* 0.1 ** 0.05 *** 0.01)

*Endogeneity
*internet_30mbps:  the availability rate of high-speed Internet in primary schools to test the endogeneity. Since the time range of this paper is 2011-2019, the contribution of primary students to the GDP is ignorable. 
ivregress 2sls log_productionthousand_per_c lifeinv agrinv secuinv consumption consumptionthousand_per_c populationthousand (spillover_11 =  internet_30mbps),r first
est store endo
esttab endo using endo.rtf, b(%12.3f) se(%12.3f) nogap compress s(year N r2) star(* 0.1 ** 0.05 *** 0.01)
