
**************************************
***ANALYSIS AT DISTRICT LEVEL***
**************************************
*This Do-file corresponds to analysis I*

*author: Luca Schmidt*


******SHRUG DATA*******

**COMBINE DATASETS IN SHRUG**
use "C:\Users\lucas\Downloads\Shrug_data\population_and_economic_census_data\shrug-v1.3.samosa-pop-econ-census-dta\shrug_pc01",clear
merge 1:1 shrid using "C:\Users\lucas\Downloads\Shrug_data\population_and_economic_census_data\shrug-v1.3.samosa-pop-econ-census-dta\shrug_pc11"
drop if _merge != 3
drop _merge
merge 1:1 shrid using "C:\Users\lucas\Downloads\Shrug_data\forest_cover_data\shrug-v1.3.samosa-vcf-dta\shrug_vcf_wide"

**DIVIDE TOTAL FOREST IN VILLAGE BY THE NUMBER OF PIXEL CELLS THAT LIE WITHIN**
local i = 0
forvalues i = 0(1)14{

	if inrange(`i',0,9){
			display `i'
		gen forest_200`i' = total_forest200`i'/num_cells
	local i = `i' + 1
	}
	
	else{
		gen forest_20`i' = total_forest20`i'/num_cells
	local i = `i' + 1
	}
	
}

drop if _merge != 3 
keep shrid pc01_pca_tot_p pc11_pca_tot_p pc01_pca_tot_p_u pc11_pca_tot_p_u forest_2000 forest_2001 forest_2002 forest_2003 forest_2003 forest_2004 forest_2005 forest_2006 forest_2007 forest_2008 forest_2009 forest_2010 forest_2011 forest_2012 forest_2013 forest_2014

**MERGE WITH NIGHT LIGHT DATA**
**NIGHT LIGHT DATA**
merge 1:1 shrid using "C:\Users\lucas\Downloads\Shrug_data\night_data\shrug-v1.3.samosa-nl-dta\shrug_nl_wide.dta"

drop if _merge != 3

local i = 0
forvalues i = 0(1)13{

	if inrange(`i',0,9){
			display `i'
		gen light_200`i' = total_light200`i'/num_cells
	local i = `i' + 1
	}
	
	else{
		gen light_20`i' = total_light20`i'/num_cells
	local i = `i' + 1
	}
	
}

keep shrid pc01_pca_tot_p pc11_pca_tot_p pc01_pca_tot_p_u pc11_pca_tot_p_u forest_2000 forest_2001 forest_2002 forest_2003 forest_2003 forest_2004 forest_2005 forest_2006 forest_2007 forest_2008 forest_2009 forest_2010 forest_2011 forest_2012 forest_2013 forest_2014 light_2000 light_2001 light_2002 light_2003 light_2004 light_2005 light_2006 light_2007 light_2008 light_2009 light_2010 light_2011 light_2012 light_2013 

**RENAME VARIABLES**
rename pc01_pca_tot_p population01
rename pc11_pca_tot_p population11
rename pc01_pca_tot_p_u urban01
rename pc11_pca_tot_p_u urban11

**MERGE WITH DISTRICT KEYS**
merge 1:1 shrid using "C:\Users\lucas\Downloads\Shrug_data\ancillary_data\shrug-v1.3.samosa-keys-dta\shrug_pc11_district_key.dta"
drop pc11_district_id pc11_state_name pc11_state_id 
drop if _merge != 3
drop _merge

***CREATE A VARIABLE FOR THE NUMBER OF VILLAGES AND TOWNS WITHIN A DISTRICT***
*In order to obtain average pixel value per district*
sort pc11_district_name
by pc11_district_name: gen numvill = _N

***SAVE NUMBER OF VILLAGES IN DISTRICT***
preserve 
keep pc11_district_name numvill
duplicates drop pc11_district_name, force 
save "C:\Users\lucas\Downloads\num_vill.dta", replace
restore

**COLLAPSE DATA TO DISTICT LEVEL**
sort pc11_district_name
collapse(sum) population01 population11 urban01 urban11 forest_2000 forest_2001 forest_2002 forest_2003 forest_2004 forest_2005 forest_2006 forest_2007 forest_2008 forest_2009 forest_2010 forest_2011 forest_2012 forest_2013 forest_2014 light_2000 light_2001 light_2002 light_2003 light_2004 light_2005 light_2006 light_2007 light_2008 light_2009 light_2010 light_2011 light_2012 light_2013, by(pc11_district_name)

merge 1:1 pc11_district_name using"C:\Users\lucas\Downloads\num_vill.dta"

**CREATE NUMERIC IDENTIFIER**	
drop in 1/1
gen id = _n
drop _merge

**RESHAPE DATA TO OBTAIN PANEL FORMAT**
reshape long forest_ light_ , i(id) j(year) 
rename pc11_district_name distname
*drop in 1/14

***GENERATE FOREST COVER DIVIDED BY NUMBER OF VILLAGES/TOWNS WITHIN DISTRICT***
gen forest = forest_/numvill
gen light = light_/numvill
drop light_ forest_

*CHECK WHETHER ZERO VALUES EXIST FOR FOREST OR NIGHT LIGHTS AT DISTRICT LEVEL*
tab id if forest == 0
tab id if light == 0

*For lights there exist some zero values (43 out of 7560 cases) not so for forest 
*Generate logarithm of forest and night lights
gen lforest = ln(forest + 1)
gen llight = ln(light + 1)

**GENERATE URBAN RATIO**
gen urbanr01 = urban01/population01
gen urbanr11 = urban11/population11

drop urban01 urban11
drop if year == 2014

save "C:\Users\lucas\Downloads\Shrug_data\panel", replace 

***************************************************************



***AGRICULTURAL PRODUCTIVITY***

**MATCH WITH AGRICULTURAL PRODUCTIVITY AT THE DISTRICT LEVEL**
use "C:\Users\lucas\Downloads\Meso_level_data_crop_yield.dta", clear 

*GENERATE MEASURE OF CROP-SPECIFIC YIELD*
 gen rice_productivity = riceproduction1000tons/ricearea1000ha
 gen wheat_productivity = wheatproduction1000tons/wheatarea1000ha
 gen sugar_productivity = sugarcaneproduction1000tons/sugarcanearea1000ha
 gen kharif_sorg_productivity = kharifsorghumproduction1000tons/kharifsorghumarea1000ha
 gen rabi_sorg_productivity= rabisorghumproduction1000tons/rabisorghumarea1000ha

*preparing the merge over district name*
gen distname_1 = lower(distname)
drop distname
rename distname_1 distname
save "C:\Users\lucas\Downloads\ICRISAT_dataset.dta", replace 

merge 1:1 distname year using"C:\Users\lucas\Downloads\Shrug_data\panel"
preserve
keep if _merge == 3
save "C:\Users\lucas\Downloads\matches_districts.dta", replace
restore


***************************************************************



***USING STATA PACKAGES TO ENHANCE MATCHES**

***IMPROVE MATCHES WITH MATCHIT***

ssc install matchit
ssc install freqindex

***KEEP NON-MERGED OBSERVATIONS SEPERATELY FOR MASTER AND USING***
preserve 
keep if _merge == 1
save "C:\Users\lucas\Desktop\MASTER_1.dta", replace
restore
keep if _merge == 2
save "C:\Users\lucas\Desktop\USING_1.dta", replace 

***USE MATCHIT TO DO FUZZY MATCHING***
use "C:\Users\lucas\Desktop\MASTER_1.dta", clear
matchit year distname using "C:\Users\lucas\Desktop\USING_1.dta", idu(year) txtu(distname) 


***GENERATE LEUVENSTEIN DISTANCE***
*SELECT MATCHES ACCORDING TO CRITERIA*
*1)similscore >= 0.75 and 2)levensteindistance <= 3
ustrdist distname distname1, generate(lev_dist)
keep if similscore >= 0.75 & lev_dist <=3 

keep distname distname1 year 
duplicates drop year distname, force
*no observations are duplicates
save "C:\Users\lucas\Downloads\additional_matches_1.dta", replace


**COMBINE WITH DISTRICT MATCHES**
use "C:\Users\lucas\Downloads\additional_matches_1.dta", clear
merge 1:1 distname year using "C:\Users\lucas\Downloads\ICRISAT_dataset.dta"
keep if _merge == 3
drop _merge
drop distname
rename distname1 distname 
duplicates drop distname year, force
merge 1:1 distname year using "C:\Users\lucas\Downloads\Shrug_data\panel"
keep if _merge==3
drop _merge 
drop id

**APPEND ADDITIONAL MERGES TO STARTING POINT MERGES**
append using "C:\Users\lucas\Downloads\matches_districts_1.dta"
drop id

***CREATE A BALANCED PANEL**
sort distname year
by distname: gen ndist=[_N]
keep if ndist == 14
drop ndist _merge
order distname year forest light

***NUMBER OF DISTRICTS*** 
di _N/14
*--> 411 districts 

**********************************************************************



***CREATE PANEL TO PERFORM REGRESSIONS***


**CREATE LOGARITHM OF FOREST, LIGHT AND CROP-SPECIFIC PRODUCTIVITY**
gen lrice_productivity = log(rice_productivity + 1)
gen lwheat_productivity = log(wheat_productivity + 1)
gen lsugar_productivity = log(sugar_productivity + 1)
gen lsorghum_productivity = log(sorghum_productivity + 1)


**CREATE DUMMY VARIABLES FOR YEARS**
local i = 1
forvalues i = 1(1)13{

	if inrange(`i',0,9){
			display `i'
		gen d0`i' = 1 if year == 200`i'
		replace d0`i' = 0 if year != 200`i'
	local i = `i' + 1
	}
	
	else{
		gen d`i' = 1 if year == 20`i'
		replace d`i' = 0 if year != 20`i'
	local i = `i' + 1
	}
	
}

**CREATE INTERACTION VARIABLES FOR POPULATION AND YEARS 2001 AND 2011 (ONLY ONES AVAILABLE)**
gen pop01 = d01 * population01
gen pop11 = d11 * population11
gen urban01 = d01 * urbanr01
gen urban11 = d11 * urbanr11


save "C:\Users\lucas\Downloads\district_level_panel.dta", replace

***SUMMARY STATISTICS***

use "C:\Users\lucas\Downloads\district_level_panel.dta", clear 

**CORRELATION**
correlate forest light
correlate lforest llight
*Correlation of 0.1456
*Correlation of 0.1323

summarize forest if year == 2000
summarize forest if year == 2013
summarize light if year == 2000
summarize light if year == 2013
summarize rice_productivity if year == 2000
summarize rice_productivity if year == 2013
summarize wheat_productivity if year == 2000
summarize wheat_productivity if year == 2013
summarize sorghum_productivity if year == 2000
summarize sorghum_productivity if year == 2013
summarize sugar_productivity if year == 2000
summarize sugar_productivity if year == 2013
summarize population01
summarize population11
summarize urban01
summarize urban11



********************************************************************
**EMPIRICAL ANALYSIS**
********************************************************************


***PANEL FE REGRESSIONS***
use "C:\Users\lucas\Downloads\district_level_panel.dta", clear 

**TELL STATA TO HANDLE PANEL DATA**
egen id = group(distname)
tsset id year 
*--> Dataset is STRONGLY BALANCED!

*OLS regression on aggregated data: 
*--> if regressors are correlated with ai this is a violation of MLR.4 and results in endogenous x and non-convergent estimator and OVB problem
*biased standard deviations due to correlation in composed errors

**OLS REGRESSION***
regress lforest llight d02-d13 
regress lforest llight d02-d13 pop01 pop11 urban01 urban11
*-->Coefficient on llight = 0.139 (positive & significant (pvalue <= 0.001))


**FD ESTIMATOR**

*CANNOT BE APPLIED TO VARIABLES THAT ARE A) CONSTANT ACROSS TIME OR B) VARY BY THE SAME DEGREE --> MEASURES THE DEGREE OF CORRELATION BETWEEN THE VARIATIONS IN Y AND VARIATIONS IN X OVER TIME 
*DOWNSIDE: STANDARD DEVIATION INCREASES 

*FUNDAMENTAL HYPOTHESIS: E(uit|xit,ai)=0
*--> Excluded the case where xit is the retarded dependent variable 


**FIRST DIFFERENCE ESTIMATOR**

xtset id year 
gen FDlforest = D.forest
gen FDllight = D.llight
regress FDlforest FDllight d02-d13 urban01 urban11 pop01 pop11

*TEST FOR JOINT SIGNIFICANCE OF YEAR DUMMIES*
test d02 d03 d04 d05 d06 d07 d08 d09 d10 d11 d12 d13


***BREUSCH-PAGAN TEST FOR HETEROSCEDASTICITY***
predict res, res
generate res2 = res^2
regress res2 FDllight d02-d13
*Reject H0: E(uit^2|xit)=0  
*--> errors are heteroscedastic 


*FOR t>2: NEED TO VERIFY WHETHER DELTAuit are autocorrelated AR(1)
***AR(1) TEST**
*LAGGED RESIDUALS*
tsset id year 
gen Lres = L.res 
regress res Lres 

*Coefficient on Lres is close to -0.5 --> prefer FE estimator
*Reject of H0: that errors are not serially correlated
*--> serial correlation 


**FIXED EFFECTS REGRESSIONS**
tsset id year


**FE REGRESSION WITH YEAR AND VILLAGE FE**
xtreg lforest llight lrice_productivity lwheat_productivity lsorghum_productivity d02-d13, fe cluster(id)


**FE REGRESSION WITH YEAR, VILLAGE AND CROP-YEAR FE**
xtreg lforest llight d02-d13 pop11 urban11 rice_productivity lsorghum_productivity wheat_productivity sugar_productivity, fe cluster(id)



**********************************************************************
	
