*****************************************
****INFRASTRUCTURE****
**EXPLOIT TIMING OF ROAD CONSTRUCTION**
*****************************************

*author: Luca Schmidt*

*********************
**DATA PROCESSING**
*********************

**GENERAL SETTINGS**
clear all


**ANCILLARY DATA**
use "C:\Users\lucas\Downloads\Shrug_data\ancillary_data\shrug-v1.3.samosa-ancillary-dta\shrug_ancillary", clear 


**MERGE WITH FOREST DATA**
merge 1:1 shrid using "C:\Users\lucas\Downloads\Shrug_data\forest_cover_data\shrug-v1.3.samosa-vcf-dta\shrug_vcf_wide"
keep if _merge == 3
drop _merge 


*CREATE VARIABLE FOR FOREST COVER* 
local i = 0
forvalues i = 0(1)14{

	if inrange(`i',0,9){
			display `i'
		gen forest_200`i' = total_forest200`i'/num_cells
		gen lforest_200`i' = ln(forest_200`i' + 0.01)
		gen l_forest_200`i' = ln(forest_200`i' + 1)
		drop total_forest200`i' max_forest200`i'
	local i = `i' + 1
	}
	
	else{
		gen forest_20`i' = total_forest20`i'/num_cells
		gen lforest_20`i' = ln(forest_20`i' + 0.01)
		gen l_forest_20`i' = ln(forest_20`i' + 1)
		drop total_forest20`i' max_forest20`i'
	local i = `i' + 1
	}
	
}


**RESHAPE TO LONG**
reshape long forest_ lforest_ l_forest_ , i(shrid) j(year) 
rename forest_ frst_cover
rename lforest_ lfrst_cover
rename l_forest_ lforest
drop if year >= 2015
drop if year <= 2001


**DROP VARIABLES**
keep shrid year road_comp_date frst_cover lfrst_cover lforest


**EXTRACT DATE OF NUMERIC STORED STRING**
 tostring road_comp_date, generate(road_date) force format(%td)
 gen lastpart = substr(road_date,6,4)
 drop if missing(road_comp_date)
 drop road_comp_date road_date
 
 
**CREATE INDICATOR WHETHER VILLAGE HAS BEEN CONNECTED TO PAVED ROAD IN YEAR**


destring lastpart, gen(date)
format year %10.0g
gen road = 0
replace road = 1 if date <= year


**MAKE SURE ALL VILLAGES HAVE NO ROAD IN 2002**
**RECEIVE ROAD BY 2014**
**DROP OBSERVATIONS OF THESE GROUPS**
drop if road == 1 & year == 2002
drop if road != 1 & year == 2014
sort shrid
by shrid: gen ngroup=[_N]
keep if ngroup == 13
drop ngroup


**HOW MANY VILLAGES/TOWNS ARE LEFT IN THE SAMPLE**
di _N/13 
*-->55 659 observations


**GENERATE NUMERIC IDENTIFIER FOR EVERY GROUP OF SHRIDS**
**TO PERFORM REGRESSION**
encode shrid, gen(id)


**SAVE AS VILLAGE-ROAD CONSTRUCTION PANEL**
save "C:\Users\lucas\Downloads\village_road_panel.dta", replace


*******************

***REGRESSIONS***

*******************


***PANEL FE REGRESSION***
use "C:\Users\lucas\Downloads\village_road_panel.dta", clear 


**SET STATA TO HANDLE PANEL DATA**
**STRONGLY BALANCED PANEL**
tsset id year 


**PANEL FE REGRESSION**
**CLUSTER AT VILLAGE LEVEL TO ACCOUNT FOR SERIAL CORRELATION--> gives larger standard error**
xtreg lfrst_cover road, fe 
xtreg lfrst_cover road, fe cluster(id)
*--> Coefficient on road is .225974 meaning that access to a road increases tree cover by 22.6% 


**GENERATE STATE*YEAR FE**
 gen state_id = substr(shrid,4,2)
 destring state_id, gen(state_id_new)
 egen state = group(state_id_new)

 
 **COMMAND CREATES DUMMIES FOR BOTH STATES AND YEARS AND ALL INTERACTIONS OF THE TWO**
 xi: gen i.state*i.year
 
 
 **REGRESSION WITH VILLAGE FE AND STATE-YEAR INTERACTION**
 xtreg lfrst_cover road _IstaXyea*, fe cluster(id)
 *--> Coefficient is substantially smaller (.0016)
 
 xtreg lforest road _IstaXyea*, fe cluster(id)
 
  
**TEST FOR JOINT SIGNIFICANCE OF INTERACTION TERMS**
testparm _IstaXyea*
*Prob > F = 0.0000 interaction terms are jointly significant

*********************************************************************



