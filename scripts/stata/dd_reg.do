*********************************************************************************
*																			
* PROJECT: 	Nature in Lockdown										
*																			
* PURPOSE: 	Diff-in-diff results    
*																			   				 							
*																			
* AUTHOR:  Raahil Madhok, madhokr@mail.ubc.ca											
*																				
********************************************************************************
*===============================================================================
*SET ENVIRONMENT
*===============================================================================
// Settings
clear all
pause on
cap log close
set more off
set maxvar 10000
set matsize 10000

//Set Directory Paths
local root 	"/Users/rmadhok/Dropbox/ebird_lockdown"
*===============================================================================

* Clean
foreach file in dd_t20_2trips dd_t20_5trips dd_t20_10trips dd_all_2trips {
	
	* Read
	import delimited "`root'/data/`file'.csv", clear
	
	* Clean
	encode county, gen(dist_id)
	encode observer_id, gen(user_id)
	encode state, gen(state_id)
	gen stationary = (protocol == "Stationary")
	
	* Save
	tempfile `file'
	save  ``file'', replace
}

* Top 20, 2 trips
use "`dd_t20_2trips'", clear

eststo m1: reghdfe s_richness treatpost treatment post ///
	duration rain temperature num_observers hotspot_km  ///
	stationary weekend, a(dist_id hour) vce(r)
	
	estadd local pcp "2 Trips"
	estadd local location "Top 20"
	estadd local dist_fe "$\checkmark$"
	estadd local hour_fe "$\checkmark$"
	
* Top 20, 2 trips, nocontrols
use "`dd_t20_2trips'", clear

reghdfe s_richness treatpost treatment post ///
	duration rain temperature stationary weekend, ///
	a(dist_id hour) vce(r)
	
	estadd local pcp "2 Trips"
	estadd local location "Top 20"
	estadd local dist_fe "$\checkmark$"
	estadd local hour_fe "$\checkmark$"
	
* Top 20, 5 trips
use "`dd_t20_5trips'", clear

eststo m2: reghdfe s_richness treatpost treatment post ///
	duration rain temperature num_observers hotspot_km ///
	stationary weekend, a(dist_id hour) vce(r)
	
	estadd local pcp "5 Trips"
	estadd local location "Top 20"
	estadd local dist_fe "$\checkmark$"
	estadd local hour_fe "$\checkmark$"
	
* Top 20, 10 trips
use "`dd_t20_10trips'", clear

eststo m3: reghdfe s_richness treatpost treatment post ///
	duration rain temperature num_observers hotspot_km ///
	stationary weekend, a(dist_id hour) vce(r)
	
	estadd local pcp "10 Trips"
	estadd local location "Top 20"
	estadd local dist_fe "$\checkmark$"
	estadd local hour_fe "$\checkmark$"
	
* All cities, 2 trips
use "`dd_all_2trips'", clear

eststo m4: reghdfe s_richness treatpost treatment post ///
	duration rain temperature num_observers hotspot_km ///
	stationary weekend, a(dist_id hour) vce(r)
	
	estadd local pcp "2 Trips"
	estadd local location "All Cities"
	estadd local dist_fe "$\checkmark$"
	estadd local hour_fe "$\checkmark$"

* SURE

use "`dd_t20_5trips'", clear // use dd_t20_5trips for robustness
fvset base 5 dist_id
fvset base 5 user_id
fvset base 0 hour

eststo m2020: regress s_richness post duration rain temperature ///
	num_observers stationary weekend hotspot_km ///
	i.user_id i.dist_id i.hour if year == 2020

eststo m2019: regress s_richness post duration rain temperature ///
	num_observers stationary weekend hotspot_km  ///
	i.user_id i.dist_id i.hour if year == 2019

suest m2020 m2019, vce(r)
eststo m6: lincomest [m2020_mean]post - [m2019_mean]post 

	estadd local dist_fe "$\checkmark$"
	estadd local hour_fe "$\checkmark$"
	estadd local user_fe "$\checkmark$"
	estadd local pcp "5 Trips"
	estadd local location "Top 20"
kk
	
* Tabulate
esttab m1 m6 m2 m3 m4 using "`root'/tables/dd_reg.tex", replace ///
	stats(pcp location dist_fe hour_fe user_fe N r2, ///
	labels(`"Partip. Constraint"' `"Location Constraint"' ///
	`"District FEs"' `"Hour FEs"' `"User FEs"' `"N"'  `"\(R^{2}\)"') ///
	fmt(0 0 0 0 0 0 3)) mgroups("Preferred" "Robustness", ///
	pattern(1 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span ///
	erepeat(\cmidrule(lr){@span})) star(* .1 ** .05 *** .01) nocons nomtitles ///
	nonotes booktabs label se b(%5.3f) se(%5.3f) width(\hsize)
eststo clear

	






