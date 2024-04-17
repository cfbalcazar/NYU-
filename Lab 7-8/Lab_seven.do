/*
=========================================================================================
Description: This is the do-file for the seventh lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 02/08/2023

Purpose: Tables and graphs.

=========================================================================================
*/
clear all // Clears the workspace. An alternative is drop _all.
set more off , permanently // To avoid unwanted pauses during the execution of the code.  
capture log close // Closes any previously open log. 
*=========================================================================================
/*
Set-up the paths. We will use today my favorite way to do this but
recall that you can also use the cd command.
*/
global root = "/Users/balcazar/Dropbox/"
global path = "$root/NYU/Dissertation - phase/Things on the side/IR Honors"
global main = "$path/Lab 8"

* Checking
disp "$main"
dir "$main"

* Let's open our log
capture log using "$main/Log_`c(current_date)'", replace text

*=========================================================================================
* GENERAL RECOMMENDATIONS
*=========================================================================================
/*
* Any table and graph needs should not have more than what the reader needs to know to understand and evaluate results.
 
* The graph must be self-contained - meaning that in your manuscript the reader
would be able to understand it even if he/she doesn't read the manuscript.

* The graph should be clear and easy to understand, especially if a reader prints
it in monocromatic. This means that you should consider using shades of gray
and different markers to produce clear graphs.
*/

*=========================================================================================
* For the exercise we will analyse the impact of precipitation shocks on public opinion

u "$main/IHDS_LONG_CLIMATE_SS.dta", clear

* Transformations
recode hh_urban (2=0) // converted to rural
forvalues i=1/4 {
	gen ln_precip_`i'=ln(precip_`i'+2)
}

* Re-label
lab var irrigation "Irrigation" 
lab var avg_light "Nighlight"

* Outcomes - confidence
local confi_govt 	  = "confidence_stategovt confidence_villagegovt confidence_politicians"

* Outcomes - conflict/cooperation
local conflict_coop  = "conflict_community conflict_amount cooperation_water"

* Controls
local controls 	= "irrigation avg_light head_sex hh_urban" 

global desvars `confi_govt' `conflict_coop' `controls'

*=========================================================================================
* SUMMARY STATISTICS: USING TABOUT

* check also: http://tabout.net.au/docs/gallery.php

* The limitation of tabout is its inability of creating standard layouts with
* multiple variables, however it is quite intutitive.
*=========================================================================================
cap erase "$main/summary_tabout.csv"

* Running loop for each variable
qui foreach var of global desvars  {
	tabout period using "$main/tables/summary_tabout.csv", ///
		sum c(mean `var' median `var' min `var' max `var' N `var') ///
		style(csv) append 
}
*=========================================================================================
* SUMMARY STATISTICS: USING ESTTAB (RECOMMENDED)

* check also: http://repec.org/bocode/e/estout/estpost.html#estpost102


* Tabstat is quite powerful for handling multiple variables in complex layouts.
* The layouts however are somewhat standard, and thus personalized layouts are
* difficult to create. Moreover, it does not take into account individuals missing.
*=========================================================================================
cap ssc install estout

* Generating descriptive stats with common sample (no missing in $desvars)
estpost tabstat $desvars, stat(mean sd min max N) listwise ///
	columns(statistics) 
esttab using "$main/tables/summary_estout.csv", /// 
	 cells("mean sd min max Obs") label nodepvar replace 

* Generating descriptive stats with common sample by period
estpost tabstat $desvars, by(period) stat(mean sd min max N) listwise ///
	columns(statistics) 
esttab using "$main/tables/summary_estout.csv", /// 
	 cells("mean sd min max Obs") label nodepvar replace 
	 
	 
/*
You can also do a loop similar to the one we did for tabout	
*/
*=========================================================================================
* SUMMARY STATISTICS: USING MATRICES (ADVANCED, BUT RECOMMENDED)

* Using a matrix gives the most flexibility and it can be highly personalized.
* The limitation is that it is not easy to get used to it.
*=========================================================================================

preserve
	cap matrix drop descriptives // Drops any existing matrix called descriptives

	local i=0 // Starts counter for renaming
	
	* Looping throught the variables to obtain basic statistics
	foreach var of global desvars  {
		local i=`i'+1
		qui sum `var', detail // summarize the variable var
		
		* Store the variable number, mean, SD, minimum, maximum, median, and N obs. in a matrix called descriptives
		matrix descriptives = (nullmat(descriptives) \ /// we need to start with null matrix
		`i', r(mean), r(sd), r(min),r(max),r(p50),r(N),0) // zero to identify full sample 
	}

	* RUN THIS SEPARATELY BECAUSE A BUG IN STATA DOES NOT CARRY OVER THE `name_`i'' local
	
	* Obtaining the names of the variables
	foreach var of global desvars  {
		local i=`i'+1
		local name_`i'="`:var l `var''" 
		disp "`name_`i''"
	}

	* Since everything is stored in a matrix, we load the matrix and format it
	clear

	* Column names
	matrix colnames descriptives = variable mean sd min max median obs ID

	* Load descriptives
	svmat descriptives, names(col)
	tostring variable, replace

	* Replace variable names
	local i=0
	foreach var of global desvars  {
		local i=`i'+1
		replace variable="`name_`i''" if variable=="`i'" 
	}
	* Rounding
	des *, varlist // creates a local with the variable names
	qui foreach var of varlist `r(varlist)'  {
		cap replace `var'=round(`var',0.01)
	}
	* Saving
	outsheet using "$main/tables/summary_matrix.csv", replace comma
restore

*=========================================================================================
* DESCRIPTIVE GRAPHS

* We already covered how to generate graphs, but below you will find some 
* additional examples to refresh your memory.
*=========================================================================================

* Generate index
egen confidence_govt=rowmean(confidence_politicians confidence_stategovt confidence_villagegovt)

* Correlation between precipitation and confidence in government
preserve
	collapse confidence_govt ln_precip_1, by(district period)
	replace confidence_govt=confidence_govt*100
	#delimit ;
	twoway (scatter confidence_govt ln_precip_1, mc(gray%20) ) 
		(lfit confidence_govt ln_precip_1, lc(gs0)) 
		, graphregion(color(white))
		xtitle("Ln Precipitation shock" "(mm of rainfall normalized to 1SD)", size(medlarge))  
		ytitle("% of confidence in State Government",size(medlarge))
		legend(off);
		graph export "$main/figures/motivating_graph_precip.png", replace;
	#delimit cr
restore

* Empirical distribution
#delimit ;
	twoway (kdensity ln_precip_1, lc(gs0) )
		(kdensity ln_precip_2, lc(gs9) ) 
		(kdensity ln_precip_3, lc(gs0) lp("-") )
		(kdensity ln_precip_4, lc(gs9) lp("-") )
		, graphregion(color(white))
		xtitle("Ln precipitation shock", size(medlarge))  
		ytitle("Density",size(medlarge))
		legend(label(1 "t-1") label(2 "t-2") label(3 "t-3") label(4 "t-4") size(medlarge));
		graph export "$main/figures/kernel_density_temp.png", replace;
#delimit cr

*=========================================================================================
* CREATING REGRESSION TABLES

* http://repec.sowi.unibe.ch/stata/estout/

* We have already covered this multiple times, so I won't spend a lot of time 
* on this - nonetheless I will show how you can use excel to format your tables 
* nicely.
*=========================================================================================
global controls 	= "irrigation avg_light"
global demo         = "head_sex hh_urban i.hh_caste i.hh_religion" 

global FEs      = "period idh" 
global clusters = "district"
*=========================================================================================
* SET OF OUTCOMES: CONFIDENCE IN GOVERNMENT
*=========================================================================================
cap erase  "$tables/results.csv"
eststo clear
eststo:  reghdfe confidence_stategovt ln_precip_1, absorb($FEs) cluster($clusters)
eststo:  reghdfe confidence_stategovt ln_precip_1 $controls, absorb($FEs) cluster($clusters)
eststo:  reghdfe confidence_stategovt ln_precip_1 $conrols $demo, absorb($FEs) cluster($clusters)

esttab using "$main/tables/results.csv", star(* 0.10 "**" 0.05 "***" 0.01) ///
	title("Outcome: `indep'") ///
	b(3) se(3) r2(3) sfmt(3) obslast replace keep(ln_precip_1) l

*=========================================================================================
* Interaction effects	
eststo clear

eststo:  reghdfe confidence_stategovt c.ln_precip_1##i.hh_urban, absorb($FEs) cluster($clusters)

* Joint hypothesis testing for the full interaction effect
lincom ln_precip_1+c.ln_precip_1#1.hh_urban
estadd scalar int_effect    = r(estimate)
estadd scalar se_int_effect = r(se)
local divide=abs(r(estimate)/r(se))
if `divide'>2.56 {
	local mstars="***"
}
else {
	if `divide'>1.96 {
		local mstars="**"
	} 
	else {
		if `divide'>1.64 {
			local mstars="*"
		}
		else {
			local mstars=""
		}
	}
}
estadd local stars="`mstars'"

* Controlling for covariates
eststo:  reghdfe confidence_stategovt c.ln_precip_1##i.hh_urban $controls, absorb($FEs) cluster($clusters)

* Joint hypothesis testing for the full interaction effect
lincom ln_precip_1+c.ln_precip_1#1.hh_urban
estadd scalar int_effect    = r(estimate)
estadd scalar se_int_effect = r(se)
local divide=abs(r(estimate)/r(se))
if `divide'>2.56 {
	local mstars="***"
}
else {
	if `divide'>1.96 {
		local mstars="**"
	} 
	else {
		if `divide'>1.64 {
			local mstars="*"
		}
		else {
			local mstars=""
		}
	}
}
estadd local stars="`mstars'"

* Controlling for all covariates
eststo:  reghdfe confidence_stategovt c.ln_precip_1##i.hh_urban $controls $demo, absorb($FEs) cluster($clusters)

* Joint hypothesis testing for the full interaction effect
lincom ln_precip_1+c.ln_precip_1#1.hh_urban
estadd scalar int_effect    = r(estimate)
estadd scalar se_int_effect = r(se)
local divide=abs(r(estimate)/r(se))
if `divide'>2.56 {
	local mstars="***"
}
else {
	if `divide'>1.96 {
		local mstars="**"
	} 
	else {
		if `divide'>1.64 {
			local mstars="*"
		}
		else {
			local mstars=""
		}
	}
}
estadd local stars="`mstars'"
		
esttab using "$main/tables/results.csv", star(* 0.10 "**" 0.05 "***" 0.01) ///
	title("Outcome: `indep'") scalars(int_effect se_int_effect stars) ///
	b(3) se(3) r2(3) sfmt(3) obslast append keep(ln_precip_1 1.hh_urban#c.ln_precip_1) l

*=========================================================================================
* MARGINS PLOT

* https://stats.oarc.ucla.edu/stata/faq/how-can-i-graph-the-results-of-the-margins-command-stata-12/

* It computes marginal effects. This is a very useful command for plotting the results from interaction effects. Or the effect of a coefficient evaluated at a certain value of the independent variable. 

* This command should not be used to plot the results from multiple regressions. If you want to have margins plots for continuous variables in the same graph, please check with me - they are somewhat tricky.

*=========================================================================================
reghdfe confidence_stategovt c.ln_precip_1##c.hh_urban $controls $demo, absorb($FEs) cluster($clusters)

* Plotting the results from the regression for a discrete moderator
margins, dydx(ln_precip_1) at(hh_urban=(0(1)1))   post
marginsplot,  recast(scatter)  graphregion(color(white))  yline(0) xscale(range(-.5 1.5)) ///
title("") ytitle("Effect of precipitation shock")  xtitle("Area of residence")

graph export "$main/figures/interaction_discrete.png", replace
			
/*
Homework: how can we change the axis labels so that they say urban and rural?
*/	
reghdfe confidence_stategovt c.ln_precip_1##c.myers $controls $demo, absorb($FEs) cluster($clusters)

* Plotting the results from the regression for a continuous moderator
margins, dydx(ln_precip_1) at(myers=(2(5)31))   post
marginsplot, plotopts(lc(gs0) mc(gs0)) recastci(rarea) ciopt(color(gray%50))  graphregion(color(white))  yline(0) ///
title("") ytitle("Effect of precipitation shock")  xtitle("Myers index")

graph export "$main/figures/interaction_continouous.png", replace
			
*=========================================================================================
* COEFPLOT

* http://repec.sowi.unibe.ch/stata/coefplot/getting-started.html

* It is a flexible command and computes marginal effects for multiple regressions.  

*
*=========================================================================================
cap ssc install coefplot

eststo clear
* SET OF OUTCOMES: CONFIDENCE IN GOVERNMENT
eststo:  reghdfe confidence_stategovt ln_precip_1, absorb($FEs) cluster($clusters)
eststo:  reghdfe confidence_stategovt ln_precip_1 $controls, absorb($FEs) cluster($clusters)
eststo:  reghdfe confidence_stategovt ln_precip_1 $conrols $demo, absorb($FEs) cluster($clusters)


#delimit ;
	coefplot (est1, keep(ln_precip_1) ciopts(lc(gs0 gs0)) mc(gs0))
			 (est2, keep(ln_precip_1) ciopts(lc(ebblue ebblue)) mc(ebblue)) 
			 (est3, keep(ln_precip_1) ciopts(lc(gray gray) lp("solid" "-")) mc(gray) mfc(none)),
		legend(order(1 "No controls" 5 "Controls" 8 "All controls"))
		yline(0) graphregion(color(white)) vertical levels(95 90)
		ytitle("Effect of an increase in rainfall")  xtitle("Model") 
		coeflabels(ln_precip_1 = "Preicipitation in t-1");
#delimit cr
graph export "$main/figures/models_plot.png", replace

*=========================================================================================
* Interaction effects: First lets create a program to be able to obtain the information we need from the linear hypothesis
cap prog drop ltest
prog def ltest, eclass
	syntax [anything]
	lincomest  `0' 
	tokenize `0'
	matrix betass=e(b)
	matrix colnames betass = "`1'"
	mat list betass
	ereturn repost b = betass, rename
end

eststo clear
* SET OF OUTCOMES: CONFIDENCE IN GOVERNMENT
eststo:  reghdfe confidence_stategovt c.ln_precip_1##i.hh_urban $conrols $demo, absorb($FEs) cluster($clusters)
eststo: ltest ln_precip_1 + c.ln_precip_1#1.hh_urban

eststo:  reghdfe confidence_stategovt c.ln_precip_2##i.hh_urban $conrols $demo, absorb($FEs) cluster($clusters)
eststo: ltest ln_precip_2 + c.ln_precip_2#1.hh_urban

eststo:  reghdfe confidence_stategovt c.ln_precip_3##i.hh_urban $conrols $demo, absorb($FEs) cluster($clusters)
eststo: ltest ln_precip_3 + c.ln_precip_3#1.hh_urban

#delimit ;
	coefplot (est1, keep(ln_precip_1) ciopts(lc(gs0 gs0)) mc(gs0) offset(-0.15)) 
			 (est2, keep(ln_precip_1) ciopts(lc(gs9 gs9)) mc(gs9) offset(0.15))
			 (est3, keep(ln_precip_2) ciopts(lc(gs0 gs0)) mc(gs0) offset(-0.15)) 
			 (est4, keep(ln_precip_2) ciopts(lc(gs9 gs9)) mc(gs9) offset(0.15))
			 (est5, keep(ln_precip_3) ciopts(lc(gs0 gs0)) mc(gs0) offset(-0.15)) 
			 (est6, keep(ln_precip_3) ciopts(lc(gs9 gs9)) mc(gs9) offset(0.15)),
		legend(order(5 "Rural" 8 "Urban"))
		yline(0) graphregion(color(white)) vertical levels(95 90)
		ytitle("Effect of an increase in rainfall")  xtitle("Model") 
		coeflabels(ln_precip_1 = "t-1"
					ln_precip_2 = "t-2"
					ln_precip_3 = "t-3");
#delimit cr
graph export "$main/figures/interaction_models_plot.png", replace
