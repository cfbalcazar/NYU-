/*
=========================================================================================
Description: This is the do-file for the fifth lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 10/21/2022

Purpose: (1) DID (2) RDD.

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
global main = "$path/Lab 5"

* Checking
disp "$main"
dir "$main"

* Let's open our log
capture log using "$main/Log_`c(current_date)'", replace text

*=========================================================================================
* DIFFERENCES IN DIFFERENCES
*=========================================================================================
/*
Recall that in differences-in-differences (DID) we observe observations that are subject
to the treatment in a particular time period. Thus we need a variable that identifies
if the unit of observation was treated during the whole time period (hence it is in the 
treatment group), and then a variable for time.

The set-up for a DID looks like this:

Y=alpha+beta_3 D*time D*T + beta_1*D + beta_2*T +e

Assume there are only two periods, and that the treatment kicks in at T=1.

We only focus on beta_3, which is the DID estimate:

beta_3 = E(Y|D=1, t=1) - E(Y|D=0, T=1)-[E(Y|D=1, T=0)-E(Y|D=1 , T=0)]

Notice this effect is the difference between two differences - hence the name DID.

*/

* We will use some standard hospitals dataset to motivate DID

u "$main/hospdd.dta", clear

* Notice that some hospitals are treated starting in April - as new procedures are introduced
tab month procedure

* Let's generate the treated and time of treatment data
bys hospital: egen treated = max(procedure)
gen time = (month>3) if month!=. // Recall to always be sure you are not inputing a zero to a missing

* First thing we want to do is have an evaluation of the trends
h preserve // preserve and restore works for loading data back
preserve
	collapse satis, by(month treated)
	#delimit ;
		twoway (line satis month if treated==1, lc(gs0)) (line satis month if treated==0, lc(gs0) lp("-")),
		title("") ytitle("Patient satisfaction") xtitle("Month")
		legend(label(1 "Treated") label(2 "Control"))
		xlabel(1 "J" 2 "F" 3 "M" 4 "A" 5 "M" 6 "J" 7 "J")
		xline(3) graphregion(color(white)) ;
	#delimit cr
restore

*=========================================================================================
eststo clear
* Running the regression
eststo: reg satis i.treated##i.time,  vce(cluster hospital) // no controls 

* Adding fixed effects
eststo: reghdfe satis i.treated##i.time,  vce(cluster hospital) absorb(hospital) 

* Adding covariates: should we add frequency of hospital visit as a control?

* We observe the results for the bet!
esttab using "$main/regression_results_DID.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(1.treated#1.time) replace 
eststo clear

* Evaluating the parallel trends assumption
quietly: eststo: reghdfe satis i.treated##i.month,  vce(cluster hospital) absorb(hospital) 
#delimit ;
	coefplot, keep(1.treated#*.month)
	title("") ytitle("Patient satisfaction") xtitle("Month")
	xlabel(1 "J" 2 "F" 3 "M" 4 "A" 5 "M" 6 "J" 7 "J")
	vertical
	yline(0)  xline(3, lc(gs9) lp("-"))  graphregion(color(white)) ;
#delimit cr

*=========================================================================================
* ANOTHER EXAMPLE WHERE THINGS FAIL USING DATA OF MILITARY INTERVENTION ON ILLICTI CROPS
* A HEARTS AND MINDS STORY

clear all

* Load the database 
u "$main/DID_colombia_illicit_crops.dta", clear

* Create a global with all the controls for the regression 
global controls male age educ raza_0 raza_1 raza_2 raza_3 raza_4 raza_5  

eststo clear
* Running the regression
eststo: reg illicit_crop treatment i.treatment##i.time,  vce(cluster mun_code) // no controls 

* Adding covariates
eststo: reg illicit_crop i.treatment##i.time $controls,  vce(cluster mun_code) 

* Adding municipality fixed effects
eststo: reghdfe illicit_crop i.treatment##i.time $controls,  vce(cluster mun_code) absorb(mun_code) 

* Showing only results for the treatment
esttab using "$main/regression_results_DID.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(1.treatment#1.time) replace 
eststo clear

* Did you notice something strange?! The parallel trends assumption does not hold!!!
* Probably we need to control for many more confounders.


*=========================================================================================
* RREGRESSION DISCONTINUITY DESIGN 
*=========================================================================================
/*
In regression discontinuty design (RDD) the treatment is a threshold value. This can be a formal
rule, like: obtaining 50% of the vote you win the election; if your SAT is above the 90%
percentile you get a scholarship. This can also a be a geographical threshold such as a border
 - this is referred to as spatial RDD. It can also be a time threshold, such as the occurrence of an event - this is referred to as RDD in time.
 
What is important to RDD is to restrict yourself to observations that are very close to the
threshold to have an all else equal comparison.

The set-up for a RDD looks like this:

Y=alpha+beta D+ delta_1f(R-c)+delta_2f(c-R)+e

where f is a function for the bandwidth (c), around the threshold (R), and D=1 if the 
observation is above the threshold.

Beta in this case is the local average treatment effect, because is restrict to those people within the bandwidth.
*/

* We will data from Devin and Sekhon (2011) on close elections generating incumbency advantage.

use "$main/RDD_data.dta",clear 

replace DPctPrv=DPctPrv-50
replace DPctNxt=DPctNxt-50
* First let's add some labels
lab var DPctPrv  "Margin of victory for demcorat in 1st election"
lab var DPctNxt "Margin of victory for democrat in run-off"


* Let's plot the data
#delimit ;
	twoway (scatter DPctNxt  DPctPrv if DPctPrv<0, msize(vsmall) msymbol(circle_hollow) mc(gs9)) 
	(scatter DPctNxt  DPctPrv if DPctPrv>0, sort mcolor(blue) msize(vsmall) msymbol(circle_hollow) mc(gs6)) 
	(lfitci DPctNxt  DPctPrv if DPctPrv<0, lcolor(red) msize(small) lwidth(medthin) lpattern(solid)) 
	(lfitci DPctNxt  DPctPrv if DPctPrv>0, lcolor(blue) msize(small) lwidth(medthin) lpattern(solid)), 
	ytitle("% vote for democrat (run-off)")  xtitle("% vote for democrat (1st election)") 
	graphregion(color(white)) legend(off);
#delimit cr

* Let's look at the binned graph
qui: cmogram DPctNxt  DPctPrv, cut(0) scatter line(0) lfitci 

* Let's define the bandwidth - look at Cattaneo's work on this regard; we will do it the easy way
gen local_sample=(abs(DPctPrv)<5) if DPctPrv!=. // I'm using 5% of margin of victory

* Let's look once again at the restricted binning plot
qui: cmogram DPctNxt  DPctPrv if local_sample==1, cut(0) scatter line(0) lfitci

* Obtain f(R-c) and f(c-R)
reg DPctNxt  DPctPrv if local_sample==1 & DPctPrv<0
predict f_polynomial if  local_sample==1 & DPctPrv<0, xb

reg DPctNxt  DPctPrv if local_sample==1 & DPctPrv>0
predict f_polynomial_right if  local_sample==1 & DPctPrv>0, xb
replace f_polynomial=f_polynomial_right if local_sample==1 & f_polynomial==.

* Define treatment
gen WinDemPrv=(DPctPrv>0) if local_sample==1 

* Define outcome
gen WinDemNxt=(DPctNxt>0) if local_sample==1 

* Run your regression!
reg WinDemNxt i.WinDemPrv##c.f_polynomial if local_sample==1, cluster( StICPSR) nocons

*There are other more complicated graphs and if your research gets there we can work on them together

*=========================================================================================
/* NOTES REGARDING RDDs

There are some packages that run RD design, but they are hard to implement and hard to interpret (https://rdpackages.github.io/rdrobust/). You can check them out an use them if you want.

Recall that you need to perform identification checks:

* 1. What if we arbitrarily change the threshold?
	* Run a placebo RD, lets say there is a jump somewhere else
	* run the above code while subtracting 5 for example from the running variable
	* redefine running variable and replace the treatment
	* so you see there is no treatment effect
	* you can try a bunch of diff placebos and show there is no effect
	
* 2. Check for sorting
	* you can show there is no sorting arround the cutoff
	* with the density command you show if data is balanced around the cutoff or if there is sorting
	* this is a little hard, so we will get to the McCrary test when your research demands it
	
* 3. Placebo regressions on pre-treatment covariates
	* run the above code replacing the outcome with a covariate
	* you show that covariates are not varying at the treatment, only the outcome is!
/* 


More information here:
https://mixtape.scunning.com/09-difference_in_differences
https://mixtape.scunning.com/06-regression_discontinuity

*/
