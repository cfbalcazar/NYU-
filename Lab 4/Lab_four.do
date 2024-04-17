/*
=========================================================================================
Description: This is the do-file for the fourth lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 10/14/2022

Purpose: (1) Inference (2) Heterogenous effects.

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
global main = "$path/Lab 4"

* Checking
disp "$main"
dir "$main"

* Let's open our log
capture log using "$main/Log_`c(current_date)'", replace text
*=========================================================================================
u "$main/data_lab.dta", clear

* Merging the dataset with oil production
merge 1:1 wb_code year using "$main/oil_production.dta", keep(master match) nogen

encode wb_code, gen(ccode) // numeric identifier

* Generate the logarithm
gen ln_gdp_pk=ln(gdp_pk+1)

* Setting your data for panel analysis
xtset ccode year


* Generating lagged variables
gen lag_gdp_pk=L.ln_gdp_pk // First lag
gen lag_intra_state_war=L2.intra_state_war // L2 means second lag

*=========================================================================================
/* Hypothesis testing

Consider

Y_i=alpha+beta*D_i+e_i

Define a null hypothesis and the alternative hypothesis. For example

H0: beta=0
H0: beta!=0

The previous hypothesis evaluates statisitical significance. That is, is the effect of D on the outcome (Y) statistically different from zero? 
*/
*=========================================================================================

reghdfe polity2 lag_gdp_pk lag_intra_state_war i.year, absorb(ccode)

/* Test coefficient lag_gdp_pk is 0
H0: lag_gdp_pk=0
Ha: lag_gdp_pk!=0
*/
test lag_gdp_pk=0

/* Estimate linear combination of coefficients
H0: lag_gdp_pk=0
Ha: lag_gdp_pk!=0
*/
lincom lag_gdp_pk + lag_intra_state_war

/* Equality of coefficients
H0: lag_gdp_pk=lag_intra_state_war
Ha: lag_gdp_pk!=lag_intra_state_war
*/
lincom lag_gdp_pk - lag_intra_state_war
lincomest lag_gdp_pk - lag_intra_state_war // alternative command

*=========================================================================================
* HETEROSCEDASTICITY
*=========================================================================================
/*
Consider our usual regression equation:

Y=a+beta_1*X+e

If the variability of the random disturbance (e) is different across elements of X,  if Var(e)!=constant, we say then that error term is heteroskedastic. e might be heteroskedastic if for instance var(e)=g(X), where g(.) is some function. That is, the variability of the treatment is unequal across the range of values of a covariate. 

The presence of heteroskedasticity implies the absence of homoskedasticity, violating the assumptions for the Gauss-Markov theorem. This means that OLS estimators are not BLUE because their variance is not the lowest of all other unbiased estimators. 

Heteroscedasticity does not cause OLS estimates to be biased, although it can cause biased standard errors. Therefore inferences obtained from data analysis are suspect---and likely wrong. Most of the times the standard errors are too small, increasing the chance of type 1 error - rejecting the null hypothesis when it is true. 
*/ 

eststo clear
eststo: reghdfe polity2 lag_gdp_pk lag_intra_state_war i.year, absorb(ccode)

* Robust standard errors solve heteroskedasticity
eststo: reghdfe polity2 lag_gdp_pk lag_intra_state_war i.year, vce(robust) absorb(ccode)

/*
CLUSTER STANDARD ERRORS:

A treatment may affect similarly certain observations, such as those belonging to the same group: the same country, the same municipality, the same state, the same classroom! In other words the observations react similarly to the treatment when they belong to the same group.

This means that the amount of units of obersvation we have is actually smaller, because each observation contributes less than "one unit of information." 

We usually cluster at the level of the treatment assignment. If there is clustered sampling, we cluster at the level of the cluster in the sampling frame.
*/ 


eststo: reghdfe polity2 lag_gdp_pk lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode)


* Showing only results for the treatment
esttab using "$main/regression_results_SE.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(lag_gdp_pk) replace 

/*
 Note that the Standard errors are bigger.
*/
*=========================================================================================
* INTERACTION EFFECTS; MODERATORS; HETEROGENOUS EFFECTS
*=========================================================================================
/*
An interaction effect explores the heterogenous effect of a regressor, say D, on our outcome Y. The variable that is used for the interaction, say X, is called a moderator.

Therefore when people use the terms interaction effects, moderators or heterogenous effects they are basically referring to the same thing!

We say that X is a moderator because it moderates the effect of D on Y. Importantly X must always occur before D, that is: X->D and not the other way around!

In a regression setting this looks like

	Y=alpha+beta_1*D+beta_2*X+beta_3*D*X+e

beta_3 is called the interaction effect. D and X are called the component terms of the interaction D*X, and they should always be included in your regression.

If X is a dummy variable (the easiest way to get this), then we have the following (assuming D is also a dummy):

beta_1=E(Y|D=1,X=0)-E(Y|D=0,X=0)

beta_1 + beta_2=E(Y|D=1,X=1)-E(Y|D=0,X=1). This is called the full interaction effect and usually what we interpret when we talk about an interaction effect.

beta_2 = E(Y|X_1=1,X_2=1)-E(Y|X_1=0,X_2=1)-[E(Y|X_1=1,X_2=0)-E(Y|X_1=0,X_2=0)] 
This is the difference in the effect of D between the groups in your sample when they take different values for X - or the differential effect.
 
Interaction effects are tricky at first. Keep always in mind the difference between beta_2 (the interaction effect) and beta_1+beta_2 (the full interaction effect).
*/

* Generate dummy for oil producer
gen oil_producer=(oil_production>0) if oil_production!=.

* Using stored results
h sum
sum oil_production, d
gen high_oil=(oil_production>r(p50)) if oil_production!=.

* Adding labels to values
label define yesno 0 "No" 1 "Yes"
label values oil_producer yesno
label values high_oil yesno

* Taking out some big oil producers
gen bigol=(wb_code=="USA" | wb_code=="SAU" | wb_code=="RUS" | wb_code=="IRN") 
drop if bigol==1 // I take out the big oil 

* Generating lagged variables
gen lag_oil_production=ln(L2.oil_production+1) // L2 means second lag
gen lag_high_oil=L2.high_oil // L2 means second lag
label values lag_high_oil yesno


eststo clear
* Running our previous regression
eststo: reghdfe polity2 lag_gdp_pk lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode)

* Controlling for oil producer
eststo: reghdfe polity2 lag_gdp_pk lag_intra_state_war lag_high_oil i.year, vce(cluster ccode) absorb(ccode)

* Interaction effect
eststo: reghdfe polity2 lag_gdp_pk c.lag_gdp_pk#i.lag_high_oil lag_high_oil lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode) // c. is telling stata we deal with a continuous variable and i. with a categorical variable

* Alternative way to write this
eststo: reghdfe polity2 c.lag_gdp_pk##i.lag_high_oil lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode) 

* Showing only results for the treatment
esttab using "$main/regression_results.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(lag_gdp_pk 1.lag_high_oil#c.lag_gdp_pk ) replace 

* Hypothesis testing
		
lincom lag_gdp_pk // This is the effect of gdp per capita conditional on being a high oil producer
	
lincom lag_gdp_pk + c.lag_gdp_pk#1.lag_high_oil // This is the effect of schooling conditional on high oil producer = 1	
	
* Let's use margins to plot this
reghdfe polity2 c.lag_gdp_pk##i.lag_high_oil lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode) 

#delimit ;
	margins, dydx(lag_gdp_pk) at(lag_high_oil=(0(1)1)) post;
	marginsplot, recast(scatter) yline(0) xscale(range(-.5 1.5))
	title("Interaction effect") ytitle("Effect of GDP on Democracy") 
	xtitle("High oil producer")
	graphregion(color(white)) ;
#delimit cr

*=========================================================================================
* Continuous moderator (TO TAKE HOME)
*=========================================================================================

reghdfe polity2 c.lag_gdp_pk##c.lag_oil_production lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode)

lincom lag_gdp_pk // This is the effect of gdp per capita conditional on being a high oil producer
	
lincom lag_gdp_pk + 9.680407*c.lag_gdp_pk#c.lag_oil_production // This is the effect of schooling conditional on high oil producer = 9.680407	

reghdfe polity2 c.lag_gdp_pk##c.lag_oil_production lag_intra_state_war i.year, vce(cluster ccode) absorb(ccode)
#delimit ;
	margins, dydx(lag_gdp_pk) at(lag_oil_production=(0(1)19)) post cont;
	marginsplot, recast(line) plot1opts(lcolor(gs0)) ciopts(recast(rarea) color(gs9))
	title("") ytitle("Effect of GDP on Democracy") xtitle("Log of oil production (USD)") level(95)
	graphregion(color(white)) ;
#delimit cr

	