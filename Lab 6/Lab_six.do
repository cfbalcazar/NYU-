/*
=========================================================================================
Description: This is the do-file for the sixth lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 11/04/2022

Purpose: (1) Instrumental Variables Design.

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
global main = "$path/Lab 6"

* Checking
disp "$main"
dir "$main"

* Let's open our log
capture log using "$main/Log_`c(current_date)'", replace text

*=========================================================================================
* INSTRUMENTAL VARIABLES
*=========================================================================================
/*
In instrumental variables we run a two stage regression using an instrument in the first-stage. That is, in the first stage we run the treatment against the instrument (Z)

D=alpha_1+beta_1*Z+ delta_1*X+e_1

then we optain the predicted values of the treatment; that is

D_predicted=alpha_1+beta_1*Z+ delta_1*X

Using these values we run the regression we are interested in - the second-stage regression:

Y=alpha_2+beta_2*D_predicted+ delta_2*X+e_2

Importantly, the instrument (Z) must satisfy two properties:

a) Relevance: The instrument and the treatment need to be HIGHLY related.   

b) Exogeneity/exclusion restriction: The instrument cannot affect the outcome through any other channel that is not the treatment. The best instruments are those that are exogenous.

Note: If you want to dig deeper into the econometric theory, you can take a look at the LATE theorem; the instrumental variables estimator is the Local Average Treatment Effect.

If these conditions are satisfied, then beta_2 identify the effect of the treatment for those that react (monotonically) to the treatment. This is a smaller group. In this case we say beta_2 identifies that Local Average Treatment Effect or LATE.

Impotantly, you should have a broad sense of the direction of the bias according to your theory. Also, you should have a good sense of who your compliers are. 

Instrumental Variables is useful because it solve:

* Problem of omitted variable bias. 
* Reverse causality.
* Endogenous measurement error (same as omitted variable bias).

*/

* For the first exercise we will analyse the effect of GDP on conflict

u "$main/mss_repdata_feb07.dta", clear

cap ssc install ivreg2 // main package for instrumental variables

h ivreg2


eststo clear
* First let us estimate the regression
eststo: regress war_prio_nar gdp_g democ i.ccode i.year_actual, vce(cluster ccode)  

* Instrument: Growth in rainfall, Control:laged growth in rainfall and democracy

regress gdp_g GPCP_g GPCP_g_l democ i.ccode i.year_actual, vce(cluster ccode)
predict gdp_g_hat, xb   

* Estimate the second stage regression

eststo: regress war_prio_nar gdp_g_hat GPCP_g_l democ i.ccode i.year_actual, vce(cluster ccode) 

* For the purposes of creating the table
qui: eststo:  regress gdp_g GPCP_g GPCP_g_l democ i.ccode i.year_actual, vce(cluster ccode)

esttab using "$main/regression_results_IV.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(gdp_g GPCP_g) replace 
eststo clear

* VERY IMPORTANT! You should not do your 2-stage regression by hand! The standard errors will be wrong because of the Moulton problem - i.e., you need to take into account the uncertainty of the first stage into the second stage. Hence computing this by hand, your standard errors will very likely be underestimated, increasing the chances of type I error. Estimating the moulton by hand in a multivariate regression and/or with clustered standard errors is quite difficult without a high level of programming experience and understanding of econometric theory.

*=========================================================================================
* USING IVREG2
*=========================================================================================

* First let us estimate the regression

eststo: ivreg2 war_prio_nar GPCP_g_l democ (gdp_g=GPCP_g) i.ccode i.year_actual, cluster(ccode) partial(i.ccode i.year_actual)

* Did you notice something strange?! The standard errors are bigger!

eststo: ivreg2 war_prio_nar GPCP_g_l democ (gdp_g=GPCP_g) i.ccode i.year_actual, cluster(ccode) partial(i.ccode i.year_actual) first

* Notice the F-test. This is a test of relevance. It needs to be at least greater than 10 (old standard by Stock and Yogo) - hopefully close to 100 or more.

esttab using "$main/regression_results_IV.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(gdp_g) append
eststo clear


*=========================================================================================
* EFFECT OF TRADE ON GDP GROWTH
*=========================================================================================

u "$main/pascali_data.dta", clear

eststo clear
* First let us estimate the regression

ivreg2 ln_GDPpc (ln_exportGDP= lpred_TOTAL_trad_5ys) dummy_C* dummy_Y*, ///
	cluster(country year) partial(dummy_C* dummy_Y*) noc // noc removes the constant

*=========================================================================================
/* NOTES REGARDING INSTRUMENTAL VARIABLES

You can perform some robustness checks:

* 1. Is the reduced form effect consistent with the LATE?
	* Run a regression of the instrument on the outcome
	* show that the effect your estimate is consistent.
	
* 2. Are the estimates estable or lower bounds? 
	* Add your pre-treatment controls one a time and evaluate 
	* the estability of your parameters. If they vary erratically  
	* this is a signal that there may be important unobservables to account for.
	* This is much less problematic if your estimate increases in size
	* and becomes more significant.

* 3. Other checks:
	* Look for associations between the instrument and things that
	* predict the outcome but should not be sensitive to the
	* instrument (like things that were determined prior to the
	* instrument).
	* Find subpopulations for which there should be no relationship
	* between the instrument and the outcome. If the instrument
	* predicts Y in this subsample, then you may have an exclusion
	* violation!
/* 
Common questions:

* Can I have more than one independent variable instrumented with the same instrument? Not in the same regression. You can potentially use an instrument for two potential treatments, but the assumptions for this are very strong! Consult with me if you want to do this.

* Can I have more than one instrument? Yes! 

* Can I instrument an interaction effect? How? Yes! Just add another instrument that is the interaction of your instrument with your moderator. Thus for instance in the first-stage you would have:

D=alpha_11+beta_11*Z+theta_11*Z*X+ delta_11*X+e_11

D*x=alpha_12+beta_12*Z+theta_12*Z*X+ delta_12*X+e_12

Use ivreg2!
 

More information here:
https://mixtape.scunning.com/07-instrumental_variables
http://scholar.harvard.edu/nunn/pages/data-0

*/
