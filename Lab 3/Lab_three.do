/*
=========================================================================================
Description: This is the do-file for the third lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 10/07/2022

Purpose: (1) Fixed effects (2) Descriptive statistics (3) Regression table.

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
global main = "$path/Lab 3"

* Checking
disp "$main"
dir "$main"

* Let's open our log
capture log using "$main/Log_`c(current_date)'", replace text
*=========================================================================================
u "$main/data_lab.dta", clear

encode wb_code, gen(ccode) // numeric identifier

* To produce tables for descriptive statistics
ssc install tabout // https://ianwatson.com.au/stata/tabout_tutorial.pdf

* To produce nice regression tables 
ssc install estout // http://repec.sowi.unibe.ch/stata/estout/

* Command for panel data
ssc install reghdfe // http://scorreia.com/software/reghdfe/

* Setting your data for panel analysis
xtset ccode year

*=========================================================================================
* MODERNIZATION HYPOTHESIS
*=========================================================================================

#delimit ;
	twoway (lfitci polity2 gdp_pk)(scatter  polity2 gdp_pk) (lfit polity2 gdp_pk), 
		title("", color(black)) 
		ytitle("Democracy scores", size(medlarge)) 
		xtitle("GDP", size(medlarge)) 
	graphregion(color(white)) ;
#delimit cr

*=========================================================================================
* Variable transformations

#delimit ;
	twoway (kdensity gdp_pk, mc(red)  m(Oh)), 
		title("Empirical distribution of GDP per-capita", color(black)) 
		ytitle("Density", size(medlarge)) 
		xtitle("GDP", size(medlarge)) 
	graphregion(color(white)) ;
#delimit cr

/* 
	When a variable exhibits a high levle of skewness this can generate problems of influential observations in a regression. Hence we can take logarithms. 
*/
gen ln_gdp_pk=ln(gdp_pk+1)


/* 
If we want to log transform a variable (x) that contains zeroes, we can log transform it as log(x+1). This is a very common adjustment. 

If somebody wants to dig really deep into this you can use the inverse hyperbolic sine when you want to log-transform a variable. Since this can get tricky, I recommend it just for those that would like to read econometric theory. 
*/

#delimit ;
	twoway (kdensity ln_gdp_pk, mc(red)  m(Oh)), 
		title("Empirical distribution of GDP per-capita", color(black)) 
		ytitle("Density", size(medlarge)) 
		xtitle("GDP", size(medlarge)) 
	graphregion(color(white)) ;
#delimit cr

*=========================================================================================
* Let's check again

#delimit ;
	twoway (lfitci polity2 ln_gdp_pk)(scatter  polity2 ln_gdp_pk) (lfit polity2 ln_gdp_pk), 
		title("", color(black)) 
		ytitle("Democracy scores", size(medlarge)) 
		xtitle("GDP", size(medlarge)) 
	graphregion(color(white)) ;
#delimit cr


*=========================================================================================
* RUNNING REGRESSIONS

* Capturing time-variant confounders
reg polity2 L.ln_gdp_pk L2.intra_state_war

/*
	Here
	
	y=alpha+beta*ln(gdp_pk)+inter_state_war+epsilon
	
	Recall that the effect is interpreted as as change in 
	1 percent in gdp_pk leads to a change in beta in polity2.
	
	See notes at the end of the do-file.
*/

* Capturing time-invariant confounders
reg polity2 L.ln_gdp_pk L2.intra_state_war i.ccode 
/*
	We are exploting the variation within country.
*/

* Capturing non-monotonic trends
reg polity2 L.ln_gdp_pk L2.intra_state_war i.year i.ccode 
/*
	We are taking into account all other things that affect countries similarly overtime.
*/

*=========================================================================================
* RUNNING REGRESSIONS
*=========================================================================================

eststo clear // deletes previously save regressions

eststo: reg polity2 L.ln_gdp_pk // No controls
eststo: reg polity2 L.ln_gdp_pk L2.intra_state_war // Observable confounder
eststo: reg polity2 L.ln_gdp_pk L2.intra_state_war i.ccode // + time-invariant confounders
eststo: reg polity2 L.ln_gdp_pk L2.intra_state_war i.year // + trend
eststo: reg polity2 L.ln_gdp_pk L2.intra_state_war i.year i.ccode // + trend time-invariant confounders 
eststo: reghdfe polity2 L.ln_gdp_pk L2.intra_state_war i.year, absorb(ccode) // Best way to do it

* Showing all regressors - messy!
esttab using "$main/regression_results.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast replace 

* Showing only results for the treatment - nicey!
esttab using "$main/regression_results.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(L.ln_gdp_pk) replace 


*=========================================================================================
* ADDING ANOTHER MODEL TO OUR RESULTS
*=========================================================================================
* Five year averages for regressions
gen period=.
forvalues i=1/`=(2010-1970)/5+1' {
	replace period=1970+`i'*5 if year<=1970+`i'*5 & year>1970+`=`i'-1'*5 
}
tabstat year, stat(min max) by(period) // Command for exploring descriptive stats by value

h collapse // Take a look at the help file

collapse polity polity2 exconst interregnum gdp_pk deaths_pyear_intra intra_state_war, by(ccode period)

gen ln_gdp_pk=ln(gdp_pk+1)

bro // Check out our new data

eststo clear // deletes previously saved regressions

eststo: reg polity2 ln_gdp_pk // No controls
eststo: reg polity2 ln_gdp_pk intra_state_war // Observable confounder
eststo: reg polity2 ln_gdp_pk intra_state_war i.ccode // + time-invariant confounders
eststo: reg polity2 ln_gdp_pk intra_state_war i.period // + trend
eststo: reg polity2 ln_gdp_pk intra_state_war i.period i.ccode // + trend time-invariant confounders 
eststo: reghdfe polity2 ln_gdp_pk intra_state_war i.period, absorb(ccode) // Best way to do it

esttab using "$main/regression_results.csv", /// 
	star(* 0.10 "**" 0.05 "***" 0.01) /// Statistical significance
	b(3) se(3) r2(3) sfmt(3) ///
	obslast keep(ln_gdp_pk) append


log close

/*
This websites provide an excellent guide of everything Stata:

https://stats.idre.ucla.edu/stata/
https://www.statalist.org/
*/


 *=========================================================================================
* BRIEF NOTES FOR HOME
* =========================================================================================
* PROPERTIES OF LOG FUNCTION AND EXPONENTIAL FUNCTION

/*
In state you can log-transform a variable using log(x) or ln(x)

Recall as well:

log(a) if a<0 is not defined

log(0) is not defined

log(1) = 0

log(a*b) = log(a)+log(b)

log(a/b) = log(a)-log(b)

log(a^b) = b*log(a)

exp(0) = 1

exp(1) = 2.718 The value of the exponential constant

exp(a*b) = exp(a)^b for all b rational

exp(a+b) = exp(a)*exp(b)

exp(a-b) = exp(a)/exp(b)

log(exp(a)) = a = exp(log(a))
*/

disp log(100)

disp ln(100)

disp ln(100*50)-(ln(100)+ln(50))

disp ln(100/50)-(ln(100)-ln(50))

disp ln(100^5)-5*ln(100)

disp exp(0)

disp exp(1) // The value of the exponential constant

disp exp(10*(1/3))-(exp(10))^(1/3)

disp exp(10+5)-(exp(10)*exp(5))

disp exp(10-5)-(exp(10)/exp(5))

disp ln(exp(10)) - exp(ln(10))

disp ln(exp(10))


* =========================================================================================
* INTERPRETATION

* Constructing a simulation
drop _all
set obs 1000 
gen parent_schooling = 0+int((16-0+1)*runiform())  
gen schooling = 0+int((16-0+1)*runiform()) ///
	+0.5*runiform()*parent_schooling 
gen parent_income = parent_schooling*100 + rnormal(10,10)
gen randn=runiform()
egen gene_health = cut(randn), group(2) 
gen eps = rnormal() 
scalar alpha = 5   
scalar beta_1 = 0.5
scalar beta_2 = 0.2
scalar beta_3 = 0.001
scalar beta_4 = -10
gen income = alpha + beta_1*schooling + beta_2*parent_schooling + beta_3*parent_income + beta_4*gene_health + eps
reg income parent_schooling parent_income gene_health

lab var income "Person's income"
lab var	schooling "Number of years of education"
lab var	parent_schooling "Number of years of education of parent"
lab var	parent_income "Income of parent"
lab var	gene_health "Health of individual"

* For instance the schooling variable has zeroes
gen zeroes_schooling=(schooling==0) if schooling!=.
tab zeroes
* If we log transform without adding 1 those zeroes become missing
gen log_schooling_wzeroes=ln(schooling)
gen log_schooling_wozeroues=ln(schooling+1)
sum log_schooling*

* Let's use the simulated data
gen ln_schooling=ln(schooling+1)
gen ln_parent_schooling=ln(parent_schooling+1)

* Univariate regression: lin-lin regression
reg schooling parent_schooling

disp "One more year of parent schooling increases children schooling by " _b[parent_schooling]

* Univariate regression: log-lin regression
reg ln_schooling parent_schooling

disp "One more year of parent schooling increases children schooling by " _b[parent_schooling] "%"

* Univariate regression: lin-log regression 
reg schooling ln_parent_schooling

disp "One % increase in parent schooling increases children schooling by " _b[ln_parent_schooling]

* Univariate regression: log-log regression
reg ln_schooling ln_parent_schooling

disp "One % increase in parent schooling increases children schooling by " _b[ln_parent_schooling] "%"

/*
Practice: Would the previous estimated coefficients be biased?
Why? Why not?

If so, what regression would you need to run to obtain an unbiased
coefficient?
*/


