/*
=========================================================================================
Description: This is the do-file for the second lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 09/30/2022

Purpose: (1) Reshaping data sets (2) Merging data sets (3) Collapsing (4) Interpreting regression results.

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
global main = "$path/Lab 2"

* Checking
disp "$main"
dir "$main"

* Let's open our log
capture log using "$main/Log_`c(current_date)'", replace text
*=========================================================================================
* POLITY V: LEVEL OF DEMOCRACY
*=========================================================================================
import excel using "$main/polity iv/p4v2012_v2.xls", firstrow clear

drop if wb_code=="" // Dropping some missing that are useless

* We will focus only on the polity score and constrainst on the executive
keep year wb_code exconst polity polity2 // keeping the variables we need and dropping the rest

* Recall to always check for missings!
codebook exconst polity polity2

tab polity2, missing

* Recall to always check your data!

sum exconst polity polity2 // sum is short for summarize

tab polity // tab is short for tabulate

tab exconst

* Do yo notice something strange?

*=========================================================================================
* Let's code interregnum periods as a dummy
 
gen interregnum=(polity<-10) if polity!=.

* Let's recode exconst and polity using a loop
qui foreach var in exconst polity  {
	replace `var'=. if `var'<-10 
}

*
save "$main/polityIV.dta", replace

*=========================================================================================
* GDP
*=========================================================================================
insheet using "$main/GDP World Bank/GDP_Data.csv", clear names

* Renaming some of the variables
rename countryname name 
rename countrycode wb_code

drop seriesname seriescode // Dropping variables we won't need

bro // Browsing the data
drop if wb_code=="" // Dropping some missing that are useless

h reshape // Let's take a look at the reshape help file

* Reshaping the data from wide to long (homework: reshape it back to wide)

reshape long yr, i(name wb_code) j(year)

rename yr gdp_pk

* Converting from string to numeric
des gdp_pk
replace gdp_pk="" if gdp_pk==".." // Replacing .. with missing   

destring gdp_pk, replace force

sum gdp_pk, d // Once again here I'm using abbreviations


save "$main/gdp_data.dta", replace

*=========================================================================================
* MERGING DATA SETS
*=========================================================================================
u "$main/polityIV.dta", replace

h merge // Let's take a look at the reshape help file

merge 1:1 wb_code year using "$main/gdp_data.dta"

tab _merge 	// Let's evaluate the merged data
tab name if _merge==2  

drop _merge // dropping 

merge 1:1 wb_code year using "$main/intra_state_wars.dta", keep(master match) nogen

* Let's generate some labels 
lab var year 	"Year"
lab var polity 	"Polity score"
lab var polity2 "Polity score (inputed)"
lab var exconst "Executive constraints"
lab var gdp_pk 	"GDP per-capita (constant USD, 2015=100)"

lab var intra_state_war "Intra-state war in 5-year period"
lab var deaths_pyear_intra "Avg. deaths per year"

* Keeping most recent data because we have more obsservations

keep if year>1979 & year<2011 // In your papers you would need to justify this choice

* Replacing years of no war (missing values in this case) with zero
replace intra_state_war=0 if intra_state_war==.

tab intra_state_war

*=========================================================================================
* RUNNING A REGRESSION
*=========================================================================================
/* 
We will run a regression of the form: Y=a+b D+e
Y: outcome
D: is the treatment or main independent variable
a: is the constant or intercept
b: is the coefficient associated to the variable DATA
e: is the error term

b is the change in y when x increases in one unit: b*(x' - x) -> b*(101 - 100) = b

In this case Y is the GDP per-capita and D is the level of democracy
*/

reg gdp_pk polity2 

/*
Notes: 

A. SS – These are the Sum of Squares associated with the three sources of variance, Total, Model and Residual. 
	SSTotal: The total variability around the mean.  Sum(Y – Ybar)^2. 
	SSResidual:  The sum of squared errors in prediction. Sum(Y – Yhat)^2. 
	SSModel: The improvement in prediction by using the predicted value of Y over just using the mean of Y. 
	Sum(Ypredicted – Ybar)^2. 

Another way to think of this is the SSModel is SSTotal – SSResidual. Note that the SSTotal = SSModel + SSResidual.  

Note that SSModel / SSTotal is equal to the value of R-Square. This is because R-Square is the proportion of
	the variance explained by the independent variables.

B. df – These are the degrees of freedom associated with the sources of variance.  
	The total variance has N-1 degrees of freedom, because one degree of freedom is lost to the constant. One more df is lost for every indepdendent variable.
	Thus residuals degress of freedome are N-1-K where K is the number of regressors.

C. MS – These are the Mean Squares, the Sum of Squares divided by their respective DF.

D. Number of obs – This is the number of observations used in the regression analysis.

E. F and Prob > F – The F-value is the Mean Square Model divided by the Mean Square Residual 
	, yielding.  
	The p-value associated with this F value is very small. 
	These values are used to answer the question 
	"Do the independent variables reliably predict the dependent variable?". 

F. R-squared – R-Squared is the proportion of variance in the dependent variable (income) 
	which can be predicted from the independent variables (highschool). 

G. Adj R-squared – Adjusted R-square.  As predictors are added to the model, each predictor will 
	explain some of the variance in the dependent variable simply due to chance. 
	One could continue to add predictors to the model which would continue to improve the ability 
	of the predictors to explain the dependent variable, although some of this increase in R-square 
	would be simply due to chance variation in that particular sample.  
*/

reg gdp polity2 intra_state_war


*=========================================================================================
* Five year averages for regressions
gen period=.
forvalues i=1/`=(2010-1970)/5+1' {
	replace period=1970+`i'*5 if year<=1970+`i'*5 & year>1970+`=`i'-1'*5 
}
tabstat year, stat(min max) by(period) // Command for exploring descriptive stats by value

h collapse // Take a look at the help file

collapse polity polity2 exconst interregnum gdp_pk deaths_pyear_intra intra_state_war, by(wb_code period)

bro // Check out our new data

reg gdp polity2 intra_state_war


log close

/*
This website provides an excellent guide of everything Stata:

https://stats.idre.ucla.edu/stata/
*/

*=========================================================================================
* Replicating some the results in the regression table
reg gdp polity2 

ereturn list

* Total sum of squares
disp e(mss)  + e(rss)

* Degrees of freedom
disp e(df_m) // The number of regressors
disp e(N) - e(rank) // The number of observations minus the number of parameters 

* R-squared	
disp 1 - e(rss)/(e(mss)+e(rss))
disp e(mss)/(e(mss)+e(rss))

* F-test for model's significance

* F-statistic
disp e(mss)/e(rss)*e(df_r)/e(df_m) 

* Compute p-value for F-test
disp Ftail(e(df_m),e(df_r),e(mss)/e(rss)*e(df_r)/e(df_m))
 *=========================================================================================
* HOMEWORK: understand the commands below and what are they doing to the data set
insheet using "$main/correlates of war/Intra-StateWarData_v4.1.csv", clear

* Renaming and generating some useful variables
rename ccodea ccode 
gen year=startyear1

gen waryears=endyear1-startyear1

* Expand data set (useful for type of data like Correlates of War)

expand waryears+1

qui bys warnum ccode:  gen dup = cond(_N==1,0,_n)

* Replace years in duplicate observations
replace year=year+dup[_n-1] if dup>=2

* Total deaths
foreach var in sideadeaths sidebdeaths{
	replace `var'=. if `var'<0
}

* Calculating the deaths per year

bysort warnum ccode: egen totdeaths_intra=sum(sideadeaths+sidebdeaths)
bysort warnum ccode: egen max_dup=max(dup)
bysort warnum ccode: g    deaths_pyear_intra=totdeaths_intra/max_dup

* Keep relevant variables
keep warnum warname wartype ccode startyear1 endyear1 year waryears intnl totdeaths_intra deaths_pyear_intra

 * War type
tab wartype, gen(wartype_)
rename wartype_1 forcentralctrl
rename wartype_2 forlocalissue
rename wartype_3 forregionalint
rename wartype_4 intercommunal

* Intra state war
gen intra_state_war=(forcentralctrl==1 | forlocalissue==1 | forregionalint==1 | intercommunal==1) if wartype!=. 

* 
drop forcentralctrl-intercommunal

preserve
	clear
	import excel using "$main/country_codes.xlsx", firstrow clear
	ren cow_code ccode
	tempfile bridge
	save `bridge'
restore
merge m:m ccode using `bridge', nogen 
drop if wb_code==""
duplicates drop wb_code year, force 

/* 
What would you do with the data that did not match? What about duplicates?

Which countries are these? Is there a pattern?

Is there a way to fix this without losing data?

This is a pretty common problem you will face with your own data sets.
*/

* I will only keep a few variables I want for our exercise
keep wb_code year intra_state_war deaths_pyear_intra

save "$main/intra_state_wars.dta", replace


