/*
=========================================================================================
Description: This is the do-file for the first lab of IR Honors in politics at NYU.

Authors: Felipe Balcazar

Last modified: 09/23/2022

Purpose: (1) Show how to use a do-file (2) Show basic commands.

Notes: this do-file requires the stata dataset prz_modified.dta
=========================================================================================
*/
clear all // Clears the workspace. An alternative is drop _all.
set more off , permanently // To avoid unwanted pauses during the execution of the code.  
capture log close // Closes any previously open log. 
set maxvar 5000 // For datasets with many variables.

/*
Notes:

A. capture or cap, for short, is a very useful command that bypasses errors. For instance,
open stata and write log close - it will return an error. Thus albeit the error may still 
exist, the program will not stop when we add capture. 

B. You can run code selections with ctrl+d 

C. You can answer almost any question in stata by "asking google." Answers to most questions
are well documented online.
*/

* =========================================================================================
* SOME BASICS
* =========================================================================================


* =========================================================================================
* 1. Help!

* Many times we don't know what a command does or the syntaxis to run it, thus use the help

help display

* =========================================================================================
* 2. Display, locals and globals

* With display you can print some (one line) output into the console, try it for yourself:

* Display text, note we add quotation marks
display "Hello world!"

display 5 + 5  

* A local keeps everything in the command line in memory only until the program or do-file ends. 

local operation = 5+5

* You access locals with `'

display `operation'

* A global keeps everything in the command line in memory for the entire session.

global operation = 5 + 5

* You access globals with $

display $operation

* Let's try it with a word

local long_word  = "pneumonoultramicroscopicsilicovolcanoconiosis"
global long_word = "`long_word'"

display "`long_word'"
display "$long_word"

/*
Note: 

Overall locals and globals fulfill the purpose of storing information that you may want
to retrive when running code (local) or during a session (global).
*/

* =========================================================================================
* 3. Scalars and variables

* Scalars store numeric information (or constants) during the working session
scalar alpha= 2

disp alpha 

disp alpha*alpha

/*
Note:

I'm using a shortened version of display here. In fact when you look at the help of a 
command you may see a portion of the command underlined, this indicates how the command may 
be shortened.
*/

* =========================================================================================
* Sets the current directory
cd "/Users/balcazar/Dropbox/NYU/Dissertation - phase/Things on the side/IR Honors/Lab 1/"
* Check path
loc dir="`c(pwd)'"
display "`dir'"
* Check files in path
dir  

* Log file: it records what we do during the session or until we close the log
capture log using "Log_`c(current_date)'", replace text

/*
Notes:

A. It is preponderant to set the path before starting a work session because this will tell
Stata where to find the files and where to save them. I have some neat tricks for this as 
well at the end of the do-file.

B. The remaining of the .do file runs without changing the directory. This code should automatically read the directory. 
*/
* =========================================================================================
* WORKING WITH DATA
* =========================================================================================


* =========================================================================================
* Load a stata data set
use prz_modified.dta, clear

/*
Notes:

A. Often we may like to load data comming from .XLSX, .CSV, .TXT or other file extensions.
 - For excel files you can use the command: import excel.
 - For csv files you can use the command: insheet.
 - For txt files you can use the command: import delimited.
 - For dbf files you can use the command: import dbase

B. Stata may not read certain file extensions such as .sav or .rdata, which are fairly common.
However these data can be converted to Stata format using the R software and the foreign
package.

C. I have saved the same database in different formats. As pracitice homework, try
loading the different databases using the different file extensions.
*/

* You can also save it (I'm using a different name just for practice)
save prz_modified_class.dta, replace
* =========================================================================================
* browse command: view your data/variables in a new window
browse name regime

* describe command: describes the data in the results' screen, note the variable types
describe name regime

* codebook command: provides detailed information about variables, such as missings
codebook regime

* sort command: sorts data in ascending order by the value of the chosen variable
sort gdppc, stable

* sorting by multiple variables
sort country year, stable

* gsort command: allows for ascending or descending sort (just put "-" in front for descending)
gsort -gdppc  

* make sorted lists using quantifiers
* the following line gives us the top ten countries with the highest GDP
list name gdppc in 1/10

* tabulate command: creates tabulation tables
tabulate hinst

* can also tabulate without the labels
encode hinst, gen(en_hinst)
tabulate hinst, nolabel

* can also tabulate including missings (good practice)
tabulate majgov, missing

* additionally, we can create cross tabs for two variables
* also add options to give us percentages
tab hinst britcol, column

* summarize command: summarizes variabe to give you mean, median, etc.
summarize gdppc

* use options to provide more detailed summary output
summarize gdppc, detail

* using a conditional
summarize gdppc if britcol==1 // Notice the double equal

* by group
bys britcol: summarize gdppc // bys means by sort

* histogram command: creates a histogram of variable
* remember we can set the number of bins or width of bins as well using options
hist gdppc, bin(10)

* graph bar command: creates a bar chart using two variables
graph bar (mean) gdppc, over(hinst)

* graph box command: creates box plots
* can also make horizontal box plots using graph hbox
graph box gdppc 

* correlate command: performs a correlation analysis
correlate gdppc agehinst

* scatter command: creates a scatterplot of the data
scatter gdppc agehinst

* generate command: creates a new variable
* the following line creates a new variable called "system" that is identical to hinst
generate system=hinst

* replace variable values
* in the following lines we replace the values of a variable to obtain the amount of growth
generate growth_amnt=ginig // Of course we don't want the gini values here
replace growth_amnt=growth*gdppc

* conditional generate command: creates a new variable based on a condition
* the following line creates a new variable called "high_growth" 
generate high_growth=(growth>10) if growth!=.

* recode command: recodes variable
* the following line recodes system to be a dichotomous variable
recode en_hinst (0 1 2=0) (3 4 5=1)

* shorter version
recode en_hinst (0/2=0) (3/5=1)

* recoding missing data
* in the following lines we found which observations have negative income per capita and recode them
list name country gdppc if gdppc<0
recode gdppc min/0=.
recode investment -999=.


*==================================================================================
* FORMATTING DATA	
*==================================================================================

* Labeling a variable
label var high_growth "Country has high growth"

label define yesno 0 "No" 1 "Yes"

label values high_growth yesno

* Transforming into string
tostring high_growth, replace force
bro high_growth


* Transforming into numeric
destring high_growth, replace force
bro high_growth

* Encoding - converts a string into numeric with labels
encode name, gen(name_codes)

*==================================================================================
* REGRESSION
*==================================================================================

* bivariate regression: regression analysis using two variables
* a good source to know how to read the regression table: http://www.ats.ucla.edu/stat/stata/output/reg_output.htm
regress gdppc agehinst

* obtain fitted values
predict yhat

* obtain residuals
predict e, resid

* check to see if y = yhat + resid
gen ypred = yhat + e
corr gdppc ypred

* twoway scatter and lfit command: creates a scatterplot with fitted line
* also adding options to make more complet looking graphs
twoway (scatter gdppc agehinst) (lfit gdppc agehinst), title(Effect of Regime Age on Income per Capita) ///
ytitle(Income per Capita) xtitle(Age of Current Regime)

* the correlate command gives correlations
corr gdppc agehinst openk

* can also get the covariance matrix by adding cov at the end
corr gdppc agehinst openk, cov

* graph matrix command: depicts pairwise correlations in one plot
graph matrix gdppc agehinst openk
graph export graph_matrix.png, replace

* mulitple regression: regression analysis using more than one independent variable
* income per capita = a + b1(regime age) + b2(trade openness) + e
regress gdppc agehinst openk

log close

/*
Note:

The first lab finishes here.

*/

* =========================================================================================
* =========================================================================================
* =========================================================================================
* =========================================================================================
* more advanced syntaxis: to practice at home - focus on the code, what is it doing?

* Instead of using cd for the path, I will use globals
global root = "/Users/balcazar/Dropbox/"
global path = "$root/NYU/Dissertation - phase/Things on the side/IR Honors"
global main = "$path/Lab 1"

* Checking
disp "$main"
dir "$main"

* I'm going to create a simulated data set
clear
set obs 1000

* I create some groups with a more advanced generate command

generate rannum = uniform() // Obtain draws from uniform distribution
egen group_id = cut(rannum), group(10) // Group ID

* I also generate and individual ID by group
bys group_id: gen id=_n // Can you figure out what is this doing?

* Generate a random variable with normal distribution, mean=10, sd=1.5
gen x=rnormal(10,1.5)
gen x_obs=x // an alternative is ussing clonevar, try it.

* Actually why don't we add some noise to x_obs     
bys group_id: egen u=mean(rnormal(100,5))
replace x_obs=x_obs+u

* Then I create y, which is a function of x plus some noise
gen y=10+5*x+runiform()

* Let's create a simple graph with residuals
quietly reg y x_obs u
predict e, resid
predict yhat

* Let's obtain the mean of the residuals 
qui: sum e
return list // When you look at the help file some information is stored in lists
disp r(mean) 

* Let's create a fancy graph and also save it

sum e
* This line of code is very long but we can break it with three backslashes
twoway (scatter e yhat, mc(red) msize(small)), /// 
	title("Residuals plot" "(Simulation with N=`r(N)')") /// 
	ytitle(Residual value) xtitle(Fitted values) ///
	graphregion(color(white)) 

* Second line of code
graph export "$main\residuals.png", replace

* Alternatively we can use a delimiter (it does not allow comments)
* I will modify the graph even more

#delimit ;
	sum e;

	twoway (scatter e yhat, mc(red)  m(Oh)), 
		title("Residuals plot" "(Simulation with N=`r(N)')", color(black)) 
		ytitle("Residual value", size(medlarge)) 
		xtitle("Fitted values", size(medlarge)) 
	graphregion(color(white)) ;

	graph export "$main\residuals.png", replace;
#delimit cr

/*
This website provides an excellent guide of everything Stata:

https://stats.idre.ucla.edu/stata/
*/

   
 



