clear

cls

use "HLFS_1percent.dta"	


/*
Sample STATA code to study the impact of Syrian immigration on Turkish Native labor market outcomes. The file uses a 1% sample of the 
Turkish Household Labor Force Survey. This sample goes over the 
following process:
- Difference-in-Difference without the use of a package
- Difference-in-Difference across multiple regions and time and continous variables
- STATA cluster options
- Bootstrapping
- Instrument Variable Analysis
- Placebo Analysis
- Synthetic Control
*/


*Keep age-range only working population

keep if age>=18 & age<=64

/*
Tabulate employment status, we intentionally want to include missing values
and nolabel so we can better generate the "employed" variable. 
*/

*******************************************************************************

tab emp_status, missing nolabel


*Since label is dropped we can easily filter by == 1

gen employed = 1 if emp_status == 1


replace employed = 0 if emp_status != 1


*We want to keep the missing values to employed
 
replace employed = . if emp_status == .


*tabulate employment status by employed to account non-Labor force participants

tab	emp_status employed, missing

*******************************************************************************

/*
Seperate in/formal workers by multiplying employment dummies with
social security dummies
*/

gen informal = employed * (soc_security==0)
gen formal = employed - informal


/*
Now will differentiate between types of workers. emp_type has four main 
categories: wageworkers, employers, selfemployed, and unpaid family worker.
*/

tab emp_type

tab emp_type, nolabel

*wageworker will be those who are employed and recieve a wage

gen wageworker = employed * (emp_type==1)

*selfemployed will those who are employed and pay themselves

gen selfemployed = employed * (emp_type==3)

*informal wageworkers are those who get paid a wage, and work in informal jobs

gen informal_wageworker = informal * wageworker

*formal wageworkers are those who get paid a  wage, and work in formal jobs
gen formal_wageworker = formal * wageworker

*formal wage found by multiplying formal dummy with wage

gen formal_wage = formal * wage

sum formal_wage

*replace non formal with missing values

replace formal_wage = . if formal==0

sum formal_wage

*******************************************************************************

/*
Now key variables of interest will be generated. Please note that there 
are easier ways to do this using packages, but doing it in the brute force 
manner allows a better understanding of the process. 
*/ 

/*

nuts2 is the level 2 regional breakdown of Turkey
12: Adana, Mersin
13: Kırıkkale, Aksaray, Niğde, Nevşehir, Kırşehir
24: Gaziantep, Adıyaman, Kilis
25: Şanlıurfa, Diyarbakır
26: Mardin, Batman, Şırnak, Siirt

*/

*the above are the regions in Turkey with the most Syrian refugees

gen treatment = (nuts2==12 | nuts2==13 | nuts2==24 | nuts2==25 | nuts2==26)

*define the post treatment to be after 20212

gen post = (year>=2012)

*Interact treatment regions with post years to generate treatment_post

gen treatment_post = treatment * post

*control for the non-linear relationship between age and wage, employment

gen age2 = age^2

/*
marital_status contains married, unmarried, divorce, widowed.
generating married only if married and not missing value
*/

gen married = (marital_status==2) if marital_status !=.

*create global control list, i.educ_attain as educ_attain is discrete

global controls age age2 married i.educ_attain

*******************************************************************************

table treatment_post if male==1, c(mean employed)

table treatment_post if male==0, c(mean employed)

table treatment_post if male==1, c(mean informal)


// reg employed treatment post treatment_post if male==1

// reg employed treatment post treatment_post if male==1 [pw=weight]

/*
Run the regression of employed on treatment, post treatment and the 
interaction of the two (the diff-in-diff coefficent). pw=weight is to calibrate
the weights of the cluster sampling to the true population weights of Turey. 
robust option is included to account for potential hetroskedasticity. The 
regressions without these commands are commented out above should one 
want to compare
*/

*male employed
reg employed treatment post treatment_post if male==1 [pw=weight], robust

*female employed
reg employed treatment post treatment_post if male==0 [pw=weight], robust

*male informal sector
reg informal treatment post treatment_post if male==1 [pw=weight], robust

*male formal sector
reg formal treatment post treatment_post if male==1 [pw=weight], robust

*female informal sector
reg informal treatment post treatment_post if male==0 [pw=weight], robust

*female formal sector
reg formal treatment post treatment_post if male==0 [pw=weight], robust

/*
We can also run regressions in the following manner using ##. ## 
includes treatment, post and treatment * post in the regression
*/ 

reg informal treatment##post $controls if male==1 [pw=weight], robust

reg informal treatment##post $controls if male==0 [pw=weight], robust

*******************************************************************************

/*
Now we will do the regression across multiple years and groups. Notice 
that now we only need to include the treatment_post variable as treatment and 
post are capture by the year and region fixed effects (i.year and i.nuts2) that
we now control for. 
*/

reg informal i.nuts2 i.year treatment_post $controls if male==1 [pw=weight], robust

reg formal i.nuts2 i.year treatment_post $controls if male==1 [pw=weight], robust

reg employed i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], robust

*We can gen nuts2_year to include every region in every year

egen nuts2_year = group(nuts2 year)

/*
We do this because the data we used is a cluster sample. If we define each 
cluster to be a region and a year then we get 243 clusters. It is important 
when regressions with Stata to specify that the data is cluster sampled. 
Consider individuals in Adana in 2004, and those in Istanbul 2004. The economic
shocks that impact Adana in 2004 will differ from Istanbul. If Stata does not 
know that these individuals are sampled from different clusters then there 
will be an underestimation in the variation. If you run two regressions one 
with clustering and one without you can observe the difference.
*/

reg employed i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], robust

reg employed i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], cluster(nuts2_year)

/*
Note that the standard errors of treatment_post increase as we choose less 
granular clusters. When we cluster only by the region level cluster size 
goes from 243 to 26, and standard error also increases. nuts1 is the highest 
regional level and has 12 clusters so it has the highest standard error
*/

reg informal i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], robust

reg informal i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], cluster(nuts2_year)

reg informal i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], cluster(nuts2)

reg informal i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], cluster(nuts1)

/*
Finally, notice that if I generate gran to cluster by age, region, and year 
I get a small standard error that is similar to the one I get when I don't 
cluster at all. This is because now the cluster size is about 100,000 so it is 
very granular. 
*/ 

egen gran = group(nuts2 year age)

reg informal i.nuts2 i.year treatment_post $controls if male==0 [pw=weight], cluster(gran)

*******************************************************************************

/*
Now analysis will done at the region-year level. preserve is used to not 
modify the original data when calculations are being done at the region level.
k is generated to be able to conut observations to be able to count the number 
of observations in each counted group.  
*/ 

preserve

gen k = 1

/*	
Here we will collapse (aggregrate) the data by calculating the mean of 
employed, formal, and informal for each nuts2 region and for each region. 
We will also count the amount of observations in each aggregrate group. 
pw=weights is still used for the same reason as mentioned above
*/

collapse (mean) employed formal informal (count) k [pw=weight], by(nuts2 year)

*Aggregrate data can observed by uncommenting the following codes

// save collapsed.dta, replace

// restore

// clear

// use collapsed.dta

// browse

// use HLFS_1percent.dta

*Now we run the regression using the same process as above

gen treatment = (nuts2==12 | nuts2==13 | nuts2==24 | nuts2==25 | nuts2==26)

gen post = (year>=2012)

gen treatment_post = treatment * post

reg informal i.nuts2 i.year treatment_post [pw=k], robust

restore

*******************************************************************************

/*
We will now do diff-in-diff using the migrant to population for each nuts2 
region in Turkey. The outcome of interest now goes from discrete to continous.

migrant_ratios_by_nuts2_long.dta contains the data containing these ratios 
by region and year. 
*/ 


*We first sort the data to ensure more efficent matching with the new data

sort nuts2 year

/*
In the merge command m:1 specifies many to one. This means that there can 
multiple data points that have the same year and region. We then specify the 
variables we want to merge (nuts2, year) and then the file that contains 
the information. Note: the .dta file should have the same variable name 
so the one to one matching can occur
*/

merge m:1 nuts2 year using migrant_ratios_by_nuts2_long.dta, keepusing("ratio")


*If you look at the merge tabulation the matched values split between pre and post

tab year _merge

/*
Note that merge has 3 values 

merge==1 : observations that only in the master dataset

merge==2 : observations that only in the dataset that will be merged

merge==3 : observations that exist in both (this is what we want)

*/

*By asserting we ensure that our data only contains merge==3

assert _merge==3 if year>=2012

drop _merge


*Replace ratio to be 0 if missing before 2012. If you look before, most 
*values before 2012 are missing anyways as refugees did not start coming yet

replace ratio = 0 if year<2012

egen nuts2year = group(nuts2 year)

*employed men
reg employed treatment_post i.nuts2 i.year $controls if male==1 [pw=weight], robust

reg employed ratio i.nuts2 i.year $controls if male==1 [pw=weight], robust

*employed women
reg employed treatment_post i.nuts2 i.year $controls if male==0 [pw=weight], robust

reg employed ratio i.nuts2 i.year $controls if male==0 [pw=weight], robust

*informal men

reg informal treatment_post i.nuts2 i.year $controls if male==1 [pw=weight], robust

reg informal ratio i.nuts2 i.year $controls if male==1 [pw=weight], robust

*employed men, using alternaitve diff-diff approaches

reg employed i.treatment##i.post $controls if male==1 [pw=weight], cluster(nuts2year)

reg employed ratio i.nuts2 i.year $controls if male==1 [pw=weight], cluster(nuts2year)

*employed women, using alternaitve diff-diff approaches

reg employed i.treatment##i.post $controls if male==0 [pw=weight], cluster(nuts2year)

reg employed ratio i.nuts2 i.year $controls if male==0 [pw=weight], cluster(nuts2year)

/*
Bootstrapping allows us to estimate the distribution of an estimator by 
resampling (wtih replacement) of the sampling data.

In this case, the confidence interval of the bootstrap results includes 0 
meaning that it we cannot reject the null hypothesis that the coefficent 
for ratio is 0. However, this is because of the fact that we sampled 
1% of the data
*/


//ssc install boottest

boottest ratio

*******************************************************************************

sort nuts2

merge m:1 nuts2 using region_data.dta

drop _merge

/*
By defining dep as a global variable if we want to change the variable that 
will be observed in the common trend easily. This is especially useful 
in cases where we are running multiple regressions at the same. 
*/

global dep formal_wage

*Baseline trend regression comparing ratio across regions and years

reg $dep ratio i.nuts2 i.year $controls if male==1 [pw=weight], robust

/*
Tests for paralell trends across regions and year
Note that # only interacts region and years (continous)
*/

reg $dep ratio i.nuts2 i.year i.region#c.year $controls if male==1 [pw=weight], robust

*Tests for linear trends across higher level regions (nuts1)
reg $dep ratio i.nuts2 i.year i.nuts1#c.year $controls if male==1 [pw=weight], robust

*Tests for paralell trends among each year individually (i.year is not continous)
reg $dep ratio i.nuts2 i.year i.region#i.year $controls if male==1 [pw=weight], robust

/*
Notice that when these regressions are ran, none of the coefficent values 
are satistically significant. This means that the paralell assumption holds. 
*/

*******************************************************************************

/*
It is important to note that there is a self selection 
endogeneity problem of immigration. Immigrants are more likely to go to 
regions where there is more economic activity. We can use to mitigate 
this problem. instrument_crosscountry_long.dta contains information 
about refugees in a region in its base year / total refugees in the base 
year * the total refugees in a given year. This instrument helps with the 
self selection problem as it uses the historical settlement of 
pattern for each region and time as an instrument
*/ 


*merge instrument data

sort nuts2 year

merge nuts2 using instrument_crosscountry_long.dta

tab year _merge

*Replace the instrument with 0 for pre-treatment years

replace inst_crosscountry = 0 if year<2012

*Comparing the two regressions we see very different coefficents for ratio

reg employed ratio i.nuts2 i.year i.nuts1#c.year $controls if male==0 [pw=weight], cluster(nuts2year)

ivregress 2sls employed (ratio=inst_crosscountry) i.nuts2 i.year i.nuts1#c.year $controls if male==0 [pw=weight], cluster(nuts2year) first

*Durbin,Wu-Hausman test of endogeneity

estat endogenous, forceweights

*Tests for the strength of instruments

estat firststage, forcenonrobust all

exit



*******************************************************************************
/*
Placebo analysis is an effective way to check the common trend assumption. 
We choose a year in the post-treatment and replace the ratio of the pre-treatment
values with the post-treatment ratio. We then run the main regression again. 
We expect the coefficent of interest to be 0, implying that the effect we find 
did not occur in the pre-treatment. 
*/

*In this case we pick the ratio from 2015

gen ratio15 = ratio if year==2015

*However, a lot of missing values generated

twoway (scatter ratio15 year)


sort nuts2 year

/*
By nuts2 region we replace the ratios of each region with the last observed
value of ratio15
*/

by nuts2: replace ratio15 = ratio15[_N]

*Now the missing values are also replaced

twoway (scatter ratio15 year)

*We define dependent variable again to easily change outcome of interest

local dep informal


/*
ib2011 differs from i.year as it sets 2011 to be the base year

We interact year dummies (using 2011 as base) with the ratio

We include region fixed effects and year fixed effects

We include ratio15 seperately to see if is related to dep independent of time 
interactions

We then interact the larger region effects with year
*/

reg $dep ib2011.year#c.ratio15 i.nuts2 ib2011.year ratio15 i.nuts1#c.year if male==0, robust

// ssc install coefplot

/*
We then plot each coefficent interacting the pre-trend years with the ratios 
along with their confidence intervals. Looking at the plot we can see the 
the "trend" stays the same throughout each year and that all of the cofficents 
are not satistically significant. 
*/

coefplot, vertical yline(0) nolabel keep(2006.year#c.ratio15 2007.year#c.ratio15 2008.year#c.ratio15 2009.year#c.ratio15 2010.year#c.ratio15 2013.year#c.ratio15 2014.year#c.ratio15 2015.year#c.ratio15) coeflabels(2006.year#c.ratio15=2006 2007.year#c.ratio15=2007 2008.year#c.ratio15=2008 2009.year#c.ratio15=2009 2010.year#c.ratio15=2010 2013.year#c.ratio15=2013 2014.year#c.ratio15=2014 2015.year#c.ratio15=2015)

// coefplot, vertical yline(0) nolabel keep(*.year#c.ratio15) coeflabels(2006.year#c.ratio15=2006 2007.year#c.ratio15=2007 2008.year#c.ratio15=2008 2009.year#c.ratio15=2009 2010.year#c.ratio15=2010 2013.year#c.ratio15=2013 2014.year#c.ratio15=2014 2015.year#c.ratio15=2015)

*******************************************************************************

/*
Not all datasets have a control group. Synthetic controls can be used to 
mitigate this problem. We can take a convex combination of other controls. 
In this example, lets assume that nuts2 5 does not have a control group. 
By running the below code, we can find the weights of other regions that
would provide a synthetic control. 
*/


*Define the treated region that needs a synthetic control

gen treated = (nuts2 == 5)

*Define the post treatment years

gen post_treatment = (year >= 2011)

*Keep only the non-missing values of the variables that would be used for weighting matrix

keep nuts2 year treated informal age male educ_attain urban ratio15 sector literate

drop if missing(informal, age, male, educ_attain, urban, ratio15, sector, literate)

*Calculate the mean of selected variables and collapse the data

collapse (mean) informal age male educ_attain urban ratio15 sector literate, by(nuts2 year)

*Specify the data to be panel data by tsset

sort nuts2 year

tsset nuts2 year

*Run synth command on variables and by setting trunit() and trperiod() to the values above

synth informal age male educ_attain urban ratio15 sector literate, trunit(5) trperiod(2011) fig

/*Another example of synthetic control using California to test 
whether or not tobacco control leads to reduce smoking. 
1989 California implemented a cigarette tax. As California is a unique 
state, using a synthetic control is useful to see the impact.
*/ 


/*
Running the below code compares California's averages for all observation variable 
and how it compares to average of all other control states
*/

use mus225smoking.dta, clear

drop if year > 1989

collapse (mean) cigsale lnincome beer retprice, by(state)

save california.dta, replace

drop if state == 3

collapse (mean) cigsale lnincome beer retprice

gen state = 0

save control.dta, replace

use california.dta, clear

keep if state == 3

append using control.dta

list group cigsale lnincome beer retprice

use mus225smoking.dta, clear


tsset state year

*specificy treated unit and treatperiod to find the weights

synth cigsale beer lnincome retprice age15to24 cigsale, trunit(3) trperiod(1989) fig







