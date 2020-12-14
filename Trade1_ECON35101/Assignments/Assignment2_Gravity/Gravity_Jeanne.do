** Assignment 2 - Gravity - Jeanne

* Set pwd() to be the folder with the code & the data

* Install packages of interest (to do only once)
/*
ssc install reghdfe
ssc install ppml
ssc install poi2hdfe
ssc install ppml_panel_sg
ssc install ftools
ssc install ppmlhdfe
ssc install hdfe
*/

** Import data
use "col_regfile09.dta", clear

** Keep variables of interest
keep flow distw iso_o year iso_d contig comlang_off
gen lflow = log(flow)
gen ldistw = log(distw)

** Prepare the variables
* Importer year & exporter year FE
egen year_isoo = group(year iso_o)
egen year_isod = group(year iso_d)
egen iso_d_factor = group(iso_d)
egen iso_o_factor = group(iso_o)
egen iso = group(iso_d iso_o)

** Table 1: log-linear estimation
eststo clear
timer clear

timer on 1
eststo: qui reg lflow contig comlang_off ldistw i.year_isoo i.year_isod if year >= 2000
timer off 1
timer list 1
estadd local time = r(t1), replace

* Note that in the regression below, as xtreg only accepts one id for identifier
* and one for time (definition of a panel), we need to include the other factor
* in the regression as a factor
timer on 2
preserve
keep if year >= 2000
xtset iso_o_factor
eststo: xtreg lflow contig comlang_off ldistw i.iso_d_factor if year >= 2000, fe
restore
timer off 2
timer list 2
estadd local time = r(t2), replace


* Note that in the regression below, as areg only accepts 1 FE, need to include the rest in the core reg
timer on 3
eststo: areg lflow contig comlang_off ldistw i.iso_d_factor i.iso_o_factor if year >= 2000, a(year) 
timer off 3
timer list 3
estadd local time = r(t3), replace


timer on 4
eststo: reghdfe lflow contig comlang_off ldistw if year >= 2000, a(year_isoo year_isod) 
timer off 4
timer list 4
estadd local time = r(t4), replace


label var contig "Contiguous countries (dummy)"
label var comlang_off "Common language (dummy)"
label var ldistw "Log(distance)"
label var lflow "log(flow)"

esttab using "Table1.tex", tex label se star(* 0.10 ** 0.05 *** 0.01) ///
	title(Table 1) ///
	mgroups("reg" "xtreg" "areg" "reghdfe", pattern(1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(})) ///
	keep(contig comlang_off ldistw) ///
	s(N r2 time, label("N" "R2" "Time (sec)"))  replace

	
	
	
	

** Table 2: zeros
gen lflow1 = log(flow+1)
gen lflow02 = log(flow+0.2)
label var lflow1 "log(flow+1)"

eststo clear
timer clear
* Col 1: log linear reg omitting observations in which flow equals 0
timer on 1
eststo: reghdfe lflow contig comlang_off ldistw if (year >= 2004 & flow > 0), a(year_isod year_isoo)
timer off 1
timer list 1
estadd local time = r(t1), replace
estadd local spec1 = "reg", replace
estadd local greater0 = "Yes", replace
estadd local FE = "Yes", replace


* Col 2: log linear reg omitting observations in which flow equals 0 ; dep = log(flow+1)
timer on 2
eststo: reghdfe lflow1 contig comlang_off ldistw if (year >= 2004 & flow > 0), a(year_isod year_isoo)
timer off 2
timer list 2
estadd local time = r(t2), replace
estadd local spec1 = "reg", replace
estadd local greater0 = "Yes", replace
estadd local FE = "Yes", replace

* Col 3: log linear reg dep = log(flow+1)
timer on 3
eststo: reghdfe lflow1 contig comlang_off ldistw if (year >= 2004), a(year_isod year_isoo)
*eststo: reghdfe lflow02 contig comlang_off ldistw if (year >= 2004), a(year_isod year_isoo)
timer off 3
timer list 3
estadd local time = r(t3), replace
estadd local spec1 = "reg", replace
estadd local greater0 = "No", replace
estadd local FE = "Yes", replace


* Col 4: ppml
preserve
qui tab year_isoo, gen(year_o_)
qui tab year_isod, gen(year_d_)

timer clear 4
timer on 4
eststo: ppml flow ldistw contig comlang_off year_o_* year_d_* if (year >= 2004)
timer off 4
timer list 4
estadd local time = r(t4), replace
estadd local spec1 = "ppml", replace
estadd local greater0 = "No", replace
estadd local FE = "Yes", replace
restore


* Col 5: poi2hdfe
timer on 5
eststo: poi2hdfe flow contig comlang_off ldistw if (year >= 2004), id1(year_isod) id2(year_isoo)
timer off 5
timer list 5
estadd local time = r(t5), replace
estadd local spec1 = "reg", replace
estadd local greater0 = "Yes", replace
estadd local FE = "No", replace



* Col 6: 
timer on 6
eststo: ppml_panel_sg flow contig comlang_off ldistw if (year >= 2004), exp(iso_o) imp(iso_d) y(year) nopair
timer off 6
timer list 6
estadd local time = r(t6), replace
estadd local spec1 = "ppml\_panel_\sg", replace
estadd local greater0 = "No", replace
estadd local FE = "Yes", replace


* Col 7: ppmlhdfe
timer on 7
eststo: ppmlhdfe flow contig comlang_off ldistw if year >= 2004, a(year_isoo year_isod) 
timer off 7
timer list 7
estadd local time = r(t7), replace
estadd local spec1 = "ppmlhdfe", replace
estadd local greater0 = "No", replace
estadd local FE = "Yes", replace


* Col 8: ppmlhdfe, no observation in which flow=0
timer on 8
eststo: ppmlhdfe flow contig comlang_off ldistw if (year >= 2004 & flow > 0), a(year_isoo year_isod) 
timer off 8
timer list 8
estadd local time = r(t8), replace
estadd local spec1 = "ppmlhdfe", replace
estadd local greater0 = "Yes", replace
estadd local FE = "Yes", replace

esttab using "Table2.tex", tex label se star(* 0.10 ** 0.05 *** 0.01) ///
	title(Table 2) ///
	keep(contig comlang_off ldistw) ///
	s(N r2 spec1 greater0 FE time, label("N" "R2" "Command" "Flow greater than 0 only" "Fixed Effect" "Time (sec)"))  replace
	
	
	
	
	
** Examine residuals from log linear regression : heteroskedastic? (assuming we're looking at reg1)
reg lflow contig comlang_off ldistw if (year >= 2004 & flow > 0)
hettest
predict residuals if (year >= 2004 & flow > 0), residuals 
predict fitted if (year >= 2004 & flow > 0)
scatter residuals fitted if (year >= 2004 & flow > 0)
graph export stata_table2_scatter.png, replace


label var contig "Contiguous countries (dummy)"
label var comlang_off "Common language (dummy)"
label var ldistw "Log(distance)"
label var lflow "log(flow)"

*** Table 3
eststo clear
timer clear
timer on 1
eststo: reghdfe lflow contig comlang_off ldistw if year >= 1948, a(year_isoo year_isod) vce(robust)
timer off 1
timer list 1
estadd local time = r(t1), replace
esttab using "Table3_stata.tex", tex label se star(* 0.10 ** 0.05 *** 0.01) ///
	title(Table 3 (Stata)) ///
	keep(contig comlang_off ldistw) ///
	s(N r2 time, label("N" "R2" "Time (sec)"))  replace
	
	

