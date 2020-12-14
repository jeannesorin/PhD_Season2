
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
*NOTE: Load dataset and generate variables by running the beginning of the do-file "Regs_QJE_Replicate_Paper"
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

tab  provinz, gen(d_province)
tab regierungsbezirk, gen(d_RGBZ)


*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Figures  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

*Distribution of vote shares -- full sample
twoway (kdensity pcDVFP245, bwidth(0.05) lcolor(black) lpattern(shortdash)) (kdensity pcNSDAP285, bwidth(0.05) lcolor(black) lpattern(solid)) ///
	(kdensity pcNSDAP309, bwidth(0.05) lcolor(black) lpattern(longdash)) (kdensity pcNSDAP333, bwidth(0.05) lcolor(black) lpattern(dash_dot)), ///
	ytitle(Kernel density) xtitle(Vote share) legend(order(1 "DVFP in 1924" 2 "NSDAP in 1928" 3 "NSDAP in 1930" 4 "NSDAP in 1933") position(1) ring(0)) scheme(s1color)

	*Distribution of vote shares -- restricted sample
	twoway (kdensity pcDVFP245 if exist1349==1, bwidth(0.05) lcolor(black) lpattern(shortdash)) (kdensity pcNSDAP285 if exist1349==1, bwidth(0.05) lcolor(black) lpattern(solid)) ///
		(kdensity pcNSDAP309 if exist1349==1, bwidth(0.05) lcolor(black) lpattern(longdash)) (kdensity pcNSDAP333 if exist1349==1, bwidth(0.05) lcolor(black) lpattern(dash_dot)), ///
		ytitle(Kernel density) xtitle(Vote share) legend(order(1 "DVFP in 1924" 2 "NSDAP in 1928" 3 "NSDAP in 1930" 4 "NSDAP in 1933") position(1) ring(0)) scheme(s1color)

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Analysis  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

//////////////////////////////////////////////// Descriptive Stats /////////////////////////////////////////////
	
*Table A.1 -- Descriptive statistics -- Full sample
sum pop33 perc_JEW33 perc_PROT25 syn33 pog20 pcNSDAP285 pcDVFP245 deptotal stuermersum exist1349 pog1349 if perc_JEW33>0  //conditioning on perc_JEW33>0 only affects one obs.
	sum syndamordest if syn33==1 //for existing synagogue

	
*Table A.2 -- Pairwise Correlation:

pwcorr pog1349 pog20s pcDVFP245 pcNSDAP285 prop_deport stuermerper10K syndamordest [weight=pop33], sig 
	pwcorr pog1349 pog20s pcDVFP245 pcNSDAP285 prop_deport stuermerper10K syndamordest if syn33==1 [weight=pop33], sig  //for existing synagogue


///////////////////////////////////////////////////////////////////////////////////////////
*******	Unobservables: Do pogroms 1349 predict city characteristics in 20C?  **************	

*Table A.3
*Panel A:
bysort exist1349: sum popgrowth1300 if pog1349~=. 
bysort exist1349: sum popgrowth1750 if pog1349~=.
bysort exist1349: sum perc_PROT25 if pog1349~=.
bysort exist1349: sum perc_JEW33 if pog1349~=.
bysort exist1349: sum perc_Blue33 if pog1349~=.
bysort exist1349: sum perc_Unemp33 if pog1349~=.
bysort exist1349: sum perc_Ind33 if pog1349~=.
bysort exist1349: sum perc_RT33 if pog1349~=.

*Panel B:
eststo: reg  popgrowth1300 pog1349 exist1349 logpop1300 perc_JEW33 perc_PROT25, cluster(kreis_nr)
eststo: reg  popgrowth1750 pog1349 exist1349 logpop1750 perc_JEW33 perc_PROT25, cluster(kreis_nr)
eststo: reg  perc_PROT25 pog1349 exist1349 logpop25c perc_JEW33, cluster(kreis_nr)
eststo: reg  perc_JEW33 pog1349 exist1349 logpop33_dep perc_PROT25, cluster(kreis_nr)
eststo: reg  perc_Blue33 pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25, cluster(kreis_nr)
eststo: reg  perc_Unemp33 pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25, cluster(kreis_nr)
eststo: reg  perc_Ind33 pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25, cluster(kreis_nr)
eststo: reg  perc_RT33 pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25, cluster(kreis_nr)
esttab using table_Appendix_Unobs.rtf, se ar2 label nocons title (Main Results at the kreis_nr level) mtitles(OLS OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*****************    Election turnout as a proxy for social capital   *********************************
*Table A.4
*Panel A:
bysort pog1349: sum turnout245 if exist1349==1
bysort pog1349: sum turnout285 if exist1349==1
bysort pog1349: sum turnout309 if exist1349==1
bysort pog1349: sum turnout333 if exist1349==1
bysort exist1349: sum turnout245 if pog1349~=.
bysort exist1349: sum turnout285 if pog1349~=.
bysort exist1349: sum turnout309 if pog1349~=.
bysort exist1349: sum turnout333 if pog1349~=.

*Panel B:
eststo: reg turnout245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg turnout285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg turnout309 pog1349 logpop309 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg turnout333 pog1349 logpop333 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg turnout245 pog1349 exist1349 logpop245 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg turnout285 pog1349 exist1349 logpop285 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg turnout309 pog1349 exist1349 logpop309 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg turnout333 pog1349 exist1349 logpop333 perc_JEW25 perc_PROT25, cluster(kreis_nr)
esttab using table_Appendix_Elect_Turnout.rtf, se ar2 label nocons title (Main Results at the kreis_nr level) mtitles(OLS OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


//////////////////////////////////////////////// Robustness of Main Results /////////////////////////////////////////////	
*Table A.5

*Baseline regs with further controls
eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 perc_Blue25 perc_Ind25 perc_selfRT25 turnout245 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 perc_Blue25 perc_Ind25 perc_selfRT25 turnout285 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 perc_Blue25 perc_Ind25 perc_selfRT25 turnout245 if exist1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 logjews33 logpop33_dep perc_JEW33 perc_PROT25 perc_Blue33 perc_Ind33 perc_selfRT25 turnout245 perc_Unemp33 if exist1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 perc_Blue33 perc_Ind33 perc_selfRT25 turnout245 perc_Unemp33 if exist1349==1, robust family(poisson)
eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 perc_Blue33 perc_Ind33 perc_selfRT25 turnout245 perc_Unemp33 if exist1349==1 & syn33==1, robust cluster(kreis_nr)
esttab using table_Appendix_Robustness1.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Matching
eststo: nnmatch pog20 pog1349 Longitude Latitude logpop25c perc_JEW25 perc_PROT25 perc_Blue25 perc_Ind25 perc_selfRT25 turnout245 if exist1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch pcNSDAP285 pog1349 Longitude Latitude logpop285 perc_JEW25 perc_PROT25 perc_Blue25 perc_Ind25 perc_selfRT25 turnout285 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcDVFP245 pog1349 Longitude Latitude logpop245 perc_JEW25 perc_PROT25 perc_Blue25 perc_Ind25 perc_selfRT25 turnout245 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch deptotal pog1349  logjews33  Longitude Latitude logpop33_dep perc_JEW33 perc_PROT25 perc_Blue33 perc_Ind33 perc_selfRT25 turnout245 perc_Unemp33 if exist1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch stuermersum pog1349 Longitude Latitude logpop33_dep perc_JEW33 perc_PROT25 perc_Blue33 perc_Ind33 perc_selfRT25 turnout245 perc_Unemp33 if exist1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch syndamordest pog1349 Longitude Latitude logpop33_dep perc_JEW33 perc_PROT25 perc_Blue33 perc_Ind33 perc_selfRT25 turnout245 perc_Unemp33 if exist1349==1 & syn33==1, m(4) robust(4) tc(att) 
esttab using table_Appendix_Robustness2.rtf, se ar2 label nocons title (Main Results mtitles(ME ME ME ME ME ME))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


////////////////// Province and Prefecture FE  /////////////////////////////////////////////////
*Table A.6, Panel A: Province FE
eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 d_province1-d_province23 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25  d_province1-d_province23 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 d_province1-d_province23 if exist1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 d_province1-d_province23 if exist1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25  d_province1-d_province23 if exist1349==1, robust family(poisson)
eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 d_province1-d_province23 if exist1349==1 & syn33==1, robust cluster(kreis_nr)
esttab using table_Appendix_FE1.rtf, drop(d_province*) se ar2 label nocons title (Province Fixed Effects) mtitles(OLS OLS OLS OLS Probit ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Table A.6, Panel B: Regierungbezirk (Prefecture) FE
eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25  d_RGBZ1-d_RGBZ54 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25  d_RGBZ1-d_RGBZ54 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25  d_RGBZ1-d_RGBZ54 if exist1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 d_RGBZ1-d_RGBZ54 if exist1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25  d_RGBZ1-d_RGBZ54 if exist1349==1, robust family(poisson)
eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25  d_RGBZ1-d_RGBZ54 if exist1349==1 & syn33==1, robust cluster(kreis_nr)
esttab using table_Appendix_FE2.rtf, drop(d_RGBZ*) se ar2 label nocons title (Regierungsbezirk Fixed Effects) mtitles(OLS OLS OLS OLS Probit ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	


//////////////////////////////////////////////// Regressions for individual variables /////////////////////////////////////////////	
	
/*Table A7: Pogroms in the 1920s*/
eststo: reg pog20 pog1349 exist1349, robust cluster(kreis_nr)
eststo: reg pog20 pog1349 exist1349 logpop25c perc_JEW25 perc_PROT25, robust cluster(kreis_nr)
eststo: reg pog20 pog1349 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr) 
eststo: probit pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr) 
eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att)   
eststo: nnmatch pog20 pog1349 exist1349 Latitude Longitude, m(2) robust(2) tc(att) keep(geomatch1A, replace) 
eststo: nnmatch pog20 pog1349 Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch2A, replace) 
esttab using table_Appendix_pog20.rtf, se ar2 label nocons title (Dependent variable: Pogrom 1920s) mtitles(OLS OLS OLS OLS Probit ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	
	
	
/*Table A9: Elections: NSDAP May 1928*/
eststo: reg  pcNSDAP285 pog1349 exist1349, cluster(kreis_nr)
eststo: reg  pcNSDAP285 pog1349 exist1349 logpop285 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg  pcNSDAP285 pog1349 if exist1349==1, cluster(kreis_nr)
eststo: reg  pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: glm  pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr) family(poisson)
eststo: nnmatch  pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcNSDAP285 pog1349 exist1349 Latitude Longitude , m(2) robust(2) tc(att) keep(geomatch3A, replace) 
eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch4A, replace) 
esttab using table_Appendix_NSDAP1928.rtf, se ar2 label nocons title (Dependent variable: % vote for NSDAP in May 1928 election) mtitles(OLS OLS OLS OLS ML ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	


/*Table A9 -- Elections: DVFP May 1924: Deutsche Völkische Freiheits-Partei (German Racial Freedom Party)*/
eststo: reg  pcDVFP245 pog1349 exist1349, cluster(kreis_nr)
eststo: reg  pcDVFP245 pog1349 exist1349 logpop245 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg  pcDVFP245 pog1349 if exist1349==1, cluster(kreis_nr)
eststo: reg  pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 , cluster(kreis_nr)
eststo: glm  pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr) family(poisson)
eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  exist1349, m(2) robust(2) tc(att) keep(geomatch5A, replace) 
eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch6A, replace) 
esttab using table_Appendix_DVFP1924.rtf, se ar2 label nocons title (Dependent variable: % vote for DVFP/NSFP in May 1924 election) mtitles(OLS OLS OLS OLS ML ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


*Table A10a -- Deportations, Controlling for Jewish pop in 1933
eststo: reg  logdeport pog1349 exist1349 logjews33, robust cluster(kreis_nr)
eststo: reg  logdeport pog1349 logjews33 if exist1349==1, robust cluster(kreis_nr)
eststo: nnmatch  logdeport pog1349 logjews33 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch  logdeport pog1349 logjews33 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1, m(4) robust(4) tc(att) 
eststo: glm  deptotal pog1349 logjews33 if exist1349==1, robust family(poisson)
eststo: glm  deptotal pog1349 logjews33 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1, robust family(poisson)
eststo: nnmatch  logdeport pog1349 exist1349 logjews33 Latitude Longitude , m(2) robust(2) tc(att) keep(geomatch7A, replace) 
eststo: nnmatch  logdeport pog1349 logjews33 Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch8A, replace) 
esttab using table_Appendix_Deport33.rtf, se ar2 label nocons title (Dependent variable: ln(deported Jews)) mtitles(OLS OLS ME ME ML ML GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Table A10b -- Deportations, Controlling for Jewish pop in 1939
eststo: reg  logdeport pog1349 exist1349 logjews39, robust cluster(kreis_nr)
eststo: reg  logdeport pog1349 logjews39 if exist1349==1, robust cluster(kreis_nr)
eststo: nnmatch  logdeport pog1349 logjews39 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch  logdeport pog1349 logjews39 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1, m(4) robust(4) tc(att) 
eststo: glm  deptotal pog1349 logjews39 if exist1349==1, robust family(poisson)
eststo: glm  deptotal pog1349 logjews39 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1, robust family(poisson)
eststo: nnmatch  logdeport pog1349 exist1349 logjews39 Latitude Longitude , m(2) robust(2) tc(att) keep(geomatch9A, replace) 
eststo: nnmatch  logdeport pog1349 logjews39 Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch10A, replace) 
esttab using table_Appendix_Deport39.rtf, se ar2 label nocons title (Dependent variable: ln(deported Jews)) mtitles(OLS OLS ME ME ML ML GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Table A11 -- Stuermer letters
eststo: reg log_stuermersum pog1349 exist1349 logpop33_dep, robust cluster(kreis_nr)
eststo: reg log_stuermersum pog1349 logpop33_dep if exist1349==1, robust cluster(kreis_nr)
eststo: nnmatch log_stuermersum pog1349 logpop33_dep if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch log_stuermersum pog1349 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1, m(4) robust(4) tc(att) 
eststo: glm stuermersum pog1349 logpop33_dep if exist1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1, robust family(poisson)
eststo: nnmatch  log_stuermersum pog1349 exist1349 logpop33_dep Latitude Longitude , m(2) robust(2) tc(att) keep(geomatch11A, replace) /*Note: Number of obs larger than in reg (1). To get same number of obs: "if logjews33~=." */
eststo: nnmatch  log_stuermersum pog1349 logpop33_dep Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch12A, replace) 
esttab using table_Appendix_Stuermer.rtf, se ar2 label nocons title (Dependent variable: ln(Reader letters to Stürmer)) mtitles(OLS OLS ME ME ML ML GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Table A12 -- Deportations per 100 Jews and Stuermer letters per 10000 inhabitants
eststo: reg prop_deport pog1349 logpop33_dep perc_PROT25 perc_JEW33   if exist1349==1 [weight=pop33], robust cluster(kreis_nr)
eststo: nnmatch prop_deport pog1349 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1 [weight=pop33], m(4) robust(4) tc(att)  
eststo: nnmatch prop_deport pog1349 Latitude Longitude logpop33_dep if exist1349==1 [weight=pop33], m(2) robust(2) tc(att) 
eststo: reg stuermerper10K pog1349 logpop33_dep perc_PROT25 perc_JEW33   if exist1349==1 [weight=pop33], robust cluster(kreis_nr)
eststo: nnmatch stuermerper10K pog1349 logpop33_dep perc_PROT25 perc_JEW33 if exist1349==1 [weight=pop33], m(4) robust(4) tc(att)  
eststo: nnmatch stuermerper10K pog1349 Latitude Longitude logpop33_dep if exist1349==1 [weight=pop33], m(2) robust(2) tc(att) 
esttab using table_Appendix_Deport_Stuermer_prop.rtf, se ar2 label nocons title (Dependent variable: Deportations per 100 Jews and Stuermer letters per 10000 inhabitants) mtitles(OLS ME GeoMatch OLS ME GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear  

*Table A13: Synagogue attacks
eststo: reg syndamordest pog1349 exist1349 if syn33==1, robust
eststo: reg syndamordest pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25 if syn33==1, robust
eststo: reg syndamordest pog1349 if exist1349==1 & syn33==1, robust
eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, robust
eststo: probit syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, robust 
eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, robust(4) tc(att)  m(4)
eststo: nnmatch syndamordest pog1349 exist1349 Latitude Longitude  if syn33==1, m(2) robust(2) tc(att) keep(geomatch13A, replace) 
eststo: nnmatch syndamordest pog1349 Latitude Longitude   if syn33==1 & exist1349==1, m(2) robust(2) tc(att) keep(geomatch14A, replace) 
esttab using table_Appendix_Synagoge.rtf, se ar2 label nocons title (Dependent variable: Synagogue damaged or destroyed in 1938) mtitles(OLS OLS OLS OLS Probit ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear



////////////////// Correcting for spatial correlation /////////////////////////////////////////////////

*Table A.14

drop if  Latitude==. |  Longitude == .
keep if exist1349==1
drop if pog20==.
drop if pog1349==.


*Pogroms 1920
	spatwmat, name(W) xcoord(Longitude) ycoord(Latitude) band(0 4) eigenval(E)
	eststo: spatreg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25, weights(W) eigenval(E) model(error)

*Elections: NSDAP May 1928
preserve
	drop if pcNSDAP285==. 
	spatwmat, name(W) xcoord(Longitude) ycoord(Latitude) band(0 4) eigenval(E)
	eststo: spatreg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25, weights(W) eigenval(E) model(error)
restore

/*Elections: DVFP May 1924: Deutsche Völkische Freiheits-Partei (German Racial Freedom Party)*/
preserve
	drop if pcDVFP245==. 
	spatwmat, name(W) xcoord(Longitude) ycoord(Latitude) band(0 4) eigenval(E)
	eststo: spatreg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25, weights(W) eigenval(E) model(error)
restore


/*Deportation*/
preserve
	drop if logdeport==. | logjews33==. 
	drop if logpop33_dep==. | logjews33 ==. |  perc_JEW33 ==. | perc_PROT25 ==. 
	spatwmat, name(W) xcoord(Longitude) ycoord(Latitude) band(0 4) eigenval(E)
	eststo: spatreg logdeport pog1349 logjews33 logpop33_dep perc_PROT25 perc_JEW33, weights(W) eigenval(E) model(error)
restore		
	
*Stuermer
preserve
	*Basic reg
	drop if log_stuermersum==. | pog1349==. | logpop33_dep==.  | perc_JEW33 ==. | perc_PROT25 ==.  	
	spatwmat, name(W) xcoord(Longitude) ycoord(Latitude) band(0 4) eigenval(E)
	eststo: spatreg log_stuermersum pog1349 logpop33_dep perc_PROT25 perc_JEW33, weights(W) eigenval(E) model(error)
restore

	
*Synagogue -- need to additionally drop all cities that don't have one
preserve
	drop if syn33~=1
	drop if logpop33_dep==. | logjews33 ==. |  perc_JEW33 ==. | perc_PROT25 ==. 
	spatwmat, name(W) xcoord(Longitude) ycoord(Latitude) band(0 4) eigenval(E)
	eststo: spatreg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25, weights(W) eigenval(E) model(error)
restore

esttab using table_Spatial_Corr.rtf, se ar2 label nocons title (Spatial Error Regression Model) mtitles(OLS OLS OLS OLS ML ME)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	
	
	
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////	
//////////////////////////////////////// Re-load previously dropped results /////////////////////////////////////////////////
*NOTE: Load dataset and generate variables by running the beginning of the do-file "Regs_QJE_Replicate_Paper"
tab  provinz, gen(d_province)
tab regierungsbezirk, gen(d_RGBZ)
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	
//////////////////////////////////////////////// Main Results -- Sample Splits /////////////////////////////////////////////	

*Tables A.15a-c: 


////// Catholic vs Protestant //////

	*Mostly Protestant -- GeoMatch
	eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist1349==1 & perc_PROT25>50, m(2) robust(2) tc(att)
	eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist1349==1 & perc_PROT25>50, m(2) robust(2) tc(att)  
	eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist1349==1 & perc_PROT25>50, m(2) robust(2) tc(att) 
	eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist1349==1 & perc_PROT25>50, m(2) robust(2) tc(att) 
	eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist1349==1 & perc_PROT25>50, m(2) robust(2) tc(att) 
	eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist1349==1 & syn33==1 & perc_PROT25>50, m(2) robust(2) tc(att)
	esttab using table_Robustness_Main_Protestant_GeoMatch.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
	
	*Mostly Protestant -- OLS
	eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & perc_PROT25>50, robust cluster(kreis_nr)
	eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25  if exist1349==1 & perc_PROT25>50, robust cluster(kreis_nr)
	eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & perc_PROT25>50, robust cluster(kreis_nr)
	eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & perc_PROT25>50, robust family(poisson)
	eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_PROT25>50, robust family(poisson)
	eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_PROT25>50 & syn33==1, robust cluster(kreis_nr)
	esttab using table_Robustness_Main_Protestant_OLS.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
		
	*Mostly Protestant -- Matching
	eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & perc_PROT25>50, m(4) robust(4) tc(att)  
	eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & perc_PROT25>50, m(4) robust(4) tc(att) 
	eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & perc_PROT25>50, m(4) robust(4) tc(att)   
	eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & perc_PROT25>50, m(4) robust(4) tc(att)  
	eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_PROT25>50, m(4) robust(4) tc(att)  
	eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_PROT25>50 & syn33==1, m(4) robust(4) tc(att) 
	esttab using table_Robustness_Main_Protestant_ME.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
		
	*Mostly Catholic -- GeoMatch
	eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist1349==1 & perc_CAT25>50, m(2) robust(2) tc(att)
	eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist1349==1 & perc_CAT25>50, m(2) robust(2) tc(att)  
	eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist1349==1 & perc_CAT25>50, m(2) robust(2) tc(att) 
	eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist1349==1 & perc_CAT25>50, m(2) robust(2) tc(att) 
	eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist1349==1 & perc_CAT25>50, m(2) robust(2) tc(att) 
	eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist1349==1 & syn33==1 & perc_CAT25>50, m(2) robust(2) tc(att)
	esttab using table_Robustness_Main_Catholic_GeoMatch.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
	
	*Mostly Catholic -- OLS
	eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & perc_CAT25>50, robust cluster(kreis_nr)
	eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & perc_CAT25>50, robust cluster(kreis_nr)
	eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & perc_CAT25>50, robust cluster(kreis_nr)
	eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & perc_CAT25>50, robust family(poisson)
	eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_CAT25>50, robust family(poisson)
	eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_CAT25>50 & syn33==1, robust cluster(kreis_nr)	
	esttab using table_Robustness_Main_Catholic_OLS.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
		
	*Mostly Catholic -- Matching
	eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & perc_CAT25>50, m(4) robust(4) tc(att)  
	eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & perc_CAT25>50, m(4) robust(4) tc(att) 
	eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & perc_CAT25>50, m(4) robust(4) tc(att)   
	eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & perc_CAT25>50, m(4) robust(4) tc(att)  
	eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_CAT25>50, m(4) robust(4) tc(att)  
	eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & perc_CAT25>50 & syn33==1, m(4) robust(4) tc(att) 
	esttab using table_Robustness_Main_Catholic_ME.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear

	
///////// West vs East /////////////	
	
sum Longitude if exist1349==1 & pog1349~=., d 

	*West Only -- GeoMatch
	eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist1349==1 & Longitude<9.18, m(2) robust(2) tc(att)
	eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist1349==1 & Longitude<9.18, m(2) robust(2) tc(att)  
	eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist1349==1 & Longitude<9.18, m(2) robust(2) tc(att) 
	eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist1349==1 & Longitude<9.18, m(2) robust(2) tc(att) 
	eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist1349==1 & Longitude<9.18, m(2) robust(2) tc(att) 
	eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist1349==1 & syn33==1 & Longitude<9.18, m(2) robust(2) tc(att)
	esttab using table_Robustness_Main_West_GeoMatch.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 
	
	*West Only -- OLS
	eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & Longitude<9.18, robust cluster(kreis_nr)
	eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude<9.18, robust cluster(kreis_nr)
	eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude<9.18, robust cluster(kreis_nr)
	eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & Longitude<9.18, robust family(poisson)
	eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude<9.18, robust family(poisson)
	eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude<9.18 & syn33==1, robust cluster(kreis_nr)
	esttab using table_Robustness_Main_West_OLS.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
	
	*West Only -- Matching
	eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & Longitude<9.18, m(4) robust(4) tc(att)  
	eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude<9.18, m(4) robust(4) tc(att) 
	eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude<9.18, m(4) robust(4) tc(att)   
	eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & Longitude<9.18, m(4) robust(4) tc(att)  
	eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude<9.18, m(4) robust(4) tc(att)  
	eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude<9.18 & syn33==1, m(4) robust(4) tc(att) 
	esttab using table_Robustness_Main_West_ME.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 
	
		
	*East Only -- GeoMatch
	eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist1349==1 & Longitude>9.18, m(2) robust(2) tc(att)
	eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist1349==1 & Longitude>9.18, m(2) robust(2) tc(att)  
	eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist1349==1 & Longitude>9.18, m(2) robust(2) tc(att) 
	eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist1349==1 & Longitude>9.18, m(2) robust(2) tc(att) 
	eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist1349==1 & Longitude>9.18, m(2) robust(2) tc(att) 
	eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist1349==1 & syn33==1 & Longitude>9.18, m(2) robust(2) tc(att)
	esttab using table_Robustness_Main_East_GeoMatch.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 
		
	*East Only -- OLS
	eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & Longitude>9.18, robust cluster(kreis_nr)
	eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude>9.18, robust cluster(kreis_nr)
	eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude>9.18, robust cluster(kreis_nr)
	eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & Longitude>9.18, robust family(poisson)
	eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude>9.18, robust family(poisson)
	eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude>9.18 & syn33==1, robust cluster(kreis_nr)	
	esttab using table_Robustness_Main_East_OLS.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 
	
	*East Only -- Matching
	eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & Longitude>9.18, m(4) robust(4) tc(att)  
	eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude>9.18, m(4) robust(4) tc(att) 
	eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & Longitude>9.18, m(4) robust(4) tc(att)   
	eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & Longitude>9.18, m(4) robust(4) tc(att)  
	eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude>9.18, m(4) robust(4) tc(att)  
	eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & Longitude>9.18 & syn33==1, m(4) robust(4) tc(att) 
	esttab using table_Robustness_Main_East_ME.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 	
	
	
///////// Large vs Small /////////////
	
sum pop33 if exist1349==1 & pog1349~=., d
	 
	*Larger cities -- GeoMatch
	tab pog1349 if exist1349==1 & pog1349~=. & pop33>9022
	eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist1349==1 & pop33>9022, m(2) robust(2) tc(att)
	eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist1349==1 & pop33>9022, m(2) robust(2) tc(att)  
	eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist1349==1 & pop33>9022, m(2) robust(2) tc(att) 
	eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist1349==1 & pop33>9022, m(2) robust(2) tc(att) 
	eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist1349==1 & pop33>9022, m(2) robust(2) tc(att) 
	eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist1349==1 & syn33==1 & pop33>9022, m(2) robust(2) tc(att)
	esttab using table_Robustness_Main_BigCities_GeoMatch.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 
	
	*Larger cities -- OLS
	eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & pop33>9022, robust cluster(kreis_nr)
	eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & pop33>9022, robust cluster(kreis_nr)
	eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & pop33>9022, robust cluster(kreis_nr)
	eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & pop33>9022, robust family(poisson)
	eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33>9022, robust family(poisson)
	eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33>9022 & syn33==1, robust cluster(kreis_nr)
	esttab using table_Robustness_Main_BigCities_OLS.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 
	
	*Larger cities -- Matching
	eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & pop33>9022, m(4) robust(4) tc(att)  
	eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & pop33>9022, m(4) robust(4) tc(att) 
	eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & pop33>9022, m(4) robust(4) tc(att)   
	eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & pop33>9022, m(4) robust(4) tc(att)  
	eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33>9022, m(4) robust(4) tc(att)  
	eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33>9022 & syn33==1, m(4) robust(4) tc(att) 
	esttab using table_Robustness_Main_BigCities_ME.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear
		
		
	*Smaller cities -- GeoMatch
	eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist1349==1 & pop33<=9022, m(2) robust(2) tc(att)
	eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist1349==1 & pop33<=9022, m(2) robust(2) tc(att)  
	eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist1349==1 & pop33<=9022, m(2) robust(2) tc(att) 
	eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist1349==1 & pop33<=9022, m(2) robust(2) tc(att) 
	eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist1349==1 & pop33<=9022, m(2) robust(2) tc(att) 
	eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist1349==1 & syn33==1 & pop33<=9022, m(2) robust(2) tc(att)
	esttab using table_Robustness_Main_SmallCities_GeoMatch.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 	
		
	*Smaller cities -- OLS
	tab pog1349 if exist1349==1 & pog1349~=. & pop33<=9022
	eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & pop33<=9022, robust cluster(kreis_nr)
	eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & pop33<=9022, robust cluster(kreis_nr)
	eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & pop33<=9022, robust cluster(kreis_nr)
	eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & pop33<=9022, robust family(poisson)
	eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33<=9022, robust family(poisson)
	eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33<=9022 & syn33==1, robust cluster(kreis_nr)
	esttab using table_Robustness_Main_SmallCities_OLS.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 

	*Smaller cities -- Matching
	eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1 & pop33<=9022, m(4) robust(4) tc(att)  
	eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1 & pop33<=9022, m(4) robust(4) tc(att) 
	eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1 & pop33<=9022, m(4) robust(4) tc(att)   
	eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1 & pop33<=9022, m(4) robust(4) tc(att)  
	eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33<=9022, m(4) robust(4) tc(att)  
	eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & pop33<=9022 & syn33==1, m(4) robust(4) tc(att) 
	esttab using table_Robustness_Main_SmallCities_ME.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
	eststo clear 


////////////////// NASDAP votes after 1928:  /////////////////////////////////////////////////

*Table A.16 -- Elections: NSDAP after 1928 (for years where data are available at the city level)*/
eststo: reg  pcNSDAP309 pog1349 exist1349 logpop309 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg  pcNSDAP309 pog1349 logpop309 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: nnmatch pcNSDAP309 pog1349 logpop309 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcNSDAP309 pog1349 Latitude Longitude  if exist1349==1, m(2) robust(2) tc(att) keep(geomatch15A, replace) /*Use geomatch4F to label this regression*/
eststo: reg  pcNSDAP333 pog1349 exist1349 logpop333 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg  pcNSDAP333 pog1349 logpop333 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: nnmatch  pcNSDAP333 pog1349 logpop333 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcNSDAP333 pog1349 Latitude Longitude   if exist1349==1, m(2) robust(2) tc(att) keep(geomatch16A, replace)  /*Use geomatch4R to label this regression*/
esttab using table_Appendix_NSDAPafter1928.rtf, se ar2 label nocons title (Dependent variable: % vote for NSDAP) mtitles(1930 1930 1930 1930 1933 1933 1933 1933)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	


////////////////// DNVP votes in 1924:  /////////////////////////////////////////////////	
/*Table A17: DNVP votes*/
eststo: reg  pcDNVP245 pog1349 exist1349, cluster(kreis_nr)
eststo: reg  pcDNVP245 pog1349 exist1349 logpop245 perc_JEW25 perc_PROT25, cluster(kreis_nr)
eststo: reg  pcDNVP245 pog1349 if exist1349==1, cluster(kreis_nr)
eststo: reg  pcDNVP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: glm  pcDNVP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, cluster(kreis_nr) family(poisson)
eststo: nnmatch pcDNVP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcDNVP245 pog1349 exist1349 Latitude Longitude , m(2) robust(2) tc(att) keep(geomatch17A, replace) 
eststo: nnmatch pcDNVP245 pog1349 Latitude Longitude  if exist1349==1, m(2) robust(2) tc(att) keep(geomatch18A, replace) 
esttab using table_Appendix_DNVP1924.rtf, se ar2 label nocons title (Dependent variable: % vote for DNVP in May 1924) mtitles(OLS OLS OLS OLS ML ME GeoMatch GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	
	
	
////////////////// Main Extended Sample Results for Cities with documented or likely Jewish settlement  /////////////////////////////////////////////////

*Table A.18
gen exist_city1349 = 1 if age1349~=. | exist1349==1
probit exist1349 Hanse incorp1349 freeimperial market1349 nav_river bishop staufer d_isolated1 ln_age1349 if exist_city1349==1, robust cluster(kreis_nr)
predict exist1349_hat if exist_city1349==1
recode exist1349_hat .=0
gen exist2 = 1 if exist1349==1 | exist1349_hat>0.5
recode exist2 .=0
replace exist2=. if pop33>1000000 //Exclude two large-pop outliers because they heavily affect GLM results -- these regressions control for logpop (using logs for consistency), while number of letters 
								  *and deportations is huge. Note that the same problem dooes not affect our baseline results, because these two cities did not have documented Jewish settlements in 1349

*Baseline regs with predicted Jewish Communities: 
eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist2==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist2==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist2==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist2==1, robust family(poisson)
eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist2==1, robust family(poisson)
eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist2==1 & syn33==1, robust cluster(kreis_nr)
esttab using table_Appendix_Predicted_Exist1.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Matching
eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist2==1, m(4) robust(4) tc(att)  
eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist2==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist2==1, m(4) robust(4) tc(att)   
eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist2==1, m(4) robust(4) tc(att)  
eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist2==1, m(4) robust(4) tc(att)  
eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist2==1 & syn33==1, m(4) robust(4) tc(att) 
esttab using table_Appendix_Predicted_Exist2.rtf, se ar2 label nocons title (Main Results mtitles(ME ME ME ME ME ME))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Geo-Matching
eststo: nnmatch pog20 pog1349 Latitude Longitude  if exist2==1, m(2) robust(2) tc(att)
eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude  if exist2==1, m(2) robust(2) tc(att)
eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude  if exist2==1, m(2) robust(2) tc(att)
eststo: nnmatch deptotal pog1349 Latitude Longitude  jews33 if exist2==1, m(2) robust(2) tc(att)
eststo: nnmatch stuermersum pog1349 Latitude Longitude  pop33 if exist2==1, m(2) robust(2) tc(att)
eststo: nnmatch syndamordest pog1349 Latitude Longitude  if exist2==1 & syn33==1, m(2) robust(2) tc(att)
esttab using table_Appendix_Predicted_Exist3.rtf, se ar2 label nocons title (Main Results mtitles(GeoMatch GeoMatch GeoMatch GeoMatch GeoMatch GeoMatch))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


////////////////// Medieval Correlates: Predicted pogrom probability /////////////////////////////////////////////////

*Table A.19

probit pog1349 freeimperial market1349 nav_river staufer ln_age1349 if exist1349==1, robust cluster(kreis_nr)
predict pog1349_hat if exist1349==1
gen resid_pog1349 = pog1349-pog1349_hat

*Baseline regs with predicted pogroms: 
eststo: reg pog20 pog1349 pog1349_hat logpop25c perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 pog1349_hat logpop285 perc_JEW25 perc_PROT25  if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 pog1349_hat logpop245 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 pog1349_hat logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 pog1349_hat logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, robust family(poisson)
eststo: reg syndamordest pog1349 pog1349_hat logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 pog1349_hat Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25  if exist1349==1, robust cluster(kreis_nr)
esttab using table_Appendix_Predicted_Pog.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


//////////////////////////////////////////////// Full sample -- only cities that were first mentioned before 1349 /////////////////////////////////////////////

*Table A.20
    *If not in memory anymore from Table A.18:
	* gen exist_city1349 = 1 if age1349~=. | exist1349==1
*Baseline regs
eststo: reg pog20 pog1349 exist1349 logpop25c perc_JEW25 perc_PROT25 if exist_city1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 exist1349 logpop285 perc_JEW25 perc_PROT25  if exist_city1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 exist1349 logpop245 perc_JEW25 perc_PROT25 if exist_city1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 exist1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist_city1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25 if exist_city1349==1, robust family(poisson)
eststo: reg syndamordest pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25 if exist_city1349==1 & syn33==1, robust cluster(kreis_nr)
esttab using table_R3_Main1.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Matching
eststo: nnmatch pog20 pog1349 exist1349 logpop25c perc_JEW25 perc_PROT25 if exist_city1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch pcNSDAP285 pog1349 exist1349 logpop285 perc_JEW25 perc_PROT25 if exist_city1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcDVFP245 pog1349 exist1349 logpop245 perc_JEW25 perc_PROT25 if exist_city1349==1, m(4) robust(4) tc(att)   
eststo: nnmatch deptotal pog1349 exist1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist_city1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch stuermersum pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25 if exist_city1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch syndamordest pog1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25 if exist_city1349==1 & syn33==1, m(4) robust(4) tc(att) 
esttab using table_R3_Main2.rtf, se ar2 label nocons title (Main Results mtitles(ME ME ME ME ME ME))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Geo-Matching
eststo: nnmatch pog20 pog1349 exist1349 Latitude Longitude  if exist_city1349==1, m(2) robust(2) tc(att) keep(geomatch19A, replace) 
eststo: nnmatch pcNSDAP285 pog1349 exist1349 Latitude Longitude  if exist_city1349==1, m(2) robust(2) tc(att) keep(geomatch20A, replace) 
eststo: nnmatch pcDVFP245 pog1349 exist1349 Latitude Longitude  if exist_city1349==1, m(2) robust(2) tc(att) keep(geomatch21A, replace) 
eststo: nnmatch deptotal pog1349 exist1349 Latitude Longitude  jews33 if exist_city1349==1, m(2) robust(2) tc(att) keep(geomatch22A, replace) 
eststo: nnmatch stuermersum pog1349 exist1349 Latitude Longitude  pop33 if exist_city1349==1, m(2) robust(2) tc(att) keep(geomatch23A, replace) 
eststo: nnmatch syndamordest pog1349 exist1349 Latitude Longitude  if exist_city1349==1 & syn33==1, m(2) robust(2) tc(att) keep(geomatch24A, replace) 
esttab using table_R3_Main3.rtf, se ar2 label nocons title (Main Results mtitles(GeoMatch GeoMatch GeoMatch GeoMatch GeoMatch GeoMatch))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear	


///////////////////////////////////  	Extinct Jewish communities    ////////////////////////////////////////////////////////

*** Surviving and extinguished Jewish communities in 1349 -- Restricted Sample ***
*Table A.21 - A
eststo: reg pog20 pogvanish1349 pogsurvive1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pogvanish1349 pogsurvive1349 logpop285 perc_JEW25 perc_PROT25  if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pogvanish1349 pogsurvive1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pogvanish1349 pogsurvive1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1, robust family(poisson)
eststo: glm stuermersum pogvanish1349 pogsurvive1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, robust family(poisson)
eststo: reg syndamordest pogvanish1349 pogsurvive1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pogvanish1349 pogsurvive1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, cluster(kreis_nr)
esttab using table_Appendix_Vanish_Restr.rtf, se ar2 label nocons title (Main Results at the kreis_nr level) mtitles(OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*** Surviving and extinguished Jewish communities in 1349 -- Full Sample ***
*Table A.21 - B	
eststo: reg pog20 pogvanish1349 pogsurvive1349 exist1349 logpop25c perc_JEW25 perc_PROT25, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pogvanish1349 pogsurvive1349 exist1349 logpop285 perc_JEW25 perc_PROT25, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pogvanish1349 pogsurvive1349 exist1349 logpop245 perc_JEW25 perc_PROT25, robust cluster(kreis_nr)
eststo: glm deptotal pogvanish1349 pogsurvive1349 exist1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25, robust family(poisson)
eststo: glm stuermersum pogvanish1349 pogsurvive1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25, robust family(poisson)
eststo: reg syndamordest pogvanish1349 pogsurvive1349 exist1349 logpop33_dep perc_JEW33 perc_PROT25 if syn33==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pogvanish1349 pogsurvive1349 exist1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25, cluster(kreis_nr)
esttab using table_Appendix_Vanish_Full.rtf, se ar2 label nocons title (Main Results) mtitles(OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

//////////////////////////////////////////////// Interpretation of Transmission -- Robustness /////////////////////////////////////////////	

********* Robustness of interaction terms from Table 9, main paper: ******************

*Table A.22:
*Open City - Dummy
eststo: reg  Std_PCA_20C_AS pog1349 d_open int_d_open Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & Latitude<min_Lat_Hanse, cluster(kreis_nr)
*City Growth interaction -- dummy for above-median growth:
eststo: reg  Std_PCA_20C_AS pog1349 gr1750_abMed int_gr1750_abMed Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & popgrowth1750~=., cluster(kreis_nr)
*City Growth interaction -- Growth due to industrialization: Among the cities with above-median pop growth, also include an interaction for the 50% most industrialized ones by 1933.
eststo: reg  Std_PCA_20C_AS pog1349 gr1750_abMed int_gr1750_abMed gr1750_Ind inter_gr1750_Ind Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & popgrowth1750~=., cluster(kreis_nr)
*Now sample split by characteristics
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & Hanse==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & Hanse==0, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & d_open==1 & Latitude<min_Lat_Hanse, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & d_open==0 & Latitude<min_Lat_Hanse, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & popgrowth1750~=. & gr1750_abMed==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & popgrowth1750~=. & gr1750_abMed==0, robust cluster(kreis_nr)
esttab using table_Robust_Interactions.rtf, se ar2 label nocons title (Dependent variable: Principal Component) mtitles(OLS OLS OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear



////////////////// Pogroms before 1347:  /////////////////////////////////////////////////
*Table A.23
ttest pog1096_1347 if firstsettle_num<=1096, by(pog1349)
ttest pog1175_1347 if firstsettle_num<=1175, by(pog1349)
ttest pog1225_1347 if firstsettle_num<=1225, by(pog1349)

*Table A.24, Panel A: 1096-1347
bysort pog1349: sum pog1096_1347 if firstsettle_num<=1096
eststo: reg pog1349 pog1096_1347 if firstsettle_num<=1096, robust
eststo: reg pog1349 pog1096_1347 freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1096,robust
eststo: reg pog1349 pog1096_1347 d_province1-d_province23 if firstsettle_num<=1096, robust
eststo: reg pog1349 pog1096_1347d if firstsettle_num<=1096,robust
eststo: reg pog1349 pog1096_1347d freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1096,robust
eststo: reg pog1349 pog1096_1347d d_province1-d_province23 if firstsettle_num<=1096,robust
eststo: nnmatch pog1349 pog1096_1347d  freeimperial market1349 nav_river staufer ln_age1349 if firstsettle_num<=1096, m(4) robust(4) tc(att)
eststo: nnmatch pog1349 pog1096_1347d Latitude Longitude if firstsettle_num<=1096, m(2) robust(2) tc(att)
esttab using table_Appendix_PrePlaguePogs1.rtf, drop(d_province*) se ar2 label nocons title (Pre-Plague Attacks, 1096-1347. Dep Var: Pog1349) mtitles(OLS OLS OLS OLS OLS OLS ME GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear
*Table A.24, Panel B: 1175-1347
bysort pog1349: sum pog1175_1347 if firstsettle_num<=1175
eststo: reg pog1349 pog1175_1347 if firstsettle_num<=1175, robust
eststo: reg pog1349 pog1175_1347 freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1175,robust
eststo: reg pog1349 pog1175_1347 d_province1-d_province23 if firstsettle_num<=1175, robust
eststo: reg pog1349 pog1175_1347d if firstsettle_num<=1175, robust
eststo: reg pog1349 pog1175_1347d freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1175,robust
eststo: reg pog1349 pog1175_1347d d_province1-d_province23 if firstsettle_num<=1175, robust
eststo: nnmatch pog1349 pog1175_1347d  freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1175, m(4) robust(4) tc(att)
eststo: nnmatch pog1349 pog1175_1347d Latitude Longitude if firstsettle_num<=1175, m(2) robust(2) tc(att)
esttab using table_Appendix_PrePlaguePogs2.rtf, drop(d_province*) se ar2 label nocons title (Pre-Plague Attacks, 1175-1347. Dep Var: Pog1349) mtitles(OLS OLS OLS OLS OLS OLS ME GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear
*Table A.24, Panel C: 1225-1347
bysort pog1349: sum pog1225_1347 if firstsettle_num<=1225
eststo: reg pog1349 pog1225_1347 if firstsettle_num<=1225,robust
eststo: reg pog1349 pog1225_1347 freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1225,robust
eststo: reg pog1349 pog1225_1347 d_province1-d_province23 if firstsettle_num<=1225,robust 
eststo: reg pog1349 pog1225_1347d if firstsettle_num<=1225, robust
eststo: reg pog1349 pog1225_1347d freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1225,robust
eststo: reg pog1349 pog1225_1347d d_province1-d_province23 if firstsettle_num<=1225,robust
eststo: nnmatch pog1349 pog1225_1347d  freeimperial market1349 nav_river ln_age1349 if firstsettle_num<=1225, m(4) robust(4) tc(att)
eststo: nnmatch pog1349 pog1225_1347d Latitude Longitude if  firstsettle_num<=1225, m(2) robust(2) tc(att)
esttab using table_Appendix_PrePlaguePogs3.rtf, drop(d_province*) se ar2 label nocons title (Pre-Plague Attacks, 1225-1347. Dep Var: Pog1349) mtitles(OLS OLS OLS OLS OLS OLS ME GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

////////////////// Pogroms before 1347, and 20C outcomes:  /////////////////////////////////////////////////
*Table A.25
eststo: reg Std_PCA_20C_AS  pog1096_1347 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if firstsettle_num<=1096, robust cluster(kreis_nr)
eststo: nnmatch Std_PCA_20C_AS  pog1096_1347d Longitude Latitude if firstsettle_num<=1096, m(2) robust(2) tc(att)
eststo: reg Std_PCA_20C_AS  pog1175_1347 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if firstsettle_num<=1175, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS  pog1175_1347 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 d_province1-d_province23 if firstsettle_num<=1175, robust cluster(kreis_nr)
eststo: nnmatch Std_PCA_20C_AS  pog1175_1347d Longitude Latitude if firstsettle_num<=1175, m(2) robust(2) tc(att)
eststo: reg Std_PCA_20C_AS  pog1225_1347 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if firstsettle_num<=1225, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS  pog1225_1347 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 d_province1-d_province23 if firstsettle_num<=1225, robust cluster(kreis_nr)
eststo: nnmatch Std_PCA_20C_AS  pog1225_1347d Longitude Latitude if firstsettle_num<=1225, m(2) robust(2) tc(att)
esttab using table_Appendix_PrePlaguePogs_and_PCA.rtf, drop(d_province*) se ar2 label nocons title (Pre-Plague Attacks and 20C Anti-Semitism Dep Var: Principal Component) mtitles(OLS GeoMatch OLS OLS GeoMatch OLS OLS GeoMatch OLS OLS GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

////////////////// Judensau and correlates:  /////////////////////////////////////////////////
*Table A.26
ttest pogpre1347 if exist1349==1 & pog1349~=., by(judensau)
ttest pog1349 if exist1349==1 & pog1349~=., by(judensau)
ttest hephep if exist1349==1 & pog1349~=., by(judensau)
ttest hephep_all if exist1349==1 & pog1349~=., by(judensau)
ttest Std_PCA_20C_AS if exist1349==1 & pog1349~=., by(judensau)

*Table A.27 -- Black Death pogroms and “Judensau” sculptures: Regressions
eststo: reg judensau pog1349 if exist1349==1, robust
eststo: reg judensau pog1349 freeimperial market1349 nav_river ln_age1349 if exist1349==1,robust
eststo: reg judensau pog1349 d_province1-d_province23 if exist1349==1, robust
eststo: nnmatch judensau pog1349 Latitude Longitude if exist1349==1, m(2) robust(2) tc(att)	
esttab using table_Appendix_JudensauRegs.rtf, drop(d_province*) se ar2 label nocons title (Black Death pogroms and “Judensau” sculptures: Regressions) mtitles(OLS OLS OLS GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

////////////////// Hep-Hep riots and correlates:  /////////////////////////////////////////////////
*Table A.28
ttest pogpre1347 if exist1349==1 & pog1349~=., by(hephep)
ttest pog1349 if exist1349==1 & pog1349~=., by(hephep)
ttest judensau if exist1349==1 & pog1349~=., by(hephep)
ttest Std_PCA_20C_AS if exist1349==1 & pog1349~=., by(hephep)	
	
*Table A.29 -- Black Death pogroms and “Hep-Hep” attacks: Regressions
eststo: reg hephep pog1349 if exist1349==1, robust
eststo: reg hephep pog1349 freeimperial market1349 nav_river ln_age1349 if exist1349==1,robust
eststo: reg hephep pog1349 d_province1-d_province23 if exist1349==1, robust
eststo: nnmatch hephep pog1349 Latitude Longitude if exist1349==1, m(2) robust(2) tc(att)
eststo: reg hephep_all pog1349 if exist1349==1, robust
eststo: reg hephep_all pog1349 freeimperial market1349 nav_river ln_age1349 if exist1349==1,robust
eststo: reg hephep_all pog1349 d_province1-d_province23 if exist1349==1, robust
eststo: nnmatch hephep_all pog1349 Latitude Longitude if exist1349==1, m(2) robust(2) tc(att)
esttab using table_Appendix_HepHepRegs.rtf, drop(d_province*) se ar2 label nocons title (Hep-Hep Riot Attacks) mtitles(OLS OLS OLS GeoMatch OLS OLS OLS GeoMatch)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

