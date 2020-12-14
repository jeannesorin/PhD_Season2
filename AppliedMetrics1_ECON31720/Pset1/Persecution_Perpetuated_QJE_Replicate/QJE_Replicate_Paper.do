clear
clear matrix
set mem 500m
set matsize 2000
set more off
pause on

*cd "C:\Users\Nico Voigtländer\Research\Persecution Perpetuated\Data and Regressions"
use Dataset_QJE_Replicate.dta

quietly{
	gen exist1349=0 //indicates existence of Jewish community in 1349
	replace exist1349=1 if judaica==1 | comm1349==1 
	
	gen exist1349_noPog=0 //Jewish community in 1349, but no pogrom.
	replace exist1349_noPog = 1 if exist1349==1 & pog1349==0
	
	gen pogvanish1349=0 if pog1349~=. // Jewish community vanishes as a conseqence of pogroms in 1349
	replace pogvanish1349=1 if pog1349==1 & vanish1349==1
	
	gen pogsurvive1349=0 if pog1349~=. // Jewish community attacked but does not vanish as a conseqence of pogroms in 1349
	replace pogsurvive1349=1 if pog1349==1 & vanish1349~=1
	
	
	*************Attacks before the Black Death:**************
	
	foreach z in  rintfleisch1298 armleder1336 crusade1096 crusade1146 guterwerner1287 crusade1309 { 
		recode `z' .=0
	}
		*Attacks before 1347
		foreach z in  localpog1 localpog2 ritualmord1 ritualmord2 hostienfrevel1 hostienfrevel2 { 
			gen `z'd = 0
			replace `z'd =1 if `z'<=1347 & `z'~=. 
		}
		gen pogpre1347 = crusade1096+crusade1146+rintfleisch1298+armleder1336+guterwerner1287+crusade1309+ ///
		localpog1d + localpog2d + ritualmord1d + ritualmord2d + hostienfrevel1d + hostienfrevel2d
		drop localpog1d-hostienfrevel2d
		gen pogpre1347d = 1 if pogpre1347>0
		recode pogpre1347d .=0
		label variable pogpre1347 "Number of attacks before the Black Death"	
		label variable pogpre1347d "Dummy for attack(s) before the Black Death"
		
		*Attacks between 1096 and 1347 -- includes crusades in 1096 and 1146 with altogether 16 attacks
		foreach z in  localpog1 localpog2 ritualmord1 ritualmord2 hostienfrevel1 hostienfrevel2 { 
			gen `z'd_1096 = 0
			replace `z'd_1096 =1 if `z'<=1347 & `z'~=. & `z'>=1096
		}
		gen pog1096_1347 = crusade1096+crusade1146+rintfleisch1298+armleder1336+guterwerner1287+crusade1309+ ///
		localpog1d_1096 + localpog2d_1096 + ritualmord1d_1096 + ritualmord2d_1096 + hostienfrevel1d_1096 + hostienfrevel2d_1096
		drop localpog1d_1096-hostienfrevel2d_1096
		gen pog1096_1347d = 1 if pog1096_1347>0
		recode pog1096_1347d .=0
		label variable pog1096_1347 "Number of attacks 1096-1347"	
		label variable pog1096_1347d "Dummy for attack(s) 1096-1347"
		
		*Attacks between 1175 and 1347
		foreach z in  localpog1 localpog2 ritualmord1 ritualmord2 hostienfrevel1 hostienfrevel2 { 
			gen `z'd_1175 = 0
			replace `z'd_1175 =1 if `z'<=1347 & `z'~=. & `z'>=1175
		}
		gen pog1175_1347 = rintfleisch1298+armleder1336+guterwerner1287+crusade1309+ ///
		localpog1d_1175 + localpog2d_1175 + ritualmord1d_1175 + ritualmord2d_1175 + hostienfrevel1d_1175 + hostienfrevel2d_1175
		drop localpog1d_1175-hostienfrevel2d_1175
		gen pog1175_1347d = 1 if pog1175_1347>0
		recode pog1175_1347d .=0
		label variable pog1175_1347 "Number of attacks 1175-1347"	
		label variable pog1175_1347d "Dummy for attack(s) 1175-1347"
		
		*Attacks between 1225 and 1347
		foreach z in  localpog1 localpog2 ritualmord1 ritualmord2 hostienfrevel1 hostienfrevel2 { 
			gen `z'd_1225 = 0
			replace `z'd_1225 =1 if `z'<=1347 & `z'~=. & `z'>=1225
		}
		gen pog1225_1347 = rintfleisch1298+armleder1336+guterwerner1287+crusade1309+ ///
		localpog1d_1225 + localpog2d_1225 + ritualmord1d_1225 + ritualmord2d_1225 + hostienfrevel1d_1225 + hostienfrevel2d_1225
		drop localpog1d_1225-hostienfrevel2d_1225
		gen pog1225_1347d = 1 if pog1225_1347>0
		recode pog1225_1347d .=0
		label variable pog1225_1347 "Number of attacks 1225-1347"	
		label variable pog1225_1347d "Dummy of attack(s) 1225-1347"
	 	
	
	***************************************
	
	*Population approximation:
	replace  pop33 =  n333pop if  pop33==. //Use values from 1933 election where Census data not available.
	replace n245pop=c25pop/c33pop1*pop33 if n245pop==. // some missing values, city level approximated given data from 1933
	
	gen syndamordest=0
	replace syndamordest =1 if syndam==1 | syndest==1
	label variable syndamordest "synagogue damaged or destroyed"
	replace syn33=1 if syn33==0 & betraum==1 //syndam and syndest are also coded ==1 if a prayer room (betraum) in smaller cities was attacked or destroyed. Thus: the indicator for an existing synagogue in 1933 must be adjusted 
	label variable syn33 "synagogue or prayer room existed in 1933"
	
	*Population Controls
	gen logpop25c=ln(c25pop)
	gen logpop33c=ln(c33pop1)
	gen logpop33_dep=ln(pop33)
	gen logpop245=ln(n245pop)
	gen logpop285=ln(n285pop)
	gen logpop309=ln(n309pop)
	gen logpop333=ln(n333pop)	
	label variable logpop25c "ln(Pop '25 Census)"
	label variable logpop245 "ln(Pop '24)"
	label variable logpop285 "ln(Pop '28)"
	label variable logpop309 "ln(Pop '30)"
	label variable logpop333 "ln(Pop '33 - election data)"
	label variable logpop33_dep "ln(Pop '33 - census)"
	
	
	gen logpop1300 = ln(1000*pop_1300)
	gen logpop1500 = ln(1000*pop_1500)
	gen logpop1750 = ln(1000*pop_1750)
	label variable logpop1300 "ln(Pop in 1300)"
	label variable logpop1500 "ln(Pop in 1500)"
	label variable logpop1750 "ln(Pop in 1750)"
	
	gen logjews25=log(1+c25juden)
	gen logjews33=log(1+jews33)
	gen logjews39=log(1+jews39)
	label variable logjews25 "ln(# Jews '25)"
	label variable logjews33 "ln(# Jews '33)"
	label variable logjews39 "ln(# Jews '39)"

	*Religion controls
	gen perc_PROT25=100*c25prot/c25pop
	gen perc_CAT25=100*c25kath/c25pop
	gen perc_JEW25=100*c25juden/c25pop
	gen perc_JEW33=100*jews33/pop33 //This is at the city-level!
		replace perc_JEW33 = perc_JEW25  if perc_JEW33==1 //uses best-guess for one weird obs
		replace perc_JEW33 = perc_JEW25 if perc_JEW33==. & perc_JEW25 ~=. & exist1349==1 // some missing values for cities with documented Jewish Communities...
	label variable perc_PROT25 "% Protestant '25"
	label variable perc_CAT25 "% Catholic '25"
	label variable perc_JEW25 "% Jewish '25"
	label variable perc_JEW33 "% Jewish '33"
	
	*Stuermer letters
	gen stuermersum = stuer1+stuer2+stuer3 //sum of the three different categories
	recode stuermersum .=0
	gen log_stuermersum=ln(1+stuermersum)
	gen stuermerper10K=stuermersum/(pop33/10000)
	label variable log_stuermersum  "ln(1 + letters to Stürmer)"
	label variable stuermerper10K  "ln(Letters to Stürmer per 10,000 inhabitants)"
	
	*Deportations
	gen logdeport=ln(1+deptotal)
	gen prop_deport = 100*(deptotal/jews33)
	
	*Election Results
	gen pcDVFP245=n245dvfp/n245gs
	gen pcKPD245=n245kpd/n245gs	
	gen pcZentrum245=n245z/n245gs	
	gen pcNSDAP285=n285nsda/n285gs
	gen pcZentrum285=n285zx/n285gs
	gen pcNSDAP309=n309nsda/n309gs
	gen pcNSDAP327=n327nsda/n327gs
	gen pcNSDAP3211=n32nnsda/n32ngs
	gen pcNSDAP333=n333nsda/n333gs
	gen pcDNVP206= n206dnvp/n206gs
	gen pcDNVP245= n245dnvp/n245gs
	gen pcDVFP_DNVP245 = pcDVFP245+pcDNVP245
	gen pcKPD285 = n285kpd/n285gs
	
	*Election turnout
	gen turnout245 = 100*n245as/n245wb
	gen turnout285 = 100*n285as/n285wb
	gen turnout309 = 100*n309as/n309wb
	gen turnout327 = 100*n327as/n327wb
	gen turnout3211 = 100*n32nas/n32nwb
	gen turnout333 = 100*n333as/n333wb
	
		
	label variable turnout285 "% Elec. turnout"
	label variable pcDVFP245 "City-level DVFP vote, May 1924"
	label variable pcDNVP245 "City-level DNVP vote, May 1924"
	label variable pcDVFP_DNVP245 "City-level combined DVFP+DNVP vote, May 1924"
	label variable pcNSDAP285 "City-level NSDAP vote, May 1928"
	label variable pcNSDAP309 "City-level NSDAP vote, September 1930"
	label variable pcNSDAP327 "City-level NSDAP vote, July 1932"
	label variable pcNSDAP3211 "City-level NSDAP vote, November 1932"
	label variable pcNSDAP333 "City-level NSDAP vote, March 1933"

	
	*Labor Market controls
	gen perc_Unemp33 = 100*c33erlos/c33erwp
	gen perc_Blue25 = 100*c25arbei/c25berwt //workers among ALL workforce "Berufszugehoerige" -- i.e., 'workers by education'
	gen perc_Blue33 = 100*(c33arbei+c33eloar)/c33erwp //workers among ALL workforce (empl and unempl) -- i.e., 'workers by education'
	
	gen perc_Ag25 = 100*c25bland/c25berwt //% ERWERBST.I.D. LAND  U. FORSTWIRTSCHAFT
	gen perc_Ag33 = 100*c33land/c33erwtt //% ERWERBST.I.D. LAND  U. FORSTWIRTSCHAFT
	
	gen perc_Ind25 = 100*c25bwerk/c25berwt //% BERUFSZUGEH. INDUSTRIE UND HANDWERK
	gen perc_Ind33 = 100*c33indu/c33erwtt //% ERWERBST.IN INDUSTRIE U.HANDWERK
	
	gen perc_self25 = 100*c25selb/c25berwt //% Self employed overall
	gen perc_self33 = 100*c33selb/c33erwtt //% Self employed overall
	
	gen perc_RT25 = 100*c25bhand/c33erwtt //% EERWERBST.IN HANDEL U. VERKEHR -- Retail and Trade
	gen perc_RT33 = 100*c33hndl/c33erwtt //% EERWERBST.IN HANDEL U. VERKEHR -- Retail and Trade
	
	gen perc_selfRT25 = 100*c25hselb/c25bhand //% Self employed in retail and trade
	
		*Best proxy for missing values in 1925 is the same ratio from 1933
		replace perc_Ind25= perc_Ind33 if perc_Ind25==.
		replace perc_Blue25= perc_Blue33 if perc_Blue25==.
		replace perc_Ag25 = perc_Ag33 if perc_Ag25==.
		replace perc_selfRT25 = perc_self33 if perc_selfRT25==. //use self employment in all sectors as proxy for self emp in Retail and Trade when missing obs
	
	label variable perc_Unemp33 "% Unemployed '33"
	label variable perc_Blue25 "% Blue collar '25"
	label variable perc_Blue33 "% Blue collar '33"
	label variable perc_Ag25 "% Employment in agriculture '25"
	label variable perc_Ag33 "% Employment in agriculture '33"
	label variable perc_Ind25 "% Employment in industry '25"
	label variable perc_Ind33 "% Employment in industry '33"
	label variable perc_self25 "% Self-Employed '25"
	label variable perc_self33 "% Self-Employed '33"
	label variable perc_selfRT25 "% Self-Employed in retail and trade '25"
	*Hep-Hep riots:
	gen hephep_all = hephep+hephep_instigated
	label var hephep_all "Hep Hep Riots in 1819"

}

*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Generate additional variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quietly{
*%%%%%%%%%%%% Generate Principal Component %%%%%%%%%%%%%%%%%%%%

/*Obtain first principal component*/

	gen stdpog20s = pog20s/pop33
	gen stdstuermer = stuermersum/pop33
	gen stddeport = prop_deport/pop33
	gen stdsyndamordest = syndamordest/pop33

	pca stdpog20s pcDVFP245 pcNSDAP285 stdstuermer stddeport stdsyndamordest if syn33==1 
	predict PCA_20C_AS if syn33==1, score
	pca stdpog20s pcDVFP245 pcNSDAP285 stdstuermer stdsyndamordest if syn33==1
	predict PCA2 if syn33==1, score
	replace PCA_20C_AS = PCA2 if PCA_20C_AS==. & syn33==1
	pca stdpog20s pcDVFP245 pcNSDAP285 stdstuermer stddeport 
	predict PCA3, score
	replace PCA_20C_AS = PCA3 if PCA_20C_AS==.
	pca pcDVFP245 pcNSDAP285 stdstuermer stddeport stdsyndamordest if syn33==1 
	predict PCA4, score
	replace PCA_20C_AS = PCA4 if PCA_20C_AS==. & syn33==1
	drop PCA2 PCA3 PCA4 

//Normalize variables to obtain beta coefficients:
	foreach var in PCA_20C_AS pog1349 logpop285 logpop33_dep perc_JEW25 perc_JEW33 perc_PROT25  perc_Blue33 perc_Ind33 perc_RT33 perc_selfRT25 { //all cities
		egen Std_`var' = std(`var') if pog1349~=.
	}		
	sum Std_*

*%%%%%%%%%%%% Other Characeteristics %%%%%%%%%%%%%%%%%%%%	
	* From Jacob (2010): Hanseatic is a dummy that takes the value of one of the city historically was a member of the Hanseatic League, and zero otherwise. 
	* 	Staufer is a dummy that takes the value of one if it was founded by the Hohenstaufen, and zero otherwise.gen inter_Han = pog1349*hanseatic	
	gen inter_Han = pog1349*Hanse
	gen inter_Free = pog1349*freeimperial
	gen inter_bishop = pog1349*bishop

*City growth interaction
	foreach y in 1300 1750 {
		*gen popgrowth`y' = 0.01*(pop33-1000*pop_`y')/(1000*pop_`y')
		gen popgrowth`y' = ln(pop33/(1000*pop_`y'))
		egen me_popgrowth`y' = median(popgrowth`y') if popgrowth`y'~=. & exist1349==1 & pog1349~=.
		gen gr`y'_abMed = 0 if popgrowth`y'~=.
		replace gr`y'_abMed = 1 if popgrowth`y'>me_popgrowth`y' & popgrowth`y'~=. & exist1349==1 & pog1349~=.
		drop me_popgrowth`y'
		gen int_gr`y'_abMed = gr`y'_abMed*pog1349
		
		egen std_popgrowth`y' = std(popgrowth`y')
		gen inter_std_gr`y' = pog1349*std_popgrowth`y'
	}

*Industrial structure:
xtile Ind_Quartile=perc_Ind33 if exist1349==1, nq(2)
	gen Ind_abMed=0 if Ind_Quartile==1
	replace Ind_abMed=1 if Ind_Quartile==2
	gen inter_Ind_abMed = Ind_abMed*pog1349
	gen inter_Ind = perc_Ind33*pog1349
	
*Isolation
	xtile isolated_x=ruggedness20 if pog1349~=., nq(2)
	gen d_isolated1=0 if isolated_x==1
	replace d_isolated1=1 if isolated_x==2
	replace d_isolated1 = 0 if nav_river==1
	gen inter_d_isolated1 = d_isolated1*pog1349
	drop isolated_x
	gen d_isolated2 = 1 if dist_to_city10>50
	recode d_isolated2 .=0
	gen inter_d_isolated2 = d_isolated2*pog1349
	
*Navigable River:
	gen inter_navRiver = nav_river*pog1349
	
*The most industrialized among the fast-growing cities:
	xtile Xperc_Ind33=perc_Ind33 if exist1349==1 & gr1750_abMed==1, nq(2)
	gen gr1750_Ind=gr1750_abMed if Xperc_Ind33==2 & gr1750_abMed==1
	recode gr1750_Ind .=0 if exist1349==1
	gen inter_gr1750_Ind = gr1750_Ind*pog1349
	drop Xperc_Ind33
	
*Medieval controls from Davide Cantoni and Noam Yuchtman:
	gen market1349=0
	replace market1349=1 if  firstmarket<1349
	gen inter_market1349 = market1349*pog1349

	gen incorp1349=0
	replace incorp1349=1 if incorporation<1349
	gen inter_incorp1349 = incorp1349*pog1349
	
	gen age1349=1349 - firstmention
	replace age1349=. if age1349<0
	gen ln_age1349 = ln(age1349)
	gen age1933=1933 - firstmention
	gen ln_age1933 = ln(age1933)
	
*Age of Jewish community before Black Death
	gen AgeJewComm = 1349-firstsettle_num	
	gen lnAgeJewComm = ln(AgeJewComm)
	label var lnAgeJewComm "ln(Age of Jewish Community in 1349)"
	label var ln_age1349 "ln(Age of City in 1349)"

*Open City Indicator and Dummy:
	gen I_open = freeimperial+nav_river+incorp1349+market1349
	egen std_I_open=std(I_open)
	gen int_I_open =std_I_open*pog1349
	
	gen d_open = min(I_open,1)
	gen int_d_open =d_open*pog1349
	
	xtile Xperc_I_open=I_open if exist1349==1, nq(2)
	gen d_openAbMed=1 if Xperc_I_open==2
	recode d_openAbMed .=0 if exist1349==1
	gen int_d_openAbMed = d_openAbMed*pog1349
	drop Xperc_I_open
	
	*Southern-most Hanseatic town
	egen min_Lat_Hanse = min(Latitude) if Hanse==1
	egen prelim = max(min_Lat_Hanse)
	replace min_Lat_Hanse = prelim - 0.01
	drop prelim
	
		*for footnote: PCA of openness indicator
		pca freeimperial incorp1349 market1349 nav_river 
		predict PCA_open
		egen std_PCAopen = std(PCA_open)
		gen inter_open = std_PCAopen*pog1349
	
}	


*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Figures  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

*The deportation graph
twoway (kdensity prop_deport if exist1349==1 & prop_deport<100 & pog1349==0 [aweight = pop33], bwidth(10) lcolor(black) lpattern(dash)) ///
	(kdensity prop_deport if exist1349==1 & prop_deport<100 & pog1349==1 [aweight = pop33], bwidth(10) lcolor(black) lpattern(solid)), ///
	ytitle(Kernel density) xtitle(Deportations per 100 Jews (in 1933)) legend(order(1 "No pogrom in 1349" 2 "Pogrom in 1349") position(1) ring(0)) scheme(s1color)


*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Analysis  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

//////////////////////////////////////////////// Descriptive Stats /////////////////////////////////////////////=

*Table 1 -- Descriptive statistics -- restricted sample
sum pop33 perc_JEW33 perc_PROT25 syn33 pog20 pcNSDAP285 pcDVFP245 deptotal stuermersum pog1349 if exist1349==1 & pog1349~=. // & perc_JEW33>0 (only affects one obs.)
	sum syndamordest if syn33==1 & exist1349==1 & pog1349~=.  //for existing synagogue

*Table 2 -- Pairwise Correlation:
pwcorr pog1349 pog20s pcDVFP245 pcNSDAP285 prop_deport stuermerper10K syndamordest if pog1349~=. & exist1349==1 [weight=pop33], sig 
	pwcorr pog1349 pog20s pcDVFP245 pcNSDAP285 prop_deport stuermerper10K syndamordest if pog1349~=. & syn33==1  & exist1349==1 [weight=pop33], sig  //for existing synagogue

///////////////////////////////////////////////////////////////////////////////////////////
*******	Unobservables: Do pogroms 1349 predict city characteristics in 20C?  **************	

*Table 3

*Panel A:
ttest popgrowth1300 if exist1349==1, by(pog1349)
ttest popgrowth1750 if exist1349==1, by(pog1349)
ttest perc_PROT25 if exist1349==1, by(pog1349)
ttest perc_JEW33 if exist1349==1, by(pog1349)
ttest perc_Blue33 if exist1349==1, by(pog1349)
ttest perc_Unemp33 if exist1349==1, by(pog1349)
ttest perc_Ind33 if exist1349==1, by(pog1349)
ttest perc_RT33 if exist1349==1, by(pog1349)

*Panel B:
eststo: reg  popgrowth1300 pog1349 logpop1300 perc_JEW33 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg  popgrowth1750 pog1349 logpop1750 perc_JEW33 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg  perc_PROT25 pog1349 logpop25c perc_JEW25 if exist1349==1, cluster(kreis_nr)
eststo: reg  perc_JEW33 pog1349 logpop33_dep perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg  perc_Blue33 pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg  perc_Unemp33 pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg  perc_Ind33 pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, cluster(kreis_nr)
eststo: reg  perc_RT33 pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, cluster(kreis_nr)
esttab using table_Results_Unobs.rtf, se ar2 label nocons title (Main Results at the kreis_nr level) mtitles(OLS OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


*Table 4 -- Conditional averages: 
tabstat pog20, st(mean N) by(pog1349), if exist1349==1 & pog1349~=.
tabstat pcDVFP245, st(mean N) by(pog1349), if exist1349==1 & pog1349~=. 
tabstat pcNSDAP285, st(mean N) by(pog1349), if exist1349==1 & pog1349~=. 
tabstat prop_deport, st(mean N) by(pog1349), if exist1349==1 & pog1349~=. [weight=pop33] //weighted
tabstat stuermerper10K, st(mean N) by(pog1349), if exist1349==1 & pog1349~=. [weight=pop33] //weighted
tabstat syndamordest, st(mean N) by(pog1349), if exist1349==1 & pog1349~=. & syn33==1 

*Footnote:	
tabstat pog1349, st(mean N) by(pog1349), if exist1349==1 & pog1349~=.	



//////////////////////////////////////////////// 2x2 Tables for 1920 Pogroms and Synagogue attacks /////////////////////////////////////////////	
*Table 5
tab pog20 pog1349 if exist1349==1, column
tab syndamordest pog1349 if exist1349==1 & syn33==1, column	
	
//////////////////////////////////////////////// Main Results /////////////////////////////////////////////	

*For Stuermer in text:
gen stuermer_pop = stuermersum/pop33
sum stuermer_pop if exist1349==1 & pog1349==1 [weight=pop33]
sum stuermer_pop if exist1349==1 & pog1349==0 [weight=pop33]
drop stuermer_pop //now calculate 1/mean to find people per letter

sum stuermersum if exist1349==1 & pog1349~=. //-->3.77 letters on average.

*Table 6

*Baseline regs
eststo: reg pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: glm deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1, robust family(poisson)
eststo: glm stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25  if exist1349==1, robust family(poisson)
eststo: reg syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, robust cluster(kreis_nr)
esttab using table_Main1.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear
	*For footnote: controlling for pop33 and jew33 w/o logs yields stronger coefficients on pog1349 but worse fit overall. 
	glm deptotal pog1349 pop33 jews33 perc_JEW33 perc_PROT25 if exist1349==1, robust family(poisson)

*Matching
eststo: nnmatch pog20 pog1349 logpop25c perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch pcNSDAP285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch pcDVFP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att)   
eststo: nnmatch deptotal pog1349 logpop33_dep logjews33 perc_JEW33 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch stuermersum pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1, m(4) robust(4) tc(att)  
eststo: nnmatch syndamordest pog1349 logpop33_dep perc_JEW33 perc_PROT25 if exist1349==1 & syn33==1, m(4) robust(4) tc(att) 
esttab using table_Main2.rtf, se ar2 label nocons title (Main Results mtitles(ME ME ME ME ME ME))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Geo-Matching
eststo: nnmatch pog20 pog1349 Latitude Longitude if exist1349==1, m(2) robust(2) tc(att) keep(geomatch1M, replace) 
eststo: nnmatch pcNSDAP285 pog1349 Latitude Longitude if exist1349==1, m(2) robust(2) tc(att) keep(geomatch2M, replace) 
eststo: nnmatch pcDVFP245 pog1349 Latitude Longitude if exist1349==1, m(2) robust(2) tc(att) keep(geomatch3M, replace) 
eststo: nnmatch deptotal pog1349 Latitude Longitude jews33 if exist1349==1, m(2) robust(2) tc(att) keep(geomatch4M, replace) 
eststo: nnmatch stuermersum pog1349 Latitude Longitude pop33 if exist1349==1, m(2) robust(2) tc(att) keep(geomatch5M, replace) 
eststo: nnmatch syndamordest pog1349 Latitude Longitude if exist1349==1 & syn33==1, m(2) robust(2) tc(att) keep(geomatch6M, replace) 
esttab using table_Main3.rtf, se ar2 label nocons title (Main Results mtitles(GeoMatch GeoMatch GeoMatch GeoMatch GeoMatch GeoMatch))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

	
//////////////////////////////////////////////// Principal component and full sample /////////////////////////////////////////////	
*Table 7

*Baseline regs with further controls
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 Std_perc_Blue33 Std_perc_Ind33 Std_perc_selfRT25 if exist1349==1, robust cluster(kreis_nr)
eststo: nnmatch Std_PCA_20C_AS pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, m(4) robust(4) tc(att) 
eststo: nnmatch Std_PCA_20C_AS pog1349 Longitude Latitude if exist1349==1, m(2) robust(2) tc(att) 
eststo: reg Std_PCA_20C_AS pog1349 exist1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 exist1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 Std_perc_Blue33 Std_perc_Ind33 Std_perc_selfRT25, robust cluster(kreis_nr)
eststo: nnmatch Std_PCA_20C_AS pog1349 exist1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25, m(4) robust(4) tc(att) 
eststo: nnmatch Std_PCA_20C_AS pog1349 exist1349 Longitude Latitude, m(2) robust(2) tc(att) 
esttab using table_PCA.rtf, se ar2 label nocons title (Main Results mtitles(OLS OLS OLS ML ML OLS))  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear


////////////////// Correlates of pogroms in 1349 /////////////////////////////////////////////////

gen exist_city1349 = 1 if age1349~=. | exist1349==1
*Table 8
eststo: reg exist1349 Hanse incorp1349 freeimperial market1349 nav_river  if exist_city1349==1, robust cluster(kreis_nr)
eststo: reg exist1349 bishop staufer d_isolated1 ln_age1349 if exist_city1349==1, robust cluster(kreis_nr)
eststo: reg lnAgeJewComm Hanse incorp1349 freeimperial market1349 nav_river if exist1349==1, robust cluster(kreis_nr)
eststo: reg lnAgeJewComm bishop staufer d_isolated1 ln_age1349 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pog1349 Hanse incorp1349 freeimperial market1349 nav_river if exist1349==1, robust cluster(kreis_nr)
eststo: reg pog1349 bishop staufer d_isolated1 ln_age1349 if exist1349==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 Hanse incorp1349 freeimperial market1349 nav_river Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25  if exist1349==1, robust cluster(kreis_nr)
eststo: reg Std_PCA_20C_AS pog1349 bishop staufer d_isolated1 ln_age1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25  if exist1349==1, robust cluster(kreis_nr)
esttab using table_Results_Medieval_Origins.rtf, se ar2 label nocons title (Main Results at the kreis_nr level) mtitles(OLS OLS OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

*Notes
	*Footnote: Kitchen sink for columns 7 and 8 works, too:
	reg Std_PCA_20C_AS pog1349 Hanse incorp1349 freeimperial market1349 nav_river bishop staufer d_isolated1 ln_age1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25  if exist1349==1, robust cluster(kreis_nr)

	*Age of Jewish community and city age are highly correlated:
	reg lnAgeJewComm ln_age1349 if exist1349==1, robust cluster(kreis_nr)
	reg pog1349 logpop1750 if exist1349==1, robust cluster(kreis_nr)

	
////////////////////////////////// Persistence before and after Black Death  /////////////////////////////////////////////////
*For text
tab pog1096_1347 if exist1349==1 & pog1349~=.
egen check = sum(pog1096_1347) if exist1349==1 & pog1349~=.
sum check
drop check

*Table 9
ttest pogpre1347 if exist1349==1, by(pog1349)
ttest judensau if exist1349==1, by(pog1349)
ttest hephep if exist1349==1, by(pog1349)
ttest hephep_all if exist1349==1, by(pog1349)	

		*For footnote: Also well-identified with FE!
		reg hephep pog1349 d_RGBZ1-d_RGBZ56 if exist1349==1
	
//////////////////////////////////////////////// When persistence fails /////////////////////////////////////////////	

*Table 10
tab Hanse
tab Hanse if exist1349==1

*Hanseatic cities for Principal Component:
eststo: reg  Std_PCA_20C_AS pog1349 Hanse inter_Han Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
*Open City - Indicator
eststo: reg  Std_PCA_20C_AS pog1349 std_I_open int_I_open Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & Latitude<min_Lat_Hanse, cluster(kreis_nr)
*City Growth interaction -- growth rate:
eststo: reg  Std_PCA_20C_AS pog1349  std_popgrowth1750 inter_std_gr1750 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & popgrowth1750~=., cluster(kreis_nr)
*Industrial towns interaction -- dummy for above-median labor share in industry
eststo: reg  Std_PCA_20C_AS pog1349 perc_Ind33 inter_Ind Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
*Bishop interaction for Principal Component:
eststo: reg  Std_PCA_20C_AS pog1349 bishop inter_bishop Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
*Geographic Isolation1 -- based on ruggedness
eststo: reg Std_PCA_20C_AS pog1349 d_isolated1 inter_d_isolated1 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
*Geographic Isolation2 -- based on distance to next large city
eststo: reg Std_PCA_20C_AS pog1349 d_isolated2 inter_d_isolated2 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
esttab using table_Interactions.rtf, se ar2 label nocons title (Dependent variable: Principal Component) mtitles(OLS OLS OLS OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

	*For footnote: Significantly lower anti-Semitism among Hanseatic cities for all cities that DID have pogroms, cities 
	reg  Std_PCA_20C_AS pog1349 Hanse Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
	reg  Std_PCA_20C_AS pog1349 Hanse Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & pog1349==1, robust cluster(kreis_nr)
	reg  Std_PCA_20C_AS pog1349 Hanse Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & pog1349==0, robust cluster(kreis_nr)

	*Including only the open city index:
	reg  Std_PCA_20C_AS pog1349 std_I_open Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if exist1349==1 & Latitude<min_Lat_Hanse, cluster(kreis_nr)

	
////////////////// NSDAP in the 1930s, right-wing parties, and Crime /////////////////////////////////////////////////

*Table 11
eststo: reg pcNSDAP309 pog1349 logpop309 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcNSDAP333 pog1349 logpop333 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)	
eststo: reg pcDNVP245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
eststo: reg pcKPD245 pog1349 logpop245 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)	
eststo: reg pcKPD285 pog1349 logpop285 perc_JEW25 perc_PROT25 if exist1349==1, robust cluster(kreis_nr)
esttab using table_NSDAP30s_extremism_crime1.rtf, se ar2 label nocons title (NSDAP in the 1930s, right-wing parties, and Crime) mtitles(OLS OLS OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
/*For second part of table: Need to collapse at the county level and re-define some variables*/	
preserve
quietly{
*Weighted average Kreis-level pogroms vote in 1928:
bysort kreis: egen tot_pog1349=sum(pog1349)
bysort kreis: egen tot_comm1349=sum(exist1349)


collapse (mean) tot_pog1349 tot_comm1349 Std_PCA_20C_AS logpop33_dep perc_PROT25 perc_JEW25 perc_JEW33 ab0812 th0812 urbpop malpopf fempopf totpop [aweight=pop33], by(kreis_nr)
 
		
	*Crime Variables
	gen log_AssaultBatt=ln(1+ab0812) /* Assault and battery */
	gen pc_AssaultBatt = ab0812/totpop
	
	gen log_Theft=ln(1+th0812) /* Simple theft */
	gen pc_Theft = th0812/totpop
	gen logpop1900 = ln(totpop)
	gen log_PopForeign=ln(1+malpopf+fempopf) /* Foreign-speaking pop */
	gen urb_rate1900 = urbpop/totpop
	gen share_foreing1900 = (malpopf+fempopf)/totpop	
	
	label variable pc_AssaultBatt "Assault and battery p.c. 1908-12" 
	label variable pc_Theft "Simple theft p.c. 1908-12" 
	label variable logpop1900 "ln(Population in 1900)" 
	
	gen dens_pog1349 = tot_pog1349/tot_comm1349
	gen One_pog1349=0 if dens_pog1349~=.
	replace One_pog1349=1 if dens_pog1349>0 & One_pog1349~=.
	replace One_pog1349=1 if dens_pog1349>0 &  One_pog1349~=.
	
	
	foreach var in dens_pog1349 pc_AssaultBatt pc_Theft logpop33_dep perc_JEW33 perc_PROT25 { 
		egen Std_`var' = std(`var')
	}
	
	label variable dens_pog1349 "Pogrom density 1349"
	label variable One_pog1349 "Indicator: One or more pogroms in kreis in 1349"
	label variable Std_perc_PROT25 "% Protestant '25"
	label variable Std_perc_JEW33 "% Jewish '33"
	label variable Std_logpop33_dep "ln(Pop '33)"
	
	label variable pc_AssaultBatt "Assault and battery  per capita 1908-12"
	label variable pc_Theft "Simple theft per capita 1908-12"
	
	label variable logpop1900 "ln(Pop 1900)"	
	label variable log_PopForeign "ln(Foreign Pop 1900)"
	label variable urb_rate "% Urban Pop 1900"
	label variable share_foreing1900 "% Foreign Pop 1900"
}	


*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   Analysis  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

*For footnote: Violent crime not correlated with pogroms.
reg log_AssaultBatt One_pog1349 logpop1900 urb_rate1900 share_foreing1900 if log_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=. , robust 
reg Std_pc_AssaultBatt One_pog1349 logpop1900 urb_rate1900 log_PopForeign if log_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=. , robust

////////////////// NSDAP in the 1930s, right-wing parties, and Crime /////////////////////////////////////////////////

*Table 11
/*For first part of table on NSDAP and right-wing parties: See Regs_QJE_Final*/	
eststo: reg Std_PCA_20C_AS One_pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & pc_AssaultBatt~=. & tot_comm1349>0 & tot_comm1349~=. , robust
eststo: reg Std_PCA_20C_AS One_pog1349 Std_pc_AssaultBatt Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=. , robust
eststo: reg Std_PCA_20C_AS One_pog1349 Std_pc_AssaultBatt Std_pc_Theft Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=. , robust
esttab using table_crime.rtf, se ar2 label nocons title (NSDAP in the 1930s, right-wing parties, and Crime) mtitles(OLS OLS OLS)  star(* 0.10 ** 0.05 *** 0.01) replace
eststo clear

/*For Footnote: Almost identical results with matching based on indicator variable at kreis level*/	
nnmatch Std_PCA_20C_AS One_pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & pc_AssaultBatt~=. & tot_comm1349>0 & tot_comm1349~=., m(4) robust(4)
nnmatch Std_PCA_20C_AS One_pog1349 Std_pc_AssaultBatt Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=., m(4) robust(4)
nnmatch Std_PCA_20C_AS One_pog1349 Std_pc_AssaultBatt pc_Theft Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=., m(4) robust(4)

/*For Footnote: Also almost identical results with pogrom density at kreis level*/
eststo: reg Std_PCA_20C_AS Std_dens_pog1349 Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & pc_AssaultBatt~=. & tot_comm1349>0 & tot_comm1349~=. , robust
eststo: reg Std_PCA_20C_AS Std_dens_pog1349 Std_pc_AssaultBatt Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=. , robust
eststo: reg Std_PCA_20C_AS Std_dens_pog1349 Std_pc_AssaultBatt pc_Theft Std_logpop33_dep Std_perc_JEW33 Std_perc_PROT25 if pc_AssaultBatt>0 & tot_comm1349>0 & tot_comm1349~=. , robust


restore
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
