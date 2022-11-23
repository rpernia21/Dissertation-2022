* Visualization of Dissertation  - Oct 21, 2022

clear
use mergesurveysvdemgtd.dta


* Fig1: Distribution of threats per country per waves
*
label drop survey
label define survey 1 "Wave 6" 2 "Wave 7"
label values survey survey

label drop row
label define row 0 "Autocracies" 1 "Democracies"
label values row row

label define trust_govt 1 "Not at all" 2 "Not much" 3 "A great deal" 4 "Very much"

label values trust_govt trust_govt

set scheme s2mono
graph hbar (mean) unsafe_cr_hm worry_terrorsm, over(row ) over(survey) legend( label(1 "Proximal threat") label(2 "Distal threat") )

cibar unsafe_cr_hm, over1( row ) over2(survey) 

cibar worry_terrorsm, over1( row ) over2(survey) 



graph combine graph1 graph2, row(100) scheme(s2mono) note("Source: World Values Survey (WVS)")

set scheme sj
cibar unsafe_cr_hm, over1( row ) over2(survey) level(95) graphopts(ytitle("Proximal threat (mean)") yscale(range(1.2 1.6)) ylabel(1.2(.1)1.6)  ylabel(1.2(.1)1.6) note("Source: World Values Survey (WVS)"))

cibar worry_terrorsm, over1( row ) over2(survey) level(95) graphopts(ytitle("Distal threat (mean)") yscale(range(2.2 3.1)) ylabel(2.2(.1)3.1)  ylabel(2.2(.1)3.1) note("Source: World Values Survey (WVS)"))






*List of countries per regimes and per waves

*Authoritarian countries in WVS6
tab country_name if row==0 & survey==1

* Authoritarian countries in WVS7
tab country_name if row==0 & survey==2

*Democratic countries in WVS6
tab country_name if row==1 & survey==1

*Democratic countries in WVS7
tab country_name if row==1 & survey==2

gen dem7 = country_name if row==1 & survey==2
gen dem6 = country_name if row==1 & survey==1
gen aut6 = country_name if row==0 & survey==1
gen aut7 = country_name if row==0 & survey==2

*Descriptive statistics

asdoc sum trust_govt strong_leader unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust dem_perception age female education pray social_class info_newspaper info_TV info_radio info_phone info_email info_internet info_talk_friends v2x_libdem v2exl_legitlead gtdscore, replace


*GOvernmental Trust per regime per waves
country_year
set scheme s2mono
graph hbar (mean) trust_govt, over(dem6)

graph hbar (mean) trust_govt, over(dem7)

graph hbar (mean) trust_govt, over(aut6)
graph hbar (mean) trust_govt, over(aut7)

cibar dem6, over(trust_govt)

set scheme s2mono
graph hbar (mean) trust_govt strong_leader, over(row ) over(survey) legend( label(1 "Governmental Trust") label(2 "Support for Strongmen") )



*Trust in Government

set scheme s2mono
cibar trust_govt, over1( row ) over2(survey) level(95) graphopts(ytitle("Trust in Government (mean)") yscale(range(2.1 2.7)) ylabel(2.1(.1)2.7)  ylabel(2.1(.1)2.7) note("Source: World Values Survey (WVS)"))





cibar humanrights, over1(year) level(99.99) graphopts(ytitle("Respect for human rights (mean)") yscale(range(0 4)) ylabel(0(1)4)  ylabel(0(1)4) note("Source: World Values Survey Wave 7 (WVS7)"))


*Support for strongmen

cibar strong_leader, over1( row ) over2(survey) 

graph hbox strong_leader, over(row) over(survey, sort(3)) nooutside 

graph hbar strong_leader, over(row) over(survey)


cibar strong_leader, over1( row ) over2(survey) level(95) graphopts(ytitle("Support for strongmen (mean)") yscale(range(2.1 2.7)) ylabel(2.1(.1)2.7)  ylabel(2.1(.1)2.7) note("Source: World Values Survey (WVS)"))



graph box unsafe_cr_hm worry_terrorsm, over(row ) over(survey) legend( label(1 "Proximal threat") label(2 "Distal threat") )

graph bar unsafe_cr_hm worry_terrorsm, over(row) stack

graph hbar (asis) public private,
over(country, sort(total) descending)
stack
title("Spending on tertiary education as % of GDP,
1999", span position(11) )
subtitle(" ")



graph bar strong_leader, over( row ) over(survey) name(graph1, replace)

cibar strong_leader, over2( row ) over1(survey) 


graph hbar if row==0 , over( strong_leader ) over(survey) name(graph1, replace)

graph hbar if row==1 , over( strong_leader ) over(survey) name(graph2, replace)


graph combine graph1 graph2, row(100) scheme(s2mono) note("Source: World Values Survey (WVS)")


*Plotting person of the leader index over time in Taiwan and the Philippines

*Plotting Person of the Leader index vs LIberal Democracy Index

tsset vdemid vdemyear

set scheme s2mono
tsset countryid Year
xtline PTS_S PTS_H PTS_A



graph hbar (count) gtdscore, over(row)

cibar unsafe_cr_hm, over1( row ) over2(survey) level(95) graphopts(ytitle("Proximal threat (mean)") yscale(range(1.2 1.6)) ylabel(1.2(.1)1.6)  ylabel(1.2(.1)1.6) note("Source: World Values Survey (WVS)"))

set scheme s2mono
graph bar (count) gtdscore, over(row )  note("Source: Global Terrorism Database of University of Maryland") ytitle("Frequency of Terrorist Attacks")


scatter v2x_libdem v2exl_legitlead, mlabel(row) 

scatter v2exl_legitlead v2x_libdem, by(row)

*recode trust_govt 1/2 = 0 3/4 = 1, gen(trust_govtRE)

*recode strong_leader 1/2 = 0 3/4 = 1, gen(strong_leaderRE)
recode army_rule 1/2 = 0 3/4 = 1, gen(army_ruleRE)
recode poltrust_no_prot 1/2 = 0 3/4 = 1, gen( poltrust_no_protRE )


* Create quadrants

recode unsafe_cr_hm 1/2 = 0 3/4 = 1, gen(unsafeRE)
recode worry_abroad 1/2 = 0 3/4 = 1, gen(worry_abroadRE)

gen type1 = country_name if unsafeRE == 1 & worry_abroadRE == 1 & trust_govtRE == 1 & strong_leaderRE == 1

gen type4 = country_name if unsafeRE == 0 & worry_abroadRE == 0

graph hbar, over(type1)
scatter mean_unsafe_crm mean_worry_abroad, by(country_name)

graph hbar unsafeRE worry_abroadRE, by(type1)


*create mean

egen mean_unsafe_crm = mean(unsafe_cr_hm)
egen mean_worry_abroad = mean(worry_abroad)
egen mean_trust_govt = mean(trust_govt)
egen mean_strongleaders = mean(strong_leader)


line mean_trust_govt mean_unsafe_crm, by(country_name)


regress trust_govt unsafe_cr_hm
. predict hat
(option xb assumed; fitted values)
. predict stdf, stdf
. generate lo = hat - 1.96*stdf
. generate hi = hat + 1.96*stdf
. scatter mpg weight || line hat lo hi weight, pstyle(p2 p3 p3) sort


twoway scatter trust_govt unsafe_cr_hm || lfit trust_govt unsafe_cr_hm, by(country_year)



egen mpgmean = mean(mpg)
gen dem7 = country_name if row==1 & survey==2

graph hbar (mean) unsafe_cr_hm worry_terrorsm, over(type1 ) 

cibar unsafe_cr_hm, over1( row ) over2(survey) 


*plots coeffients: only phils and taiwan
set scheme s2mono
reg trust_govt unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

coefplot, xline(0) drop(_cons) msymbol(d) mfcolor(white) levels(99.9 99 95) name(graph1, replace)

reg strong_leader unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

coefplot, xline(0) drop(_cons) msymbol(d) mfcolor(white) levels(99.9 99 95) name(graph2, replace)

graph combine graph1 graph2, xsize(8) 


*other methods
reg trust_govt unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

estim stor trust1


coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("{bf:Confidence in Government}") ylabel(1 "Proximal Threat" 2 "Distal Threat" 3 "Pocketbook Economic Evaluation" 4 "Interest in Politics" 5 "Social Trust" 6 "Democratic Perception" 7 "Age" 8 "Female" 9 "Social Class" 10 "Education" 11 "Religiosity" 12 "Source of information: Newspaper" 13 "Source of information: TV" 14 "Source of information: Radio" 15 "Source of information: Phone" 16 "Source of information: Email" 17 "Source of information: Internet" 18 "Source of information: Talk with Friends") name(graph1, replace)


reg strong_leader unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

estim stor leader2

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("{bf:Support for strong leaders}") ylabel(1 "Proximal Threat" 2 "Distal Threat" 3 "Pocketbook Economic Evaluation" 4 "Interest in Politics" 5 "Social Trust" 6 "Democratic Perception" 7 "Age" 8 "Female" 9 "Social Class" 10 "Education" 11 "Religiosity" 12 "Source of information: Newspaper" 13 "Source of information: TV" 14 "Source of information: Radio" 15 "Source of information: Phone" 16 "Source of information: Email" 17 "Source of information: Internet" 18 "Source of information: Talk with Friends") name(graph2, replace)


coefplot trust1 leader2,xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth ylabel(1 "Proximal Threat" 2 "Distal Threat" 3 "Pocketbook Economic Evaluation" 4 "Interest in Politics" 5 "Social Trust" 6 "Democratic Perception" 7 "Age" 8 "Female" 9 "Social Class" 10 "Education" 11 "Religiosity" 12 "Source of information: Newspaper" 13 "Source of information: TV" 14 "Source of information: Radio" 15 "Source of information: Phone" 16 "Source of information: Email" 17 "Source of information: Internet" 18 "Source of information: Talk with Friends") 


*subset only Taiwan 
keep if country_year == 110 | country_year == 212 
save WVSTaiwan.dta, replace

clear
use WVSTaiwan.dta

reg trust_govt unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

estim stor trust1

reg strong_leader unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

estim stor leader2

coefplot trust1 leader2,xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth ylabel(1 "Proximal Threat" 2 "Distal Threat" 3 "Pocketbook Economic Evaluation" 4 "Interest in Politics" 5 "Social Trust" 6 "Democratic Perception" 7 "Age" 8 "Female" 9 "Social Class" 10 "Education" 11 "Religiosity" 12 "Source of information: Newspaper" 13 "Source of information: TV" 14 "Source of information: Radio" 15 "Source of information: Phone" 16 "Source of information: Email" 17 "Source of information: Internet" 18 "Source of information: Talk with Friends") 


*subset only Philippines
keep if country_year == 139 | country_year == 244
save WVSPhils.dta, replace

clear
use WVSPhils.dta

reg trust_govt unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

estim stor trust1

reg strong_leader unsafe_cr_hm worry_terrorsm satis_household_inc interest_politics social_trust  dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone  info_email info_internet info_talk_friends

estim stor leader2

coefplot trust1 leader2,xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth ylabel(1 "Proximal Threat" 2 "Distal Threat" 3 "Pocketbook Economic Evaluation" 4 "Interest in Politics" 5 "Social Trust" 6 "Democratic Perception" 7 "Age" 8 "Female" 9 "Social Class" 10 "Education" 11 "Religiosity" 12 "Source of information: Newspaper" 13 "Source of information: TV" 14 "Source of information: Radio" 15 "Source of information: Phone" 16 "Source of information: Email" 17 "Source of information: Internet" 18 "Source of information: Talk with Friends") 



*use only wvs 7
clear
use WVSPhils.dta

keep if country == 44


sum strong_leader, detail
*mean: 2.91
sum ideology, detail 
*mean : 6.66
sum vote_natl, detail
*mean: 3.70

recode strong_leader 1/2.91 = 0 2.91/4 = 1, gen(strong_leaderN)
recode ideology 1/6.66 = 0 6.66/10 = 1, gen(ideologyN)
recode vote_natl 1/3.69 = 0 3.69/4 = 1, gen(vote_R)

gen prodictator = strong_leaderN == 1 & ideology == 1

oprobit vote_R strong_leader ideology


reg vote_natl strong_leaderN ideologyN

reg vote_natl i.strong_leaderN##i.ideologyN

reg vote_natl prodictator


reg vote_natl strong_leader ideology dem_perception interest_politics corruption age female education pray social_class

estim stor m1
esttab m1


*conditional effects
reg vote_natl strong_leader ideology interest_politics dem_perception corruption age female education pray social_class

estim stor m1

reg vote_natl c.strong_leader##c.interest_politics ideology dem_perception corruption age female education pray social_class

estim stor m2

reg vote_natl c.ideology##c.interest_politics strong_leader dem_perception corruption age female education pray social_class

estim stor m3

esttab m1 m2 m3 using marcos.rtf, replace


save WVSPhils.dta, replace