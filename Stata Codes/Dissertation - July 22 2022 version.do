+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
"Threat, authoritarian values and trust in political institutions: How support for strong leadership moderate the effect of perceived threats on political trust"
replication code 
2022-07-23
Ronald A. Pernia 
rpernia21@gmail.com
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+


clear
use "WVS_Cross-National_Wave_7_stata_v4_0.dta" 

* Step 1: Select specific columns and create variables
*political trust variables
gen trust_army = 5-Q65
gen trust_police = 5-Q69
gen trust_courts = 5-Q70
gen trust_govt = 5-Q71
gen trust_pol_parties = 5-Q72
gen trust_press = 5-Q66
gen trust_tv = 5-Q67
gen trust_congress = 5- Q73
gen trust_civ_serv = 5-Q74
gen trust_elections = 5-Q76

*check for reliability and homogeneity
alpha trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv trust_elections

pca trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv trust_elections

*political trust
gen poltrust_no_prot = (trust_courts + trust_govt + trust_pol_parties + trust_congress + trust_civ_serv + trust_elections)/6

*authoritarian values
gen strong_leader = 5-Q235
gen army_rule = 5- Q237
gen experts_rule = 5-Q236
gen reli_rule = 5-Q239

alpha strong_leader army_rule experts_rule reli_rule
pca strong_leader army_rule experts_rule reli_rule

*proximal threat
gen unsafe_cr_hm = 5-Q52
gen insecure_nghbrs = Q131

*distal threat
gen worry_terrorsm = 5-Q147
gen worry_civ_war = 5-Q148
gen worry_abroad =5-Q146

*controls
gen satis_household_inc = Q50
gen interest_politics = 5-Q199
gen social_trust = 3-Q57
gen dem_perception = Q251
gen corruption = Q112

*socio-demo controls 
gen age = Q262
gen female = Q260
gen education = Q275
gen pray = 9-Q172
gen social_class = 6-Q287

*sources of information
gen info_newspaper = 6-Q201
gen info_TV = 6-Q202
gen info_radio = 6-Q203
gen info_phone = 6-Q204
gen info_email = 6-Q205
gen info_internet = 6-Q206
gen info_socialmedia = 6-Q207
gen info_talk_friends = 6-Q208

*country-level variables
gen libdem = v2x_libdem

* Step 2: Descriptive statistics
* Step 3: Regression results
*Model 1
mixed poltrust_no_prot unsafe_cr_hm worry_terrorsm strong_leader satis_household_inc interest_politics social_trust dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone info_email info_internet info_socialmedia info_talk_friends || libdem:

estim stor m1

*Model 2
mixed poltrust_no_prot c.unsafe_cr_hm##c.strong_leader worry_terrorsm satis_household_inc interest_politics social_trust dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone info_email info_internet info_socialmedia info_talk_friends || libdem:

estim stor m2

*Model 3
mixed poltrust_no_prot c.worry_terrorsm##c.strong_leader unsafe_cr_hm satis_household_inc interest_politics social_trust dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone info_email info_internet info_socialmedia info_talk_friends || libdem:

estim stor m3

*Table 1
esttab m1 m2 m3 using Table1.rtf, replace se stat(N r2 r2_a aic bic)


*Conditional plots
*Model 2
mixed poltrust_no_prot c.unsafe_cr_hm##c.strong_leader worry_terrorsm satis_household_inc interest_politics social_trust dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone info_email info_internet info_socialmedia info_talk_friends || libdem:

margins, dydx(unsafe_cr_hm) at (strong_leader=(1 (0.1) 4)) vsquish
marginsplot, x(strong_leader)
marginsplot, plot1opts(lp(solid) lc(gs1)) ci1opts(lp(dash) lc(gs6)) graphregion(color(white)) title("") subtitle("") xtitle("support for strong leadership")  ytitle("effects of proximal threat on political trust") nolabels recast(line) recastci(rline) yline(0)


*Model 3

mixed poltrust_no_prot c.worry_terrorsm##c.strong_leader unsafe_cr_hm satis_household_inc interest_politics social_trust dem_perception age female social_class education pray info_newspaper info_TV info_radio info_phone info_email info_internet info_socialmedia info_talk_friends || libdem:

margins, dydx(worry_terrorsm) at (strong_leader=(1 (0.1) 4)) vsquish
marginsplot, x(strong_leader)
marginsplot, plot1opts(lp(solid) lc(gs1)) ci1opts(lp(dash) lc(gs6)) graphregion(color(white)) title("") subtitle("") xtitle("support for strong leadership")  ytitle("effects of distal threat on political trust") nolabels recast(line) recastci(rline) yline(0)

*End of replication
