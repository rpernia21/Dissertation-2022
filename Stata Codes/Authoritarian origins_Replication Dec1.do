
*Authoritarian Origins
*Replication*

clear
*use Origins_Dec2021.dta
use Origins_merge.finals.2021version.dta

*Political Trust figures

hist PolTrust , freq width(.15) lcolor(gs5) color(gs12) name(graph1, replace) title("Pooled") ytitle("Frequency") xtitle("Levels of Political Trust")

hist PolTrust if SurveyYear==1 , freq width(.15) lcolor(gs5) color(gs12) name(graph2, replace) title("Wave 1 (2002)") ytitle("Frequency") xtitle("Levels of Political Trust")

hist PolTrust if SurveyYear==2 , freq width(.15) lcolor(gs5) color(gs12) name(graph3, replace) title("Wave 2 (2005)") ytitle("Frequency") xtitle("Levels of Political Trust")

hist PolTrust if SurveyYear==3 , freq width(.15) lcolor(gs5) color(gs12) name(graph4, replace) title("Wave 3 (2010)") ytitle("Frequency") xtitle("Levels of Political Trust")

hist PolTrust if SurveyYear==4 , freq width(.15) lcolor(gs5) color(gs12) name(graph5, replace) title("Wave 4 (2014)") ytitle("Frequency") xtitle("Levels of Political Trust")

hist PolTrust if SurveyYear==5 , freq width(.15) lcolor(gs5) color(gs12) name(graph6, replace) title("Wave 5 (2018)") ytitle("Frequency") xtitle("Levels of Political Trust")

graph combine graph2 graph3 graph4 graph5 graph6 graph1, row(2)  note("Source: Asian Barometer Survey Wave 5 (ABS5)") 

*Authoritarian values figures

hist AuthoritarianValues , freq width(.33) lcolor(gs5) color(gs12) name(graph1, replace) title("Pooled") ytitle("Frequency") xtitle("Levels of Authoritarian Values")

hist AuthoritarianValues if SurveyYear==1 , freq width(.33) lcolor(gs5) color(gs12) name(graph2, replace) title("Wave 1 (2002)") ytitle("Frequency") xtitle("Levels of Authoritarian Values")

hist AuthoritarianValues if SurveyYear==2 , freq width(.33) lcolor(gs5) color(gs12) name(graph3, replace) title("Wave 2 (2005)") ytitle("Frequency") xtitle("Levels of Authoritarian Values")

hist AuthoritarianValues if SurveyYear==3 , freq width(.33) lcolor(gs5) color(gs12) name(graph4, replace) title("Wave 3 (2010)") ytitle("Frequency") xtitle("Levels of Authoritarian Values")

hist AuthoritarianValues if SurveyYear==4 , freq width(.33) lcolor(gs5) color(gs12) name(graph5, replace) title("Wave 4 (2014)") ytitle("Frequency") xtitle("Levels of Authoritarian Values")

hist AuthoritarianValues if SurveyYear==5 , freq width(.33) lcolor(gs5) color(gs12) name(graph6, replace) title("Wave 5 (2018)") ytitle("Frequency") xtitle("Levels of Authoritarian Values")

graph combine graph2 graph3 graph4 graph5 graph6 graph1, row(2)  note("Source: Asian Barometer Survey Wave 5 (ABS5)") 


*Per institutions
*Figure 3, box and whisker plots

graph box TrustCourts, name(graph1, replace) ytitle("Trust in Courts") scheme(s2mono)

graph box TrustNatGov , name(graph2, replace) ytitle("Trust in National Government") scheme(s2mono)
graph box TrustParties , name(graph3, replace) ytitle("Trust in Political Parties") scheme(s2mono)
graph box TrustCongress , name(graph4, replace) ytitle("Trust in Congress") scheme(s2mono)
graph box TrustCivService , name(graph5, replace) ytitle("Trust in Civil Service") scheme(s2mono)
graph box TrustMilitary , name(graph6, replace) ytitle("Trust in the Military") scheme(s2mono)
graph box TrustPolice , name(graph7, replace) ytitle("Trust in the Police") scheme(s2mono)

graph combine graph1 graph2 graph3 graph4 graph5 graph6 graph7, row(2)  note("Source: Asian Barometer Survey Wave 5 (ABS 5)")



*Regression

*Model 1: OLS

*Add fixed effects

xtset SurveyYear

reg PolTrust AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

estim stor m1

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") name(m1, replace)


esttab m1 using model1.rtf, replace se stat(N aic bic r2 r2_a)

*Model 2: Logit

logit PolTrustR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") name(m3, replace)

estim stor m3
esttab m3 using model3.rtf, replace se stat(N aic bic r2 r2_a)

*plot coefficients

*combine 

graph combine m1 m3

coefplot m1 m3, xline(0, lpattern(dash_dot) lwidth(medium)) drop( _cons) msymbol(T) cismooth ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") 


graph combine m1 m3, xsize(2)

*Combine models

coefplot Pooled CourtsR NatGovR PartiesR CongressR CivServiceR MilitaryR PoliceR, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") 


*Update the graphs on plotting coefficients



*Plot coefficients

*Pooled * Logit
set scheme s2mono

logit PolTrustR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")
 
estim stor Pooled

*Trust courts
logit TrustCourtsR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor CourtsR

*Trust nat govt
logit TrustNatGovR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor NatGovR

*TrustPartiesR
logit TrustPartiesR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor PartiesR

*TrustCongressR

logit TrustCongressR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor CongressR

*TrustCivServiceR
logit TrustCivServiceR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor CivServiceR

*TrustMilitaryR
logit TrustMilitaryR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor MilitaryR

*TrustPoliceR
logit TrustPoliceR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear

coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth xtitle("Odds ratio: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5")

estim stor PoliceR


*Combine models

coefplot Pooled CourtsR NatGovR PartiesR CongressR CivServiceR MilitaryR PoliceR, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") 
 

graph

 
 *OLS Pooled
reg PolTrust AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear


coefplot, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(t) cismooth xtitle("Coefficients: Political Trust") ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") name(m2, replace)

estim stor OLS
 
*Combine models

coefplot Logit OLS, xline(0, lpattern(dash) lwidth(thin)) drop( _cons) msymbol(d) cismooth ylabel(1 "Authoritarian Values" 2 "Follow Parents" 3 "Conflict Avoidance" 4 "Present Economic Evaluation" 5 "Present Family Economic Evaluation" 6 "Political Interest" 7 "Social Trust" 8 "Age"9 "Gender" 10 "Income" 11 "Urbanization" 12 "Education" 13 "Wave 2" 14"Wave 3" 15 "Wave 4" 16 "Wave 5") 
 
 
graph combine b c d e f g h a, xsize()



**

logit TrustCourtsR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m1: margins, at(AuthoritarianValues=(1(.2)4)) post

coefplot m1, at recast(line) lwidth(*2) ciopts(recast(rline) lpattern(dash)) name(model1, replace)


logit TrustNatGovR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m2: margins, at(AuthoritarianValues=(1(.2)4)) post

coefplot m2, at recast(line) lwidth(*2) ciopts(recast(rline) lpattern(dash)) name(model2, replace)

graph combine model1 model2


logit TrustNatGovR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m3: margins, at(AuthoritarianValues=(1(.2)4)) post

logit TrustPartiesR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m4: margins, at(AuthoritarianValues=(1(.2)4)) post

logit TrustCongressR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m5: margins, at(AuthoritarianValues=(1(.2)4)) post

logit TrustMilitaryR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m6: margins, at(AuthoritarianValues=(1(.2)4)) post

logit TrustPoliceR AuthoritarianValues FollowParents ConflictAvoidance PresentEconEval PresentFamEcon PolInterest SocialTrust Age Gender IncomeGroup Urbanization Education i.SurveyYear
eststo m7: margins, at(AuthoritarianValues=(1(.2)4)) post


coefplot m1 m2 m3 m4 m5 m6 m7, at recast(line) lwidth(*2) ciopts(recast(rline) lpattern(dash))

*recast(line) recastci(rarea)
*ciopts(recast(rline) lpattern(dash))
*margins PolTrustR, atmeans

marginsplot


*Cibar
*mean of Political Trust
set scheme s2mono

cibar PolTrust, over1(SurveyYear) level(99.99) graphopts(ytitle("Political Trust (mean)") yscale(range(0 4)) ylabel(0(1)4) text(0.5 1 "Wave 1", place(c)) text(0.5 2 "Wave 2", place(c)) text(0.5 3  "Wave 3", place(c)) text(0.5 4  "Wave 4", place(c)) text(0.5 5  "Wave 5 ", place(c))) 


cibar PolTrust, over1(SurveyYear) level(99.99) graphopts(ytitle("Political Trust (mean)") yscale(range(0 4)) ylabel(0(1)4) text(0.5 1 "Wave 1", place(c)) text(0.5 2 "Wave 2", place(c)) text(0.5 3  "Wave 3", place(c)) text(0.5 4  "Wave 4", place(c)) text(0.5 5  "Wave 5 ", place(c))) 


**Graph bars
*Figure 3

graph hbar TrustCourtsR, over(SurveyYear) name(graph1, replace) ytitle("Trust in Courts") scheme(s2mono)

graph hbar TrustNatGovR , over(SurveyYear) name(graph2, replace) ytitle("Trust in National Government") scheme(s2mono)

graph hbar TrustPartiesR , over(SurveyYear) name(graph3, replace) ytitle("Trust in Political Parties") scheme(s2mono)

graph hbar TrustCongressR , over(SurveyYear) name(graph4, replace) ytitle("Trust in Congress") scheme(s2mono)

graph hbar TrustCivServiceR, over(SurveyYear) name(graph5, replace) ytitle("Trust in Civil Service") scheme(s2mono)

graph hbar TrustMilitaryR , over(SurveyYear) name(graph6, replace) ytitle("Trust in the Military") scheme(s2mono)

graph hbar TrustPoliceR , over(SurveyYear) name(graph7, replace) ytitle("Trust in the Police") scheme(s2mono)

graph combine graph1 graph2 graph3 graph4 graph5 graph6 graph7, row(2) xsize(8) note("Source: Asian Barometer Survey Wave 5 (ABS5)")


*not recoded

**Graph bars
*Figure 3

graph bar TrustCourts, over(SurveyYear) name(graph1, replace) ytitle("Trust in Courts") scheme(s2mono) ylabel(1(1)4)

graph bar TrustNatGov, over(SurveyYear) name(graph2, replace) ytitle("Trust in National Government") scheme(s2mono) ylabel(1(1)4)

graph bar TrustParties, over(SurveyYear) name(graph3, replace) ytitle("Trust in Political Parties") scheme(s2mono) ylabel(1(1)4)

graph bar TrustCongress, over(SurveyYear) name(graph4, replace) ytitle("Trust in Congress") scheme(s2mono) ylabel(1(1)4)

graph bar TrustCivService, over(SurveyYear) name(graph5, replace) ytitle("Trust in Civil Service") scheme(s2mono) ylabel(1(1)4)

graph bar TrustMilitary, over(SurveyYear) name(graph6, replace) ytitle("Trust in the Military") scheme(s2mono) ylabel(1(1)4)

graph bar TrustPolice, over(SurveyYear) name(graph7, replace) ytitle("Trust in the Police") scheme(s2mono) ylabel(1(1)4)

graph bar PolTrust, over(SurveyYear) name(graph8, replace) ytitle("Index of Political Trust") scheme(s2mono) ylabel(1(1)4)


graph combine graph1 graph2 graph3 graph4 graph5 graph6 graph7 graph8 graph9, row(2) xsize(8) note("Source: Asian Barometer Survey (ABS)")



graph hbar TrustPresident, over(SurveyYear) name(graph9, replace) ytitle("Trust in the Presidency") scheme(s2mono) ylabel(1(1)4)

graph hbar PolTrust, over(SurveyYear) name(graph8, replace) ytitle("Index of Political Trust") scheme(s2mono) ylabel(1(1)4)

graph combine graph8 graph9, xsize(8) note("Source: Asian Barometer Survey (ABS)")



**Graph bars
*Figure 4

graph hbar AuthoritarianValues, over(SurveyYear) ytitle("Authoritarian Values (mean)") scheme(s2mono) note("Source: Asian Barometer Survey Wave (ABS)") ylabel(0(1)4) yline(2.33, lstyle ( legend) lwidth(thick) lcolor(gs12)) 


*median
sum AuthoritarianValues, detail


graph box TrustCourts, name(graph1, replace) ytitle("Trust in Courts") scheme(s2mono)

graph box AuthoritarianValues, over(SurveyYear) ytitle("Authoritarian Values") scheme(s2mono) note("Source: Asian Barometer Survey Wave (ABS)") 

