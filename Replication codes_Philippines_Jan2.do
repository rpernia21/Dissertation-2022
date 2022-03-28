*Replication code

*Philippines
clear
use merge.final_Philippines_2021version.dta


*create dichotomous DV

sum PolTrust, detail 

* To check for median : 2.571429 
recode PolTrust 1/2.57 = 0 2.57/4 = 1, gen(PolTrustRE) 
tab PolTrustRE


*Model 1: OLS
reg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se
estim stor m1


*Model 2: OLS, Fixed-effects
xtset SurveyYear

xtreg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se
estim stor m2


*Model 3: Logit models
logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se
estim stor m3

*Model 4: Logit, fixed-effects

xtlogit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se
estim stor m4


*export results
esttab m1 m2 m3 m4 using Philippines_Thesis.Table100.rtf, replace se stat(N r2 r2_a aic bic)


