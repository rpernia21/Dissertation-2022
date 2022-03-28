*Replication code

*Taiwan

clear
use merge.final_Taiwan_2021version.dta

*create dichotomous DV

sum PolTrust, detail 

* To check for median :  2.285714 

recode PolTrust 1/2.286 = 0 2.286/4 = 1, gen(PolTrustRE) 
tab PolTrustRE

*OLS models
reg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se

estim stor m1

*fixed effects
xtset SurveyYear

xtreg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se

estim stor m2


*logit models

logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se

estim stor m3

*fixed-effects logit

xtlogit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se

estim stor m4


*export results
esttab m1 m2 m3 m4 using Taiwan_Thesis.Table96.rtf, replace se stat(N r2 r2_a aic bic)


