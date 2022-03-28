*Replication code

*Indonesia

clear
use merge.final_Indonesia.version.dta


*create dichotomous DV

sum PolTrust, detail 

* To check for median : 2.714286 
recode PolTrust 1/2.71 = 0 2.71/4 = 1, gen(PolTrustRE) 
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

logistic PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear


*export results
esttab m1 m2 m3 m4 using Indonesia_Thesis.Table100.rtf, replace se stat(N r2 r2_a aic bic)


*per waves

*2001
logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2001

logistic PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2001

esttab, replace se
estim stor m5

*2010
logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2010

logistic PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2010

esttab, replace se
estim stor m6


*2014
logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2014

logistic PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2014

esttab, replace se
estim stor m7


*export results
esttab m3 m4 m5 m6 m7 using Indonesia_Thesis.Table100.rtf, replace se stat(N r2 r2_a aic bic)


*output summary statistics

asdoc sum PolTrust if SurveyYear==2001
