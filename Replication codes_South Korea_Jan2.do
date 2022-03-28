*Replication code

*South Korea

clear
use merge.final_S.Korea_2021version.dta


*create dichotomous DV

sum PolTrust, detail 

* To check for median : 2.142857 
recode PolTrust 1/2.14 = 0 2.14/4 = 1, gen(PolTrustRE) 
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


*Per waves

*Model 3: Wave1
reg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2003

esttab, replace se
estim stor m3

*Model 4: Wave2
reg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2006

esttab, replace se
estim stor m4

*Model 5: Wave 3
reg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2011

esttab, replace se
estim stor m5


*Model 6: Wave 4
reg PolTrust AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2015

esttab, replace se
estim stor m6

*Alternative specifications

*Model 7: Logit models
logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se
estim stor m7

*Model 8: Logit, fixed-effects

xtlogit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education i.SurveyYear

esttab, replace se
estim stor m8

*Model 9: Wave1
logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2003

esttab, replace se
estim stor m9

*Model 10: Wave 2

logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2006

esttab, replace se
estim stor m10

*Model 11: Wave 3

logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2011

esttab, replace se
estim stor m11

*Model 12: Wave 4

logit PolTrustRE AuthoritarianValues PresentEconEval ProspectiveFamEcon FollowParents ConflictAvoidance PolInterest SocialTrust DemSatisfaction RegimeSatisfaction Age Gender IncomeGroup Urbanization Education if SurveyYear==2015

esttab, replace se
estim stor m12



*export results
esttab m1 m2 m3 m4 using South.Korea_Thesis.Table100.rtf, replace se stat(N r2 r2_a aic bic)

esttab m1 m2 m3 m4 m5 m6 using South.Korea_Thesis.Tabl200.rtf, replace se stat(N r2 r2_a aic bic)

esttab m7 m8 m9 m10 m11 m12 using South.Korea_Thesis.Tabl300.rtf, replace se stat(N r2 r2_a aic bic)


outreg2 m7 m8 m9 m10 m11 m12  using South.Korea_Thesis.Table1.doc
