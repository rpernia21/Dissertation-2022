#+++++++Dissertation++++++++####
#--------+---------+---------+---------+---------+---------+---------+---------+
#Political trust and authoritarian values: 
#Theoretical consideration and empirical evidence from the Philippines, South Korea and Taiwan#
#++++++Sept 7, 2021+++++++####
#--------+---------+---------+---------+---------+---------+---------+---------+

rm(list = ls())

library(haven)
library(foreign)
library(car)
library(stargazer)
library(Hmisc)
library(psych)
library(ggplot2)
library(dplyr)

#setup wd
setwd("C:/Users/User/Desktop/R projects/Asian Barometer Survey_Project/Asian Barometer/Authoritarian Values and Political Trust/Dissertation 2021")

#load data####

#+++++++++Wave2+++++++####
Wave2 <- read_dta("Indonesia_Wave2.dta")#View(Wave2)

#trust variables####
#no need to reverse code

Wave2$TrustPresidency <-Wave2$qII07
Wave2$TrustPresidency[Wave2$TrustPresidency>4]<-NA


Wave2$TrustCourts <-Wave2$q007
Wave2$TrustCourts[Wave2$TrustCourts >4]<-NA

Wave2$TrustNatGov <-Wave2$q008
Wave2$TrustNatGov[Wave2$TrustNatGov>4]<-NA

Wave2$TrustParties<-Wave2$q009
Wave2$TrustParties[Wave2$TrustParties>4]<-NA

Wave2$TrustCongress <-Wave2$q010
Wave2$TrustCongress[Wave2$TrustCongress>4]<-NA

Wave2$TrustCivService <-Wave2$q011
Wave2$TrustCivService[Wave2$TrustCivService >4]<-NA

Wave2$TrustMilitary <-Wave2$q012
Wave2$TrustMilitary[Wave2$TrustMilitary>4]<-NA

Wave2$TrustPolice <-Wave2$q013
Wave2$TrustPolice[Wave2$TrustPolice>4]<-NA

#authoritarian values####
Wave2$StrongLeaders <-car::recode(Wave2$q121,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave2$StrongLeaders[Wave2$StrongLeaders>4]<-NA

Wave2$OnePartyRule <-car::recode(Wave2$qII125,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave2$OnePartyRule[Wave2$OnePartyRule>4]<-NAcollabmeet

Wave2$MilitaryRule <-car::recode(Wave2$q123,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave2$MilitaryRule[Wave2$MilitaryRule>4]<-NA 



Wave2$ConformitytoLeaders <-car::recode(Wave2$q133,"1=4;2=3;3=2;4=1")
Wave2$ConformitytoLeaders[Wave2$ConformitytoLeaders>4]<-NA

Wave2$Submission<-car::recode(Wave2$q134,"1=4;2=3;3=2;4=1")
Wave2$Submission[Wave2$Submission>4]<-NA

Wave2$AntiPolPluralism1<-car::recode(Wave2$q135,"1=4;2=3;3=2;4=1")
Wave2$AntiPolPluralism1[Wave2$AntiPolPluralism1>4]<-NA

Wave2$AntiPolPluralism2<-car::recode(Wave2$q139,"1=4;2=3;3=2;4=1")
Wave2$AntiPolPluralism2[Wave2$AntiPolPluralism2>4]<-NA

#economic performance variables####
Wave2$PresentEconEval <-Wave2$q001 #very good (5)
Wave2$PresentEconEval[Wave2$PresentEconEval>5]<-NA

Wave2$RetroEconEval<-Wave2$q002 #much better (5)
Wave2$RetroEconEval[Wave2$RetroEconEval>5]<-NA

Wave2$ProspectEconEval<-Wave2$q003 #much better (5)
Wave2$ProspectEconEval[Wave2$ProspectEconEval>5]<-NA

Wave2$PresentFamEcon <-Wave2$q004
Wave2$PresentFamEcon[Wave2$PresentFamEcon>5]<-NA

Wave2$RetroFamEcon <-Wave2$q005
Wave2$RetroFamEcon[Wave2$RetroFamEcon>5]<-NA

Wave2$ProspectiveFamEcon <-Wave2$q006
Wave2$ProspectiveFamEcon[Wave2$ProspectiveFamEcon>5]<-NA

#Traditional values
Wave2$FollowParents <-car::recode(Wave2$q064,"1=4;2=3;3=2;4=1")
Wave2$FollowParents[Wave2$FollowParents>4]<-NA

Wave2$ConflictAvoidance<-car::recode(Wave2$q066,"1=4;2=3;3=2;4=1")
Wave2$ConflictAvoidance[Wave2$ConflictAvoidance>4]<-NA

Wave2$Passive<- car::recode(Wave2$q068,"1=4;2=3;3=2;4=1")
Wave2$Passive[Wave2$Passive>4]<-NA

Wave2$FamilyFirst<-car::recode(Wave2$q069,"1=4;2=3;3=2;4=1")
Wave2$FamilyFirst[Wave2$FamilyFirst>4]<-NA

#controls####
#fighting corruption
Wave2$CurbCorruption <-Wave2$q108 #5 point scale, much better than before (5)
Wave2$CurbCorruption[Wave2$CurbCorruption>5]<-NA

#political interest
Wave2$PolInterest <-Wave2$q056 #very interested(4)
Wave2$PolInterest[Wave2$PolInterest>4]<-NA

#social trust
Wave2$SocialTrust <-Wave2$q024 # most people can be trusted(2)
Wave2$SocialTrust[Wave2$SocialTrust>2]<-NA

#Democratic Satisfaction
Wave2$DemSatisfaction<-Wave2$q098 #very satisfied (4)
Wave2$DemSatisfaction[Wave2$DemSatisfaction>4]<-NA

#Current regime/Government satisfaction
Wave2$RegimeSatisfaction <-Wave2$q104
Wave2$RegimeSatisfaction[Wave2$RegimeSatisfaction>4]<-NA

#demographic controls####
Wave2$SurveyYear <-2001

#Age
Wave2$Age<-Wave2$se003a

#Gender
Wave2$Gender <-Wave2$se002 # 1 male; 2 female

#income group
Wave2$IncomeGroup <-Wave2$se009 #high income (5); 40K+ and above

#urbanization
Wave2$Urbanization <-car::recode(Wave2$level3,"1=2;2=1")

#education
Wave2$Education <-Wave2$se005 

#religiosity
Wave2$Religiosity<-car::recode(Wave2$se007,"1=9;2=8;3=7;4=6;5=5=6=4;7=3;8=2;9=1")

#Construct indexes####
#index of political trust####
Wave2$PolTrust<-rowMeans(cbind(Wave2$TrustCourts,Wave2$TrustNatGov,Wave2$TrustCongress,
                               Wave2$TrustParties,
                               Wave2$TrustMilitary,Wave2$TrustPolice,
                               Wave2$TrustCivService),na.rm=TRUE)
summary (Wave2$PolTrust)

#index of authoritarian values####
Wave2$AuthoritarianValues <-rowMeans(cbind(Wave2$StrongLeaders,Wave2$ConformitytoLeaders,
                                           Wave2$Submission, Wave2$AntiPolPluralism1,
                                           Wave2$AntiPolPluralism2),na.rm=TRUE)
summary(Wave2$AuthoritarianValues)

#index of economic performance####
Wave2$EconPerf <-rowMeans(cbind(Wave2$PresentEconEval,Wave2$RetroEconEval, Wave2$ProspectEconEval,
                                Wave2$PresentFamEcon,Wave2$RetroFamEcon,Wave2$ProspectiveFamEcon),na.rm=TRUE)
summary(Wave2$EconPerf)

#index of traditional values####
Wave2$Trad.Values <-rowMeans(cbind(Wave2$FollowParents,Wave2$ConflictAvoidance,Wave2$Passive,Wave2$FamilyFirst),na.rm=TRUE)
summary(Wave2$Trad.Values)

#save a copy####
write.dta(Wave2, file="IndonesiaWave2_2021version.dta")


#wave3####

#load data
Wave3 <- read_dta("Indonesia_Wave3.dta")
#View(Wave3)

#trust variables####
# to be reverse code


Wave3$TrustPresidency <-car::recode(Wave3$q7, "1=4;2=3;3=2;4=1")
Wave3$TrustPresidency[Wave3$TrustPresidency>4]<-NA

Wave3$TrustCourts <-car::recode(Wave3$q8, "1=4;2=3;3=2;4=1")
Wave3$TrustCourts[Wave3$TrustCourts>4]<-NA

Wave3$TrustNatGov <-car::recode(Wave3$q9, "1=4;2=3;3=2;4=1")
Wave3$TrustNatGov[Wave3$TrustNatGov>4]<-NA

Wave3$TrustParties<-car::recode(Wave3$q10,"1=4;2=3;3=2;4=1")
Wave3$TrustParties[Wave3$TrustParties>4]<-NA

Wave3$TrustCongress <-car::recode(Wave3$q11,"1=4;2=3;3=2;4=1")
Wave3$TrustCongress[Wave3$TrustCongress>4]<-NA

Wave3$TrustCivService <-car::recode(Wave3$q12,"1=4;2=3;3=2;4=1")
Wave3$TrustCivService[Wave3$TrustCivService>4]<-NA

Wave3$TrustMilitary <-car::recode(Wave3$q13,"1=4;2=3;3=2;4=1")
Wave3$TrustMilitary[Wave3$TrustMilitary>4]<-NA

Wave3$TrustPolice <-car::recode(Wave3$q14,"1=4;2=3;3=2;4=1")
Wave3$TrustPolice[Wave3$TrustPolice>4]<-NA

#econ performance variables
Wave3$PresentEconEval <-car::recode(Wave3$q1,"1=5;2=4;3=3;4=2;5=1")#very good (5)
Wave3$PresentEconEval[Wave3$PresentEconEval>5]<-NA

Wave3$RetroEconEval<-car::recode(Wave3$q2,"1=5;2=4;3=3;4=2;5=1") #much better (5)
Wave3$RetroEconEval[Wave3$RetroEconEval>5]<-NA

Wave3$ProspectEconEval<-car::recode(Wave3$q3,"1=5;2=4;3=3;4=2;5=1") 
Wave3$ProspectEconEval[Wave3$ProspectEconEval>5]<-NA

Wave3$PresentFamEcon <-car::recode(Wave3$q4,"1=5;2=4;3=3;4=2;5=1") 
Wave3$PresentFamEcon[Wave3$PresentFamEcon >5]<-NA

Wave3$RetroFamEcon <-car::recode(Wave3$q5,"1=5;2=4;3=3;4=2;5=1") 
Wave3$RetroFamEcon[Wave3$RetroFamEcon>5]<-NA

Wave3$ProspectiveFamEcon <-car::recode(Wave3$q6,"1=5;2=4;3=3;4=2;5=1") 
Wave3$ProspectiveFamEcon[Wave3$ProspectiveFamEcon>5]<-NA

#Traditional cultural values
Wave3$FollowParents <-car::recode(Wave3$q55,"1=4;2=3;3=2;4=1") #parents
Wave3$FollowParents[Wave3$FollowParents>4]<-NA

Wave3$ConflictAvoidance<-car::recode(Wave3$q59,"1=4;2=3;3=2;4=1") #neighbor
Wave3$ConflictAvoidance[Wave3$ConflictAvoidance>4]<-NA

Wave3$Passive<- car::recode(Wave3$q60,"1=4;2=3;3=2;4=1") #opinion
Wave3$Passive[Wave3$Passive>4]<-NA

Wave3$FamilyFirst<-car::recode(Wave3$q50,"1=4;2=3;3=2;4=1")
Wave3$FamilyFirst[Wave3$FamilyFirst>4]<-NA

#authoritarian-cultural values####
Wave3$StrongLeaders <-car::recode(Wave3$q132,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave3$StrongLeaders[Wave3$StrongLeaders >4]<-NA

Wave3$OnePartyRule <-car::recode(Wave3$q130,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave3$OnePartyRule [Wave3$OnePartyRule >4]<-NA

Wave3$MilitaryRule <-car::recode(Wave3$q131,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave3$MilitaryRule  [Wave3$MilitaryRule  >4]<-NA

Wave3$ExpertsRule <-car::recode(Wave3$q132,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave3$ExpertsRule [Wave3$ExpertsRule >4]<-NA



Wave3$ConformitytoLeaders <-car::recode(Wave3$q141,"1=4;2=3;3=2;4=1")
Wave3$ConformitytoLeaders[Wave3$ConformitytoLeaders>4]<-NA

Wave3$Submission<-car::recode(Wave3$q142,"1=4;2=3;3=2;4=1")
Wave3$Submission[Wave3$Submission>4]<-NA

Wave3$AntiPolPluralism1<-car::recode(Wave3$q143,"1=4;2=3;3=2;4=1")
Wave3$AntiPolPluralism1[Wave3$AntiPolPluralism1>4]<-NA

Wave3$AntiPolPluralism2<-car::recode(Wave3$q147,"1=4;2=3;3=2;4=1")
Wave3$AntiPolPluralism2[Wave3$AntiPolPluralism2>4]<-NA

#controls####
#fighting corruption

Wave3$CurbCorruption <-car::recode(Wave3$q118,"1=4;2=3;3=2;4=1") #4 very effective
Wave3$CurbCorruption[Wave3$CurbCorruption>4]<-NA

#political interest
Wave3$PolInterest <-car::recode(Wave3$q43,"1=4;2=3;3=2;4=1")
table(Wave3$PolInterest )
Wave3$PolInterest[Wave3$PolInterest >4]<-NA

#social trust
Wave3$SocialTrust <-car::recode(Wave3$q23,"1=2;2=1")# most people can be trusted(2)
table(Wave3$SocialTrust)
Wave3$SocialTrust[Wave3$SocialTrust>2]<-NA
table(Wave3$SocialTrust)

#Democratic Satisfaction
Wave3$DemSatisfaction<-car::recode(Wave3$q89,"1=4;2=3;3=2;4=1")#very satisfied (4)
table(Wave3$DemSatisfaction)
Wave3$DemSatisfaction[Wave3$DemSatisfaction>4]<-NA

#Current regime/Government satisfaction
Wave3$RegimeSatisfaction <-car::recode(Wave3$q95,"1=4;2=3;3=2;4=1")
table(Wave3$RegimeSatisfaction)
Wave3$RegimeSatisfaction[Wave3$RegimeSatisfaction>4]<-NA

#demographic controls####
Wave3$SurveyYear <-2010

#Age
Wave3$Age<-Wave3$se3a

#Gender
Wave3$Gender <-Wave3$se2 # 1 male; 2 female

#income group
Wave3$IncomeGroup <-Wave3$se13 #high income (5); 40K+ and above
Wave3$IncomeGroup[Wave3$IncomeGroup>5]<-NA

#urbanization
Wave3$Urbanization <-car::recode(Wave3$level3,"1=2;2=1")

#education
Wave3$Education <-Wave3$se5

#religiosity
Wave3$Religiosity<-car::recode(Wave3$se7,"1=10;2=9;3=8;4=7;5=6=6=5;7=4;8=3;9=2;10=1")
table(Wave3$se7)
Wave3$se7[Wave3$se7>10]<-NA

#Construct indexes####
#index of political trust####

Wave3$PolTrust<-rowMeans(cbind(Wave3$TrustCourts,Wave3$TrustNatGov,Wave3$TrustCongress,
                               Wave3$TrustMilitary,Wave3$TrustParties,Wave3$TrustCivService,
                               Wave3$TrustPolice),na.rm=TRUE)
summary (Wave3$PolTrust)

#index of authoritarian values####
Wave3$AuthoritarianValues <-rowMeans(cbind(Wave3$StrongLeaders,Wave3$ConformitytoLeaders,
                                           Wave3$Submission, Wave3$AntiPolPluralism1,
                                           Wave3$AntiPolPluralism2),na.rm=TRUE)
summary(Wave3$AuthoritarianValues)

#index of economic performance####
Wave3$EconPerf <-rowMeans(cbind(Wave3$PresentEconEval,Wave3$RetroEconEval, Wave3$ProspectEconEval,
                                Wave3$PresentFamEcon,Wave3$RetroFamEcon,Wave3$ProspectiveFamEcon),na.rm=TRUE)
summary(Wave3$EconPerf)

#index of Traditional values####
Wave3$Trad.Values <-rowMeans(cbind(Wave3$FollowParents,Wave3$ConflictAvoidance,Wave3$Passive,Wave3$FamilyFirst),na.rm=TRUE)
summary(Wave3$Trad.Values)


#save a copy####
write.dta(Wave3, file="IndonesiaWave3_2021version.dta")


#wave4 #subset from merge data####
'W4_v15_merged20181211_release <- read_dta("C:/Users/User/Desktop/R projects/Asian Barometer Survey_Project/Asian Barometer/All Merged Data ABS/W4 Merged Data/W4 Merged Data/W4_v15_merged20181211_release.dta")
Wave4_merge <-data.frame(W4_v15_merged20181211_release )
Indonesia_Wave4 <-subset(Wave4_merge, country==9)
#save a copy####
write.dta(Indonesia_Wave4, file="Indonesia_Wave4.dta")

'
#load data

Wave4 <-read_dta("Indonesia_Wave4.dta")
#View(Wave4)

#trust variables####
# to be reverse code

Wave4$TrustPresidency <-car::recode(Wave4$q7, "1=4;2=3;3=2;4=1")
Wave4$TrustPresidency[Wave4$TrustPresidency>4]<-NA

Wave4$TrustCourts <-car::recode(Wave4$q8, "1=4;2=3;3=2;4=1")
Wave4$TrustCourts[Wave4$TrustCourts>4]<-NA

Wave4$TrustNatGov <-car::recode(Wave4$q9, "1=4;2=3;3=2;4=1")
Wave4$TrustNatGov[Wave4$TrustNatGov>4]<-NA


Wave4$TrustParties<-car::recode(Wave4$q10,"1=4;2=3;3=2;4=1")
Wave4$TrustParties[Wave4$TrustParties>4]<-NA

Wave4$TrustCongress <-car::recode(Wave4$q11,"1=4;2=3;3=2;4=1")
Wave4$TrustCongress[Wave4$TrustCongress>4]<-NA 

Wave4$TrustCivService <-car::recode(Wave4$q12,"1=4;2=3;3=2;4=1")
Wave4$TrustCivService[Wave4$TrustCivService>4]<-NA

Wave4$TrustMilitary <-car::recode(Wave4$q13,"1=4;2=3;3=2;4=1")
Wave4$TrustMilitary[Wave4$TrustMilitary>4]<-NA

Wave4$TrustPolice <-car::recode(Wave4$q14,"1=4;2=3;3=2;4=1")
Wave4$TrustPolice[Wave4$TrustPolice>4]<-NA

#economic performance variables
Wave4$PresentEconEval <-car::recode(Wave4$q1,"1=5;2=4;3=3;4=2;5=1")#very good (5)
Wave4$PresentEconEval[Wave4$PresentEconEval>5]<-NA

Wave4$RetroEconEval<-car::recode(Wave4$q2,"1=5;2=4;3=3;4=2;5=1") #much better (5)
Wave4$RetroEconEval[Wave4$RetroEconEval>5]<-NA

Wave4$ProspectEconEval<-car::recode(Wave4$q3,"1=5;2=4;3=3;4=2;5=1") 
Wave4$ProspectEconEval[Wave4$ProspectEconEval>5]<-NA

Wave4$PresentFamEcon <-car::recode(Wave4$q4,"1=5;2=4;3=3;4=2;5=1") 
Wave4$PresentFamEcon[Wave4$PresentFamEcon >5]<-NA

Wave4$RetroFamEcon <-car::recode(Wave4$q5,"1=5;2=4;3=3;4=2;5=1") 
Wave4$RetroFamEcon[Wave4$RetroFamEcon>5]<-NA

Wave4$ProspectiveFamEcon <-car::recode(Wave4$q6,"1=5;2=4;3=3;4=2;5=1") 
Wave4$ProspectiveFamEcon[Wave4$ProspectiveFamEcon>5]<-NA

#Traditional values
Wave4$FollowParents <-car::recode(Wave4$q60,"1=4;2=3;3=2;4=1") #parents
Wave4$FollowParents[Wave4$FollowParents>4]<-NA

Wave4$ConflictAvoidance<-car::recode(Wave4$q64,"1=4;2=3;3=2;4=1") #neighbor
Wave4$ConflictAvoidance[Wave4$ConflictAvoidance>4]<-NA

Wave4$Passive<- car::recode(Wave4$q65,"1=4;2=3;3=2;4=1") #opinion
Wave4$Passive[Wave4$Passive>4]<-NA

Wave4$FamilyFirst<-car::recode(Wave4$q55,"1=4;2=3;3=2;4=1")
Wave4$FamilyFirst[Wave4$FamilyFirst>4]<-NA

#authoritarian values####
Wave4$StrongLeaders <-car::recode(Wave4$q130,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave4$StrongLeaders[Wave4$StrongLeaders >4]<-NA

Wave4$OnePartyRule <-car::recode(Wave4$q131,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave4$OnePartyRule[Wave4$OnePartyRule >4]<-NA

Wave4$MilitaryRule <-car::recode(Wave4$q132,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave4$MilitaryRule[Wave4$MilitaryRule >4]<-NA

Wave4$ExpertsRule <-car::recode(Wave4$q133,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave4$ExpertsRule[Wave4$ExpertsRule >4]<-NA




Wave4$ConformitytoLeaders <-car::recode(Wave4$q142,"1=4;2=3;3=2;4=1")
Wave4$ConformitytoLeaders[Wave4$ConformitytoLeaders>4]<-NA

Wave4$Submission<-car::recode(Wave4$q143,"1=4;2=3;3=2;4=1")
Wave4$Submission[Wave4$Submission>4]<-NA

Wave4$AntiPolPluralism1<-car::recode(Wave4$q144,"1=4;2=3;3=2;4=1")
Wave4$AntiPolPluralism1[Wave4$AntiPolPluralism1>4]<-NA

Wave4$AntiPolPluralism2<-car::recode(Wave4$q148,"1=4;2=3;3=2;4=1")
Wave4$AntiPolPluralism2[Wave4$AntiPolPluralism2>4]<-NA

#controls####
#fighting corruption
Wave4$CurbCorruption <-car::recode(Wave4$q119,"1=4;2=3;3=2;4=1") #high values indicate it is doing its best, low values it is doing nothing
Wave4$CurbCorruption [Wave4$CurbCorruption>4]<-NA
table(Wave4$CurbCorruption )

#political interest
Wave4$PolInterest <-car::recode(Wave4$q44,"1=4;2=3;3=2;4=1")
table(Wave4$PolInterest )
Wave4$PolInterest[Wave4$PolInterest >4]<-NA

#social trust
Wave4$SocialTrust <-car::recode(Wave4$q23,"1=2;2=1")# most people can be trusted(2)
table(Wave4$SocialTrust)
Wave4$SocialTrust[Wave4$SocialTrust>2]<-NA
table(Wave4$SocialTrust)

#Democratic Satisfaction
Wave4$DemSatisfaction<-car::recode(Wave4$q92,"1=4;2=3;3=2;4=1")#very satisfied (4)
table(Wave4$DemSatisfaction)
Wave4$DemSatisfaction[Wave4$DemSatisfaction>4]<-NA

#Current regime/Government satisfaction
Wave4$RegimeSatisfaction <-car::recode(Wave4$q98,"1=4;2=3;3=2;4=1")
table(Wave4$RegimeSatisfaction)
Wave4$RegimeSatisfaction[Wave4$RegimeSatisfaction>4]<-NA

#demographic controls####
Wave4$SurveyYear <-2014

#Age
Wave4$Age<-Wave4$se3_2

#Gender
Wave4$Gender <-Wave4$se2 # 1 male; 2 female

#income group
Wave4$IncomeGroup <-Wave4$se14 #high income (5); 40K+ and above
Wave4$IncomeGroup[Wave4$IncomeGroup>5]<-NA

#urbanization
Wave4$Urbanization <-Wave4$level

#education
Wave4$Education <-Wave4$se5

#religiosity
Wave4$Religiosity<-car::recode(Wave4$se7,"1=9;2=8;3=7;4=6;5=5=6=4;7=3;8=2;9=1")
table(Wave4$se7)

#Construct indexes####
#index of political trust####
Wave4$PolTrust<-rowMeans(cbind(Wave4$TrustCourts,Wave4$TrustNatGov,Wave4$TrustCongress,
                               Wave4$TrustMilitary,Wave4$TrustParties,Wave4$TrustCivService,
                               Wave4$TrustPolice),na.rm=TRUE)
summary (Wave4$PolTrust)

#index of authoritarian values####
Wave4$AuthoritarianValues <-rowMeans(cbind(Wave4$StrongLeaders,Wave4$ConformitytoLeaders,
                                           Wave4$Submission, Wave4$AntiPolPluralism1,
                                           Wave4$AntiPolPluralism2),na.rm=TRUE)
summary(Wave4$AuthoritarianValues)

#index of economic performance####
Wave4$EconPerf <-rowMeans(cbind(Wave4$PresentEconEval,Wave4$RetroEconEval, Wave4$ProspectEconEval,
                                Wave4$PresentFamEcon,Wave4$RetroFamEcon,Wave4$ProspectiveFamEcon),na.rm=TRUE)
summary(Wave4$EconPerf)

#index of Traditional values####
Wave4$Trad.Values <-rowMeans(cbind(Wave4$FollowParents,Wave4$ConflictAvoidance,Wave4$Passive,Wave4$FamilyFirst),na.rm=TRUE)
summary(Wave4$Trad.Values)

#save a copy####
write.dta(Wave4, file="IndonesiaWave4_2021version.dta")


#merge dataset####
colnames(Wave2)
dat2<-Wave2[,267:308]

colnames(Wave3)
dat3<-Wave3[,260:301]

colnames(Wave4)
dat4<-Wave4[,290:331]


library(dplyr)
merge1 <-full_join(dat2, dat3, index="SurveyYear") 
merge2 <-full_join(merge1, dat4, index="SurveyYear")
merge.final <-full_join(merge1, merge2, index="SurveyYear")

#convert year to dummy variables####
merge.final$SurveyYearR <-factor(merge.final$SurveyYear)

write.dta(merge.final, file="Merge_final_Indonesia_Jan12_version.dta")
colnames(merge.final)


