#+++++++Dissertation++++++++####
#++++++August 5, 2021+++++++####

rm(list = ls())

#setup wd
setwd("C:/Users/User/Desktop/R projects/Asian Barometer Survey_Project/Asian Barometer/Authoritarian Values and Political Trust/Dissertation 2021")

#install libraries####
library(haven)
library(foreign)
library(car)
library(stargazer)
library(Hmisc)
library(psych)
library(ggplot2)
library(dplyr)

#load data####

#+++++++++Wave1+++++++####

Wave1 <- read_sav("Philippines_Wave1.sav")
View(Wave1)

#trust variables####
#no need to reverse code

Wave1$TrustCourts <-Wave1$q007
Wave1$TrustCourts[Wave1$TrustCourts >4]<-NA

Wave1$TrustNatGov <-Wave1$q008
Wave1$TrustNatGov[Wave1$TrustNatGov>4]<-NA

Wave1$TrustParties<-Wave1$q009
Wave1$TrustParties[Wave1$TrustParties>4]<-NA

Wave1$TrustCongress <-Wave1$q010
Wave1$TrustCongress[Wave1$TrustCongress>4]<-NA

Wave1$TrustCivService <-Wave1$q011
Wave1$TrustCivService[Wave1$TrustCivService >4]<-NA

Wave1$TrustMilitary <-Wave1$q012
Wave1$TrustMilitary[Wave1$TrustMilitary>4]<-NA

Wave1$TrustPolice <-Wave1$q013
Wave1$TrustPolice[Wave1$TrustPolice>4]<-NA

#Institutional variables####

Wave1$CurbCorruption <-Wave1$q108 #5 point scale, much better than before (5)
Wave1$CurbCorruption[Wave1$CurbCorruption>4]<-NA

Wave1$NoLocalCorruption <-Wave1$q114 #high values, no local corruption/ hardly anyone is involved (4)
Wave1$NoLocalCorruption[Wave1$NoLocalCorruption>4]<-NA

Wave1$NoNatCorruption <-Wave1$q115 #high values, no national corruption/ hardly anyone is involved (4)
Wave1$NoNatCorruption[Wave1$NoNatCorruption>4]<-NA

#sociotropic/economy-at-large
Wave1$PresentEconEval <-Wave1$q001 #very good (5)
Wave1$PresentEconEval[Wave1$PresentEconEval>5]<-NA

Wave1$RetroEconEval<-Wave1$q002 #much better (5)
Wave1$RetroEconEval[Wave1$RetroEconEval>5]<-NA

Wave1$ProspectEconEval<-Wave1$q003 #much better (5)
Wave1$ProspectEconEval[Wave1$ProspectEconEval>5]<-NA

#pocketbook/individual
Wave1$PresentFamEcon <-Wave1$q004
Wave1$PresentFamEcon[Wave1$PresentFamEcon>5]<-NA

Wave1$RetroFamEcon <-Wave1$q005
Wave1$RetroFamEcon[Wave1$RetroFamEcon>5]<-NA

Wave1$ProspectiveFamEcon <-Wave1$q006
Wave1$ProspectiveFamEcon[Wave1$ProspectiveFamEcon>5]<-NA

#Democratic values/political orientation####
#strongly disagree (4 as high scores)

Wave1$DemV1 <-Wave1$q133 #Government leaders are like the head of a family; we should all follow their decisions.
Wave1$DemV1[Wave1$DemV1>4]<-NA

Wave1$DemV2 <-Wave1$q134 #The government should decide whether certain ideas should be allowed to be discussed in society.
Wave1$DemV2[Wave1$DemV2>4]<-NA

Wave1$DemV3 <-Wave1$q135 #Harmony of the community will be disrupted if people organize lots of groups.
Wave1$DemV3[Wave1$DemV3>4]<-NA

Wave1$DemV4 <-Wave1$q136 #When judges decide important cases, they should accept the view of the executive branch.
Wave1$DemV4[Wave1$DemV4>4]<-NA

Wave1$DemV5 <-Wave1$q137 #If the government is constantly checked [i.e. monitored and supervised] by the legislature,it cannot possibly accomplish great things.
Wave1$DemV5[Wave1$DemV5>4]<-NA

Wave1$DemV6 <-Wave1$q138 #If we have political leaders who are morally upright, we can let them decide everything.
Wave1$DemV6[Wave1$DemV6>4]<-NA

Wave1$DemV7 <-Wave1$q139 #If people have too many different ways of thinking, society will be chaotic.
Wave1$DemV7[Wave1$DemV7>4]<-NA

#Traditional values
Wave1$FollowParents <-car::recode(Wave1$q064,"1=4;2=3;3=2;4=1")
Wave1$FollowParents[Wave1$FollowParents>4]<-NA

Wave1$ConflictAvoidance<-car::recode(Wave1$q066,"1=4;2=3;3=2;4=1")
Wave1$ConflictAvoidance[Wave1$ConflictAvoidance>4]<-NA

Wave1$Passive<- car::recode(Wave1$q068,"1=4;2=3;3=2;4=1")
Wave1$Passive[Wave1$Passive>4]<-NA

Wave1$FamilyFirst<-car::recode(Wave1$q069,"1=4;2=3;3=2;4=1")
Wave1$FamilyFirst[Wave1$FamilyFirst>4]<-NA

#authoritarian values####
Wave1$StrongLeaders <-car::recode(Wave1$q121,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave1$StrongLeaders[Wave1$StrongLeaders>4]<-NA

Wave1$ConformitytoLeaders <-car::recode(Wave1$q133,"1=4;2=3;3=2;4=1")
Wave1$ConformitytoLeaders[Wave1$ConformitytoLeaders>4]<-NA

Wave1$Submission<-car::recode(Wave1$q134,"1=4;2=3;3=2;4=1")
Wave1$Submission[Wave1$Submission>4]<-NA

Wave1$AntiPolPluralism1<-car::recode(Wave1$q135,"1=4;2=3;3=2;4=1")
Wave1$AntiPolPluralism1[Wave1$AntiPolPluralism1>4]<-NA

Wave1$AntiPolPluralism2<-car::recode(Wave1$q139,"1=4;2=3;3=2;4=1")
Wave1$AntiPolPluralism2[Wave1$AntiPolPluralism2>4]<-NA

#controls####

#political interest
Wave1$PolInterest <-Wave1$q056 #very interested(4)
Wave1$PolInterest[Wave1$PolInterest>4]<-NA

#social trust
Wave1$SocialTrust <-Wave1$q024 # most people can be trusted(2)
Wave1$SocialTrust[Wave1$SocialTrust>2]<-NA

#Democratic Satisfaction
Wave1$DemSatisfaction<-Wave1$q098 #very satisfied (4)
Wave1$DemSatisfaction[Wave1$DemSatisfaction>4]<-NA

#Current regime/Government satisfaction
Wave1$RegimeSatisfaction <-Wave1$q104
Wave1$RegimeSatisfaction[Wave1$RegimeSatisfaction>4]<-NA

#demographic controls####

Wave1$SurveyYear <-2002

#Age
Wave1$Age<-Wave1$se003a

#Gender
Wave1$Gender <-Wave1$se002 # 1 male; 2 female

#income group
Wave1$IncomeGroup <-Wave1$se009 #high income (5); 40K+ and above

#urbanization
Wave1$Urbanization <-car::recode(Wave1$level3,"1=2;2=1")

#education
Wave1$Education <-Wave1$se005 

#religiosity
Wave1$Religiosity<-car::recode(Wave1$se007,"1=9;2=8;3=7;4=6;5=5=6=4;7=3;8=2;9=1")

#Construct indexes####
#index of political trust####

Wave1$PolTrust<-rowMeans(cbind(Wave1$TrustCourts,Wave1$TrustNatGov,Wave1$TrustCongress,
                               Wave1$TrustMilitary,Wave1$TrustParties,Wave1$TrustCivService,
                               Wave1$TrustPolice),na.rm=TRUE)

summary (Wave1$PolTrust)

#institutional performance####
Wave1$Inst.performance <-rowMeans(cbind(Wave1$PresentEconEval,Wave1$RetroEconEval, Wave1$ProspectEconEval,
                                        Wave1$PresentFamEcon,Wave1$RetroFamEcon,Wave1$ProspectiveFamEcon,
                                        Wave1$CurbCorruption,Wave1$NoNatCorruption,Wave1$NoLocalCorruption),na.rm=TRUE)
summary(Wave1$Inst.performance)

#Traditional values####
Wave1$Trad.Values <-rowMeans(cbind(Wave1$FollowParents,Wave1$ConflictAvoidance,Wave1$Passive,Wave1$FamilyFirst),na.rm=TRUE)
summary(Wave1$Trad.Values)

#Democratic values####
Wave1$Demo.Values <-rowMeans(cbind(Wave1$DemV1, Wave1$DemV2, Wave1$DemV3,
                                   Wave1$DemV4, Wave1$DemV5, Wave1$DemV6,
                                   Wave1$DemV7),na.rm=TRUE)
summary(Wave1$Demo.Values) #low dem values

#authoritarian-cultural values####
Wave1$AuthoritarianValues <-rowMeans(cbind(Wave1$StrongLeaders,Wave1$ConformitytoLeaders,
                                           Wave1$Submission, Wave1$AntiPolPluralism1,
                                           Wave1$AntiPolPluralism2),na.rm=TRUE)
summary(Wave1$AuthoritarianValues)

#regressions####
summary(Mod1 <-lm(PolTrust~Inst.performance + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave1 ))

summary(Mod2 <-lm(PolTrust~Inst.performance*Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave1 ))

summary(Mod3 <-lm(PolTrust~Inst.performance*DemSatisfaction + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave1 ))

summary(Mod4 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave1 ))

summary(Mod5 <-lm(PolTrust~Inst.performance + Trad.Values*Demo.Values*AuthoritarianValues+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave1 ))

summary(Mod6 <-lm(PolTrust~Inst.performance*Demo.Values*AuthoritarianValues+Trad.Values+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave1 ))

summary(Mod7 <-lm(PolTrust~Inst.performance*DemSatisfaction*RegimeSatisfaction + Trad.Values+ 
                    Demo.Values+AuthoritarianValues+
                     SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                     Religiosity + Urbanization +  Education , data = Wave1 ))

summary(Mod8 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction*RegimeSatisfaction + 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave1 ))

library(stargazer)

stargazer(Mod1, Mod2, Mod3, Mod4, type="text")
stargazer(Mod5, Mod6, Mod7, Mod8, type="text")
stargazer(Mod1, Mod2, Mod3, Mod4, type="html", title="Philippines_Wave1_Table1", out = "Philippines_Wave1_Table1.htm")
stargazer(Mod5, Mod6, Mod7, Mod8, type="html", title="Philippines_Wave1_Table2", out = "Philippines_Wave1_Table2.htm")

#save a copy, for merging dataset####
write.dta(Wave1, file="PhilippinesWave1_2021version.dta")

#+++++++++Wave2+++++++####

Wave2<-read_sav("Philippines_Wave2.sav")
View(Wave2)

#trust variables####
#no need to reverse code

Wave2$TrustCourts <-Wave2$q007
Wave2$TrustCourts[Wave2$TrustCourts>4]<-NA

Wave2$TrustNatGov <-Wave2$q008
Wave2$TrustNatGov[Wave2$TrustNatGov>4]<-NA

Wave2$TrustParties<-Wave2$q009
Wave2$TrustParties[Wave2$TrustParties>4]<-NA

Wave2$TrustCongress <-Wave2$q010
Wave2$TrustCongress[Wave2$TrustCongress>4]<-NA

Wave2$TrustCivService <-Wave2$q011
Wave2$TrustCivService[Wave2$TrustCivService>4]<-NA

Wave2$TrustMilitary <-Wave2$q012
Wave2$TrustMilitary[Wave2$TrustMilitary>4]<-NA

Wave2$TrustPolice <-Wave2$q013
Wave2$TrustPolice[Wave2$TrustPolice>4]<-NA

#Institutional variables####

Wave2$CurbCorruption <-car::recode(Wave2$qII120,"1=4;2=3;3=2;4=1") #4 it is doing its best
Wave2$CurbCorruption[Wave2$CurbCorruption>4]<-NA

Wave2$NoLocalCorruption <-Wave2$q114 #high values, no local corruption/ hardly anyone is involved (4)
Wave2$NoLocalCorruption[Wave2$NoLocalCorruption>4]<-NA

Wave2$NoNatCorruption <-Wave2$q115 #high values, no national corruption/ hardly anyone is involved (4)
Wave2$NoNatCorruption[Wave2$NoNatCorruption>4]<-NA

#sociotropic econ
Wave2$PresentEconEval <-Wave2$q001 #very good (5)
Wave2$PresentEconEval[Wave2$PresentEconEval>5]<-NA

Wave2$RetroEconEval<-Wave2$q002 #much better (5)
Wave2$RetroEconEval[Wave2$RetroEconEval>5]<-NA

Wave2$ProspectEconEval<-Wave2$q003 #much better (5)
Wave2$ProspectEconEval[Wave2$ProspectEconEval>5]<-NA

#pocketbook econ
Wave2$PresentFamEcon <-Wave2$q004
Wave2$PresentFamEcon[Wave2$PresentFamEcon>5]<-NA

Wave2$RetroFamEcon <-Wave2$q005
Wave2$RetroFamEcon[Wave2$RetroFamEcon>5]<-NA

Wave2$ProspectiveFamEcon <-Wave2$q006
Wave2$ProspectiveFamEcon[Wave2$ProspectiveFamEcon>5]<-NA

#Democratic values/political orientation####
#strongly disagree (4 as high scores)

Wave2$DemV1 <-Wave2$q133 #Government leaders are like the head of a family; we should all follow their decisions.
Wave2$DemV1[Wave2$DemV1>4]<-NA

Wave2$DemV2 <-Wave2$q134 #The government should decide whether certain ideas should be allowed to be discussed in society.
Wave2$DemV2[Wave2$DemV2>4]<-NA

Wave2$DemV3 <-Wave2$q135 #Harmony of the community will be disrupted if people organize lots of groups.
Wave2$DemV3[Wave2$DemV3>4]<-NA

Wave2$DemV4 <-Wave2$q136 #When judges decide important cases, they should accept the view of the executive branch.
Wave2$DemV4[Wave2$DemV4>4]<-NA

Wave2$DemV5 <-Wave2$q137 #If the government is constantly checked [i.e. monitored and supervised] by the legislature,it cannot possibly accomplish great things.
Wave2$DemV5[Wave2$DemV5>4]<-NA

Wave2$DemV6 <-Wave2$q138 #If we have political leaders who are morally upright, we can let them decide everything.
Wave2$DemV6[Wave2$DemV6>4]<-NA

Wave2$DemV7 <-Wave2$q139 #If people have too many different ways of thinking, society will be chaotic.
Wave2$DemV7[Wave2$DemV7>4]<-NA

#Traditional cultural values
Wave2$FollowParents <-car::recode(Wave2$q064,"1=4;2=3;3=2;4=1") #parents
Wave2$FollowParents[Wave2$FollowParents>4]<-NA

Wave2$ConflictAvoidance<-car::recode(Wave2$q066,"1=4;2=3;3=2;4=1") #neighbor
Wave2$ConflictAvoidance[Wave2$ConflictAvoidance>4]<-NA

Wave2$Passive<- car::recode(Wave2$q068,"1=4;2=3;3=2;4=1")
Wave2$Passive[Wave2$Passive>4]<-NA

Wave2$FamilyFirst<-car::recode(Wave2$q069,"1=4;2=3;3=2;4=1")
Wave2$FamilyFirst[Wave2$FamilyFirst>4]<-NA

#authoritarian-cultural values####
Wave2$StrongLeaders <-car::recode(Wave2$q121,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave2$StrongLeaders[Wave2$StrongLeaders>4]<-NA

Wave2$ConformitytoLeaders <-car::recode(Wave2$q133,"1=4;2=3;3=2;4=1")
Wave2$ConformitytoLeaders[Wave2$ConformitytoLeaders>4]<-NA

Wave2$Submission<-car::recode(Wave2$q134,"1=4;2=3;3=2;4=1")
Wave2$Submission[Wave2$Submission>4]<-NA

Wave2$AntiPolPluralism1<-car::recode(Wave2$q135,"1=4;2=3;3=2;4=1")
Wave2$AntiPolPluralism1[Wave2$AntiPolPluralism1>4]<-NA

Wave2$AntiPolPluralism2<-car::recode(Wave2$q139,"1=4;2=3;3=2;4=1")
Wave2$AntiPolPluralism2[Wave2$AntiPolPluralism2>4]<-NA

#controls####

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
Wave2$RegimeSatisfaction <-car::recode(Wave2$q104,"1=4;2=3;3=2;4=1")
Wave2$RegimeSatisfaction[Wave2$RegimeSatisfaction>4]<-NA

#demographic controls####

Wave2$SurveyYear <-2005

#Age
Wave2$Age<-Wave2$se003a

#Gender
Wave2$Gender <-Wave2$se002 # 1 male; 2 female

#income group
Wave2$IncomeGroup <-Wave2$se009 #high income (5); 40K+ and above
Wave2$IncomeGroup[Wave2$IncomeGroup>5]<-NA

#urbanization
Wave2$Urbanization <-car::recode(Wave2$level3,"1=2;2=1")
Wave2$Urbanization[Wave2$Urbanization>2]<-NA

#education
Wave2$Education <-Wave2$se005 

#religiosity
Wave2$Religiosity<-car::recode(Wave2$se007,"1=10;2=9;3=8;4=7;5=6=6=5;7=4;8=3;9=2;10=1")
Wave2$Religiosity[Wave2$Religiosity>10]<-NA


#Construct indexes####
#index of political trust####

Wave2$PolTrust<-rowMeans(cbind(Wave2$TrustCourts,Wave2$TrustNatGov,Wave2$TrustCongress,
                               Wave2$TrustMilitary,Wave2$TrustParties,Wave2$TrustCivService,
                               Wave2$TrustPolice),na.rm=TRUE)

summary (Wave2$PolTrust)

#institutional performance####
Wave2$Inst.performance <-rowMeans(cbind(Wave2$PresentEconEval,Wave2$RetroEconEval, Wave2$ProspectEconEval,
                                        Wave2$PresentFamEcon,Wave2$RetroFamEcon,Wave2$ProspectiveFamEcon,
                                        Wave2$CurbCorruption,Wave2$NoNatCorruption,Wave2$NoLocalCorruption),na.rm=TRUE)
summary(Wave2$Inst.performance)

#Traditional values####
Wave2$Trad.Values <-rowMeans(cbind(Wave2$FollowParents,Wave2$ConflictAvoidance,Wave2$Passive,Wave2$FamilyFirst),na.rm=TRUE)
summary(Wave2$Trad.Values)


#Democratic values####
Wave2$Demo.Values <-rowMeans(cbind(Wave2$DemV1, Wave2$DemV2, Wave2$DemV3,
                                   Wave2$DemV4, Wave2$DemV5, Wave2$DemV6,
                                   Wave2$DemV7),na.rm=TRUE)
summary(Wave2$Demo.Values)

#authoritarian-cultural values####

Wave2$AuthoritarianValues <-rowMeans(cbind(Wave2$StrongLeaders,Wave2$ConformitytoLeaders,
                                           Wave2$Submission, Wave2$AntiPolPluralism1,
                                           Wave2$AntiPolPluralism2),na.rm=TRUE)
summary(Wave2$AuthoritarianValues)


#regressions####
summary(Mod1 <-lm(PolTrust~Inst.performance + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave2 ))

summary(Mod2 <-lm(PolTrust~Inst.performance*Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave2 ))

summary(Mod3 <-lm(PolTrust~Inst.performance*DemSatisfaction + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave2 ))

summary(Mod4 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave2 ))

summary(Mod5 <-lm(PolTrust~Inst.performance + Trad.Values*Demo.Values*AuthoritarianValues+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave2 ))

summary(Mod6 <-lm(PolTrust~Inst.performance*Demo.Values*AuthoritarianValues+Trad.Values+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave2 ))

summary(Mod7 <-lm(PolTrust~Inst.performance*DemSatisfaction*RegimeSatisfaction + Trad.Values+ 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave2 ))

summary(Mod8 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction*RegimeSatisfaction + 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave2 ))
library(stargazer)

stargazer(Mod1, Mod2, Mod3, Mod4, type="text")
stargazer(Mod5, Mod6, Mod7, Mod8, type="text")
stargazer(Mod1, Mod2, Mod3, Mod4, type="html", title="Philippines_Wave2_Table1", out = "Philippines_Wave2_Table1.htm")
stargazer(Mod5, Mod6, Mod7, Mod8, type="html", title="Philippines_Wave2_Table2", out = "Philippines_Wave2_Table2.htm")

#save a copy####
write.dta(Wave2, file="PhilippinesWave2_2021version.dta")

#+++++++++Wave3+++++++####

Wave3 <- read_sav("Philippines_Wave3.sav")
View(Wave3)

#trust variables####
# to be reverse code
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


#Institutional variables####

Wave3$CurbCorruption <-car::recode(Wave3$q118,"1=4;2=3;3=2;4=1") #4 very effective
Wave3$CurbCorruption[Wave3$CurbCorruption>4]<-NA

Wave3$NoLocalCorruption <-car::recode(Wave3$q116,"1=4;2=3;3=2;4=1") #high values, no local corruption/ hardly anyone is involved (4)
table(Wave3$NoLocalCorruption )
Wave3$NoLocalCorruption[Wave3$NoLocalCorruption >4]<-NA
table(Wave3$NoLocalCorruption )

Wave3$NoNatCorruption <-car::recode(Wave3$q117,"1=4;2=3;3=2;4=1") #high values, no national corruption/ hardly anyone is involved (4)
table(Wave3$NoNatCorruption)
Wave3$NoNatCorruption[Wave3$NoNatCorruption >4]<-NA


#sociotropic econ
Wave3$PresentEconEval <-car::recode(Wave3$q1,"1=5;2=4;3=3;4=2;5=1")#very good (5)
Wave3$PresentEconEval[Wave3$PresentEconEval>5]<-NA

Wave3$RetroEconEval<-car::recode(Wave3$q2,"1=5;2=4;3=3;4=2;5=1") #much better (5)
Wave3$RetroEconEval[Wave3$RetroEconEval>5]<-NA

Wave3$ProspectEconEval<-car::recode(Wave3$q3,"1=5;2=4;3=3;4=2;5=1") 
Wave3$ProspectEconEval[Wave3$ProspectEconEval>5]<-NA

#pocketbook econ
Wave3$PresentFamEcon <-car::recode(Wave3$q4,"1=5;2=4;3=3;4=2;5=1") 
Wave3$PresentFamEcon[Wave3$PresentFamEcon >5]<-NA

Wave3$RetroFamEcon <-car::recode(Wave3$q5,"1=5;2=4;3=3;4=2;5=1") 
Wave3$RetroFamEcon[Wave3$RetroFamEcon>5]<-NA

Wave3$ProspectiveFamEcon <-car::recode(Wave3$q6,"1=5;2=4;3=3;4=2;5=1") 
Wave3$ProspectiveFamEcon[Wave3$ProspectiveFamEcon>5]<-NA

#Democratic values/political orientation####
#strongly disagree (4 as high scores)

Wave3$DemV1 <-Wave3$q141 #Government leaders are like the head of a family; we should all follow their decisions.
Wave3$DemV1[Wave3$DemV1>4]<-NA

Wave3$DemV2 <-Wave3$q142 #The government should decide whether certain ideas should be allowed to be discussed in society.
Wave3$DemV2[Wave3$DemV2>4]<-NA

Wave3$DemV3 <-Wave3$q143 #Harmony of the community will be disrupted if people organize lots of groups.
Wave3$DemV3[Wave3$DemV3>4]<-NA

Wave3$DemV4 <-Wave3$q144 #When judges decide important cases, they should accept the view of the executive branch.
Wave3$DemV4[Wave3$DemV4>4]<-NA

Wave3$DemV5 <-Wave3$q145 #If the government is constantly checked [i.e. monitored and supervised] by the legislature,it cannot possibly accomplish great things.
Wave3$DemV5[Wave3$DemV5>4]<-NA

Wave3$DemV6 <-Wave3$q146 #If we have political leaders who are morally upright, we can let them decide everything.
Wave3$DemV6[Wave3$DemV6>4]<-NA

Wave3$DemV7 <-Wave3$q147 #If people have too many different ways of thinking, society will be chaotic.
Wave3$DemV7[Wave3$DemV7>4]<-NA

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

Wave3$ConformitytoLeaders <-car::recode(Wave3$q141,"1=4;2=3;3=2;4=1")
Wave3$ConformitytoLeaders[Wave3$ConformitytoLeaders>4]<-NA

Wave3$Submission<-car::recode(Wave3$q142,"1=4;2=3;3=2;4=1")
Wave3$Submission[Wave3$Submission>4]<-NA

Wave3$AntiPolPluralism1<-car::recode(Wave3$q143,"1=4;2=3;3=2;4=1")
Wave3$AntiPolPluralism1[Wave3$AntiPolPluralism1>4]<-NA

Wave3$AntiPolPluralism2<-car::recode(Wave3$q147,"1=4;2=3;3=2;4=1")
Wave3$AntiPolPluralism2[Wave3$AntiPolPluralism2>4]<-NA

#controls####

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

#institutional performance####
Wave3$Inst.performance <-rowMeans(cbind(Wave3$PresentEconEval,Wave3$RetroEconEval, Wave3$ProspectEconEval,
                                        Wave3$PresentFamEcon,Wave3$RetroFamEcon,Wave3$ProspectiveFamEcon,
                                        Wave3$CurbCorruption,Wave3$NoNatCorruption,Wave3$NoLocalCorruption),na.rm=TRUE)
summary(Wave3$Inst.performance)

#Traditional values####
Wave3$Trad.Values <-rowMeans(cbind(Wave3$FollowParents,Wave3$ConflictAvoidance,Wave3$Passive,Wave3$FamilyFirst),na.rm=TRUE)
summary(Wave3$Trad.Values)

#Democratic values####
Wave3$Demo.Values <-rowMeans(cbind(Wave3$DemV1, Wave3$DemV2, Wave3$DemV3,
                                   Wave3$DemV4, Wave3$DemV5, Wave3$DemV6,
                                   Wave3$DemV7),na.rm=TRUE)
summary(Wave3$Demo.Values)

#authoritarian-cultural values####
Wave3$AuthoritarianValues <-rowMeans(cbind(Wave3$StrongLeaders,Wave3$ConformitytoLeaders,
                                           Wave3$Submission, Wave3$AntiPolPluralism1,
                                           Wave3$AntiPolPluralism2),na.rm=TRUE)
summary(Wave3$AuthoritarianValues)


#regressions####
summary(Mod1 <-lm(PolTrust~Inst.performance + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave3 ))

summary(Mod2 <-lm(PolTrust~Inst.performance*Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave3 ))

summary(Mod3 <-lm(PolTrust~Inst.performance*DemSatisfaction + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave3 ))

summary(Mod4 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave3 ))

summary(Mod5 <-lm(PolTrust~Inst.performance + Trad.Values*Demo.Values*AuthoritarianValues+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave3 ))

summary(Mod6 <-lm(PolTrust~Inst.performance*Demo.Values*AuthoritarianValues+Trad.Values+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave3 ))

summary(Mod7 <-lm(PolTrust~Inst.performance*DemSatisfaction*RegimeSatisfaction + Trad.Values+ 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave3 ))

summary(Mod8 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction*RegimeSatisfaction + 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave3 ))
library(stargazer)

stargazer(Mod1, Mod2, Mod3, Mod4, type="text")
stargazer(Mod5, Mod6, Mod7, Mod8, type="text")
stargazer(Mod1, Mod2, Mod3, Mod4, type="html", title="Philippines_Wave3_Table1", out = "Philippines_Wave3_Table1.htm")
stargazer(Mod5, Mod6, Mod7, Mod8, type="html", title="Philippines_Wave3_Table2", out = "Philippines_Wave3_Table2.htm")

#save a copy####
write.dta(Wave3, file="PhilippinesWave3_2021version.dta")

#+++++++++Wave4+++++++####

Wave4 <-read_sav("Philippines_Wave4.sav")
View(Wave4)

#trust variables####
# to be reverse code

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

#Institutional variables####

Wave4$CurbCorruption <-car::recode(Wave4$q119,"1=4;2=3;3=2;4=1") #4 very effective
Wave4$CurbCorruption[Wave4$CurbCorruption>4]<-NA

Wave4$NoLocalCorruption <-car::recode(Wave4$q117,"1=4;2=3;3=2;4=1") #high values, no local corruption/ hardly anyone is involved (4)
table(Wave4$NoLocalCorruption )
Wave4$NoLocalCorruption[Wave4$NoLocalCorruption >4]<-NA
table(Wave4$NoLocalCorruption )

Wave4$NoNatCorruption <-car::recode(Wave4$q118,"1=4;2=3;3=2;4=1") #high values, no national corruption/ hardly anyone is involved (4)
table(Wave4$NoNatCorruption)
Wave4$NoNatCorruption[Wave4$NoNatCorruption >4]<-NA

#sociotropic econ
Wave4$PresentEconEval <-car::recode(Wave4$q1,"1=5;2=4;3=3;4=2;5=1")#very good (5)
Wave4$PresentEconEval[Wave4$PresentEconEval>5]<-NA

Wave4$RetroEconEval<-car::recode(Wave4$q2,"1=5;2=4;3=3;4=2;5=1") #much better (5)
Wave4$RetroEconEval[Wave4$RetroEconEval>5]<-NA

Wave4$ProspectEconEval<-car::recode(Wave4$q3,"1=5;2=4;3=3;4=2;5=1") 
Wave4$ProspectEconEval[Wave4$ProspectEconEval>5]<-NA

#pocketbook econ
Wave4$PresentFamEcon <-car::recode(Wave4$q4,"1=5;2=4;3=3;4=2;5=1") 
Wave4$PresentFamEcon[Wave4$PresentFamEcon >5]<-NA

Wave4$RetroFamEcon <-car::recode(Wave4$q5,"1=5;2=4;3=3;4=2;5=1") 
Wave4$RetroFamEcon[Wave4$RetroFamEcon>5]<-NA

Wave4$ProspectiveFamEcon <-car::recode(Wave4$q6,"1=5;2=4;3=3;4=2;5=1") 
Wave4$ProspectiveFamEcon[Wave4$ProspectiveFamEcon>5]<-NA

#Democratic values/political orientation####
#strongly disagree (4 as high scores)

Wave4$DemV1 <-Wave4$q142 #Government leaders are like the head of a family; we should all follow their decisions.
Wave4$DemV1[Wave4$DemV1>4]<-NA

Wave4$DemV2 <-Wave4$q143 #The government should decide whether certain ideas should be allowed to be discussed in society.
Wave4$DemV2[Wave4$DemV2>4]<-NA

Wave4$DemV3 <-Wave4$q144 #Harmony of the community will be disrupted if people organize lots of groups.
Wave4$DemV3[Wave4$DemV3>4]<-NA

Wave4$DemV4 <-Wave4$q145 #When judges decide important cases, they should accept the view of the executive branch.
Wave4$DemV4[Wave4$DemV4>4]<-NA

Wave4$DemV5 <-Wave4$q146 #If the government is constantly checked [i.e. monitored and supervised] by the legislature,it cannot possibly accomplish great things.
Wave4$DemV5[Wave4$DemV5>4]<-NA

Wave4$DemV6 <-Wave4$q147 #If we have political leaders who are morally upright, we can let them decide everything.
Wave4$DemV6[Wave4$DemV6>4]<-NA

Wave4$DemV7 <-Wave4$q148 #If people have too many different ways of thinking, society will be chaotic.
Wave4$DemV7[Wave4$DemV7>4]<-NA

#Traditional cultural values
Wave4$FollowParents <-car::recode(Wave4$q60,"1=4;2=3;3=2;4=1") #parents
Wave4$FollowParents[Wave4$FollowParents>4]<-NA

Wave4$ConflictAvoidance<-car::recode(Wave4$q64,"1=4;2=3;3=2;4=1") #neighbor
Wave4$ConflictAvoidance[Wave4$ConflictAvoidance>4]<-NA

Wave4$Passive<- car::recode(Wave4$q65,"1=4;2=3;3=2;4=1") #opinion
Wave4$Passive[Wave4$Passive>4]<-NA

Wave4$FamilyFirst<-car::recode(Wave4$q55,"1=4;2=3;3=2;4=1")
Wave4$FamilyFirst[Wave4$FamilyFirst>4]<-NA

#authoritarian-cultural values####
Wave4$StrongLeaders <-car::recode(Wave4$q130,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave4$StrongLeaders[Wave4$StrongLeaders >4]<-NA

Wave4$ConformitytoLeaders <-car::recode(Wave4$q142,"1=4;2=3;3=2;4=1")
Wave4$ConformitytoLeaders[Wave4$ConformitytoLeaders>4]<-NA

Wave4$Submission<-car::recode(Wave4$q143,"1=4;2=3;3=2;4=1")
Wave4$Submission[Wave4$Submission>4]<-NA

Wave4$AntiPolPluralism1<-car::recode(Wave4$q144,"1=4;2=3;3=2;4=1")
Wave4$AntiPolPluralism1[Wave4$AntiPolPluralism1>4]<-NA

Wave4$AntiPolPluralism2<-car::recode(Wave4$q148,"1=4;2=3;3=2;4=1")
Wave4$AntiPolPluralism2[Wave4$AntiPolPluralism2>4]<-NA

#controls####

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

#institutional performance####
Wave4$Inst.performance <-rowMeans(cbind(Wave4$PresentEconEval,Wave4$RetroEconEval, Wave4$ProspectEconEval,
                                        Wave4$PresentFamEcon,Wave4$RetroFamEcon,Wave4$ProspectiveFamEcon,
                                        Wave4$CurbCorruption,Wave4$NoNatCorruption,Wave4$NoLocalCorruption),na.rm=TRUE)
summary(Wave4$Inst.performance)

#Traditional values####
Wave4$Trad.Values <-rowMeans(cbind(Wave4$FollowParents,Wave4$ConflictAvoidance,Wave4$Passive,Wave4$FamilyFirst),na.rm=TRUE)
summary(Wave4$Trad.Values)


#Democratic values####
Wave4$Demo.Values <-rowMeans(cbind(Wave4$DemV1, Wave4$DemV2, Wave4$DemV3,
                                   Wave4$DemV4, Wave4$DemV5, Wave4$DemV6,
                                   Wave4$DemV7),na.rm=TRUE)
summary(Wave4$Demo.Values)

#authoritarian-cultural values####
Wave4$AuthoritarianValues <-rowMeans(cbind(Wave4$StrongLeaders,Wave4$ConformitytoLeaders,
                                           Wave4$Submission, Wave4$AntiPolPluralism1,
                                           Wave4$AntiPolPluralism2),na.rm=TRUE)
summary(Wave4$AuthoritarianValues)

#regressions####
summary(Mod1 <-lm(PolTrust~Inst.performance + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave4 ))

summary(Mod2 <-lm(PolTrust~Inst.performance*Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave4 ))

summary(Mod3 <-lm(PolTrust~Inst.performance*DemSatisfaction + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave4 ))

summary(Mod4 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave4 ))

summary(Mod5 <-lm(PolTrust~Inst.performance + Trad.Values*Demo.Values*AuthoritarianValues+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave4 ))

summary(Mod6 <-lm(PolTrust~Inst.performance*Demo.Values*AuthoritarianValues+Trad.Values+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave4 ))

summary(Mod7 <-lm(PolTrust~Inst.performance*DemSatisfaction*RegimeSatisfaction + Trad.Values+ 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave4 ))

summary(Mod8 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction*RegimeSatisfaction + 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave4 ))
library(stargazer)

stargazer(Mod1, Mod2, Mod3, Mod4, type="text")
stargazer(Mod5, Mod6, Mod7, Mod8, type="text")
stargazer(Mod1, Mod2, Mod3, Mod4, type="html", title="Philippines_Wave4_Table1", out = "Philippines_Wave4_Table1.htm")
stargazer(Mod5, Mod6, Mod7, Mod8, type="html", title="Philippines_Wave4_Table2", out = "Philippines_Wave4_Table2.htm")

#save a copy####
write.dta(Wave4, file="PhilippinesWave4_2021version.dta")

#+++++++++Wave5+++++++####

Wave5 <-read_sav("Philippines_Wave5.sav")
View(Wave5)

#trust variables####
# to be reverse code, and standardized into common scale = from 6 to 4
Wave5$TrustCourts <-car::recode(Wave5$Q8, "1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustCourts[Wave5$TrustCourts>6]<-NA 

Wave5$TrustNatGov <-car::recode(Wave5$Q9, "1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustNatGov[Wave5$TrustNatGov>6]<-NA

Wave5$TrustParties<-car::recode(Wave5$Q10,"1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustParties[Wave5$TrustParties>6]<-NA

Wave5$TrustCongress <-car::recode(Wave5$Q11,"1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustCongress[Wave5$TrustCongress>6]<-NA

Wave5$TrustCivService <-car::recode(Wave5$Q12,"1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustCivService[Wave5$TrustCivService>6]<-NA

Wave5$TrustMilitary <-car::recode(Wave5$Q13,"1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustMilitary[Wave5$TrustMilitary>6]<-NA

Wave5$TrustPolice <-car::recode(Wave5$Q14,"1=6;2=5;3=4;4=3;5=2;6=1")
Wave5$TrustPolice[Wave5$TrustPolice>6]<-NA


#Institutional variables####
Wave5$CurbCorruption <-car::recode(Wave5$Q126,"1=4;2=3;3=2;4=1") #4 very effective
Wave5$CurbCorruption[Wave5$CurbCorruption>4]<-NA

Wave5$NoLocalCorruption <-car::recode(Wave5$Q124,"1=4;2=3;3=2;4=1") #high values, no local corruption/ hardly anyone is involved (4)
Wave5$NoLocalCorruption[Wave5$NoLocalCorruption >4]<-NA
table(Wave5$NoLocalCorruption )

Wave5$NoNatCorruption <-car::recode(Wave5$Q125,"1=4;2=3;3=2;4=1") #high values, no national corruption/ hardly anyone is involved (4)
Wave5$NoNatCorruption[Wave5$NoNatCorruption >4]<-NA
table(Wave5$NoNatCorruption)

#sociotropic econ
Wave5$PresentEconEval <-car::recode(Wave5$Q1,"1=5;2=4;3=3;4=2;5=1")#very good (5)
Wave5$PresentEconEval[Wave5$PresentEconEval>5]<-NA

Wave5$RetroEconEval<-car::recode(Wave5$Q2,"1=5;2=4;3=3;4=2;5=1") #much better (5)
Wave5$RetroEconEval[Wave5$RetroEconEval>5]<-NA

Wave5$ProspectEconEval<-car::recode(Wave5$Q3,"1=5;2=4;3=3;4=2;5=1") 
Wave5$ProspectEconEval[Wave5$ProspectEconEval>5]<-NA

#pocketbook econ
Wave5$PresentFamEcon <-car::recode(Wave5$Q4,"1=5;2=4;3=3;4=2;5=1") 
Wave5$PresentFamEcon[Wave5$PresentFamEcon >5]<-NA

Wave5$RetroFamEcon <-car::recode(Wave5$Q5,"1=5;2=4;3=3;4=2;5=1") 
Wave5$RetroFamEcon[Wave5$RetroFamEcon>5]<-NA

Wave5$ProspectiveFamEcon <-car::recode(Wave5$Q6,"1=5;2=4;3=3;4=2;5=1") 
Wave5$ProspectiveFamEcon[Wave5$ProspectiveFamEcon>5]<-NA

#Democratic values/political orientation####
#strongly disagree (4 as high scores)
Wave5$DemV1 <-Wave5$Q149 #Government leaders are like the head of a family; we should all follow their decisions.
Wave5$DemV1[Wave5$DemV1>4]<-NA

Wave5$DemV2 <-Wave5$Q150 #The government should decide whether certain ideas should be allowed to be discussed in society.
Wave5$DemV2[Wave5$DemV2>4]<-NA

Wave5$DemV3 <-Wave5$Q151 #Harmony of the community will be disrupted if people organize lots of groups.
Wave5$DemV3[Wave5$DemV3>4]<-NA

Wave5$DemV4 <-Wave5$Q152 #When judges decide important cases, they should accept the view of the executive branch.
Wave5$DemV4[Wave5$DemV4>4]<-NA

Wave5$DemV5 <-Wave5$Q153 #If the government is constantly checked [i.e. monitored and supervised] by the legislature,it cannot possibly accomplish great things.
Wave5$DemV5[Wave5$DemV5>4]<-NA

Wave5$DemV6 <-Wave5$Q154 #If we have political leaders who are morally upright, we can let them decide everything.
Wave5$DemV6[Wave5$DemV6>4]<-NA

Wave5$DemV7 <-Wave5$Q155 #If people have too many different ways of thinking, society will be chaotic.
Wave5$DemV7[Wave5$DemV7>4]<-NA


#Traditional cultural values
Wave5$FollowParents <-car::recode(Wave5$Q62,"1=4;2=3;3=2;4=1") #parents
Wave5$FollowParents[Wave5$FollowParents>4]<-NA

Wave5$ConflictAvoidance<-car::recode(Wave5$Q66,"1=4;2=3;3=2;4=1") #neighbor
Wave5$ConflictAvoidance[Wave5$ConflictAvoidance>4]<-NA

Wave5$Passive<- car::recode(Wave5$Q67,"1=4;2=3;3=2;4=1")
Wave5$Passive[Wave5$Passive>4]<-NA

Wave5$FamilyFirst<-car::recode(Wave5$Q58,"1=4;2=3;3=2;4=1")
Wave5$FamilyFirst[Wave5$FamilyFirst>4]<-NA

#authoritarian-cultural values####
Wave5$StrongLeaders <-car::recode(Wave5$Q137,"1=4;2=3;3=2;4=1") #strong agree (4)
Wave5$StrongLeaders[Wave5$StrongLeaders>4]<-NA

Wave5$ConformitytoLeaders <-car::recode(Wave5$Q149,"1=4;2=3;3=2;4=1")
Wave5$ConformitytoLeaders[Wave5$ConformitytoLeaders>4]<-NA

Wave5$Submission<-car::recode(Wave5$Q150,"1=4;2=3;3=2;4=1")
Wave5$Submission[Wave5$Submission>4]<-NA

Wave5$AntiPolPluralism1<-car::recode(Wave5$Q151,"1=4;2=3;3=2;4=1")
Wave5$AntiPolPluralism1[Wave5$AntiPolPluralism1>4]<-NA

Wave5$AntiPolPluralism2<-car::recode(Wave5$Q155,"1=4;2=3;3=2;4=1")
Wave5$AntiPolPluralism2[Wave5$AntiPolPluralism2>4]<-NA

#controls####

#political interest
Wave5$PolInterest <-car::recode(Wave5$Q46,"1=4;2=3;3=2;4=1")
Wave5$PolInterest[Wave5$PolInterest>4]<-NA

#social trust
Wave5$SocialTrust <-car::recode(Wave5$Q22,"1=2;2=1")# most people can be trusted(2)
table(Wave5$SocialTrust)
Wave5$SocialTrust[Wave5$SocialTrust>2]<-NA
table(Wave5$SocialTrust)

#Democratic Satisfaction
Wave5$DemSatisfaction<-car::recode(Wave5$Q99,"1=4;2=3;3=2;4=1")#very satisfied (4)
Wave5$DemSatisfaction[Wave5$DemSatisfaction>4]<-NA

#Current regime/Government satisfaction
Wave5$RegimeSatisfaction <-car::recode(Wave5$Q105,"1=4;2=3;3=2;4=1")
Wave5$RegimeSatisfaction[Wave5$RegimeSatisfaction>4]<-NA

#demographic controls####
Wave5$SurveyYear <-2018

#Age
Wave5$Age<-Wave5$Se3_1

#Gender
Wave5$Gender <-Wave5$Se2 # 1 male; 2 female

#income group
Wave5$IncomeGroup <-Wave5$Se14 #high income (5); 40K+ and above
Wave5$IncomeGroup[Wave5$IncomeGroup>5]<-NA

#urbanization
Wave5$Urbanization <-Wave5$Level

#education
Wave5$Education <-Wave5$Se5
table(Wave5$Education)

#religiosity
Wave5$Religiosity<-car::recode(Wave5$Se7,"1=10;2=9;3=8;4=7;5=6=6=5;7=4;8=3;9=2;10=1")
Wave5$Religiosity[Wave5$Religiosity>10]<-NA


#index of political trust####
Wave5$PolTrust<-rowMeans(cbind(Wave5$TrustCourts,Wave5$TrustNatGov,Wave5$TrustCongress,
                               Wave5$TrustMilitary,Wave5$TrustParties,Wave5$TrustCivService,
                               Wave5$TrustPolice),na.rm=TRUE)
summary (Wave5$PolTrust)

#institutional performance####
Wave5$Inst.performance <-rowMeans(cbind(Wave5$PresentEconEval,Wave5$RetroEconEval, Wave5$ProspectEconEval,
                                        Wave5$PresentFamEcon,Wave5$RetroFamEcon,Wave5$ProspectiveFamEcon,
                                        Wave5$CurbCorruption,Wave5$NoNatCorruption,Wave5$NoLocalCorruption),na.rm=TRUE)
summary(Wave5$Inst.performance)

#Traditional values####
Wave5$Trad.Values <-rowMeans(cbind(Wave5$FollowParents,Wave5$ConflictAvoidance,Wave5$Passive,Wave5$FamilyFirst),na.rm=TRUE)
summary(Wave5$Trad.Values)

#Democratic values####
Wave5$Demo.Values <-rowMeans(cbind(Wave5$DemV1, Wave5$DemV2, Wave5$DemV3,
                                   Wave5$DemV4, Wave5$DemV5, Wave5$DemV6,
                                   Wave5$DemV7),na.rm=TRUE)
summary(Wave5$Demo.Values)

#authoritarian-cultural values####
Wave5$AuthoritarianValues <-rowMeans(cbind(Wave5$StrongLeaders,Wave5$ConformitytoLeaders,
                                           Wave5$Submission, Wave5$AntiPolPluralism1,
                                           Wave5$AntiPolPluralism2),na.rm=TRUE)
summary(Wave5$AuthoritarianValues)


#regressions####
summary(Mod1 <-lm(PolTrust~Inst.performance + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave5 ))

summary(Mod2 <-lm(PolTrust~Inst.performance*Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave5 ))

summary(Mod3 <-lm(PolTrust~Inst.performance*DemSatisfaction + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave5 ))

summary(Mod4 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = Wave5 ))

summary(Mod5 <-lm(PolTrust~Inst.performance + Trad.Values*Demo.Values*AuthoritarianValues+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave5 ))

summary(Mod6 <-lm(PolTrust~Inst.performance*Demo.Values*AuthoritarianValues+Trad.Values+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = Wave5 ))

summary(Mod7 <-lm(PolTrust~Inst.performance*DemSatisfaction*RegimeSatisfaction + Trad.Values+ 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave5 ))

summary(Mod8 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction*RegimeSatisfaction + 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = Wave5 ))

library(stargazer)
stargazer(Mod1, Mod2, Mod3, Mod4, type="text")
stargazer(Mod5, Mod6, Mod7, Mod8, type="text")
stargazer(Mod1, Mod2, Mod3, Mod4, type="html", title="Philippines_Wave5_Table1", out = "Philippines_Wave5_Table1.htm")
stargazer(Mod5, Mod6, Mod7, Mod8, type="html", title="Philippines_Wave5_Table2", out = "Philippines_Wave5_Table2.htm")

#save a copy####
write.dta(Wave5, file="PhilippinesWave5_2021version.dta")

#merge dataset####
#subset the datasets

colnames(Wave1)
dat1<-Wave1[,291:338]

colnames(Wave2)
dat2<-Wave2[,267:314]

colnames(Wave3)
dat3<-Wave3[,260:307]

colnames(Wave4)
dat4<-Wave4[,289:336]

colnames(Wave5)
dat5<-Wave5[,304:351]

library(dplyr)

merge1 <-full_join(dat1, dat2, index="SurveyYear") 
merge2 <-full_join(dat3, dat4, index="SurveyYear")
merge3 <-full_join(dat5, merge1, index="SurveyYear")
merge.final<-full_join(merge2, merge3, index="SurveyYear")

#pooled regressions####
summary(Pooled.Mod1 <-lm(PolTrust~Inst.performance + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = merge.final ))

summary(Pooled.Mod2 <-lm(PolTrust~Inst.performance*Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest + DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = merge.final ))

summary(Pooled.Mod3 <-lm(PolTrust~Inst.performance*DemSatisfaction + Trad.Values + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = merge.final ))

summary(Pooled.Mod4 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction + Demo.Values + AuthoritarianValues+
                    SocialTrust + PolInterest +
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education ,data = merge.final ))

summary(Pooled.Mod5 <-lm(PolTrust~Inst.performance + Trad.Values*Demo.Values*AuthoritarianValues+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = merge.final ))

summary(Pooled.Mod6 <-lm(PolTrust~Inst.performance*Demo.Values*AuthoritarianValues+Trad.Values+
                    SocialTrust + PolInterest +DemSatisfaction+
                    RegimeSatisfaction+ Age + Gender +IncomeGroup+
                    Religiosity + Urbanization +  Education,data = merge.final ))

summary(Pooled.Mod7 <-lm(PolTrust~Inst.performance*DemSatisfaction*RegimeSatisfaction + Trad.Values+ 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = merge.final ))

summary(Pooled.Mod8 <-lm(PolTrust~Inst.performance + Trad.Values*DemSatisfaction*RegimeSatisfaction + 
                    Demo.Values+AuthoritarianValues+
                    SocialTrust + PolInterest +  Age + Gender +IncomeGroup +
                    Religiosity + Urbanization +  Education , data = merge.final ))


library(stargazer)
stargazer(Pooled.Mod1, Pooled.Mod2, Pooled.Mod3, Pooled.Mod4, type="text")
stargazer(Pooled.Mod5, Pooled.Mod6, Pooled.Mod7, Pooled.Mod8, type="text")
stargazer(Pooled.Mod1, Pooled.Mod2, Pooled.Mod3, Pooled.Mod4, type="html", title="PooledTable1", out = "Pooled_Table1.htm")
stargazer(Pooled.Mod5, Pooled.Mod6, Pooled.Mod7, Pooled.Mod8, type="html", title="Pooledable2", out = "Pooled_Table2.htm")

#save a copy####
write.dta(merge.final, file="merge.final_PhilippinesWave5_2021version.dta")

