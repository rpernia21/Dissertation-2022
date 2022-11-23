# Dissertation - August 1 2022 version ####

rm(list = ls())

#packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(dplyr)
library(haven)
library(lme4)
library(foreign)
library(psych)
library(interplot)
library(stargazer)
library(lme4)
library(MuMIn)

#set up directory

setwd("C:/Users/User/Desktop/R projects/World Values Survey_Project/plots")

#load data
WVS_merge1 <-read_dta("mergesurveysvdemgtd.dta")
#WVS_merge2 <-read_dta("mergesurveyandmacro.dta")

names(WVS_merge1)
#names(WVS_merge2)

#subset

autocracies_WVS <- subset(WVS_merge1, row==0)
democracies_WVS <-subset(WVS_merge1, row==1)
names(WVS_merge1)


#regression

library(lme4)
library(MuMIn)

names(WVS_merge1)

summary(m1 <-lmer(poltrust_no_prot~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m2 <-lmer(trust_army~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m3 <-lmer(trust_police~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m4 <-lmer(trust_courts~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 


summary(m5 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m6 <-lmer(trust_pol_parties~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m7 <-lmer(trust_congress~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends +v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m8 <-lmer(trust_civ_serv~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 


summary(m9 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends  + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m10 <-lmer(poltrust_no_prot~strong_leader + unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends  +v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 


summary(m11 <-lmer(poltrust_no_prot~strong_leader*unsafe_cr_hm +worry_terrorsm + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends  + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 

summary(m12 <-lmer(poltrust_no_prot~strong_leader*worry_terrorsm + unsafe_cr_hm + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends  + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 


summary(m13 <-lmer(poltrust_no_prot~strong_leader*v2exl_legitlead + worry_terrorsm + unsafe_cr_hm + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends  + v2x_libdem +  v2exl_legitlead + v2exl_legitperf + v2exl_legitratio + e_wbgi_pve + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 

#export results 

library(stargazer)
stargazer(m1, m2,m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, type='html',
          out='merge.htm')

browseURL(file.path('merge.htm'))



#only dictatorship
summary(m1 <-lmer(trust_govt~  worry_terrorsm*strong_leader + unsafe_cr_hm+
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + 
                    (1 | v2x_libdem ) + (1 | gtdscore ),
                  data = democracies_WVS))

library(interplot)
interplot(m = m1, var1 = 'worry_terrorsm', var2 = 'strong_leader', point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() 

#subset

autocracies_WVS <- subset(WVS_merge1, row==0)
democracies_WVS <-subset(WVS_merge1, row==1)
names(WVS_merge1)


#Oct 10 2022 version####


summary(m1 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

#plotting coefficients estimates####
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(glmmTMB)

plot_model(m1,  colors = "Black",
           value.offset = .4,
           value.size = 4,
           dot.size = 3,
           line.size = 1.5,
           vline.color = "blue",
           width = 1.5)

plot_model(m1)
plot_model(m2)

library("arm")

coefplot(m1)
coefplot(m2, add=TRUE, col.pts="red",  intercept=TRUE)
coefplot(M3, add=TRUE, col.pts="blue", intercept=TRUE, offset=0.2)



summary(m2 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

plot_model(m2)

#https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

summary(m3 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm  + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = autocracies_WVS)) 

summary(m4 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm +
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = democracies_WVS)) 

summary(m5 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm +
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = democracies_WVS)) 

summary(m6 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = autocracies_WVS)) 


library(stargazer)
stargazer(m1, m2, m3, m4, m5, m6, type='html',
          out='merge_Oct7.htm')

browseURL(file.path('merge_Oct7.htm'))

#interaction effect

summary(m7 <-lmer(trust_govt~ unsafe_cr_hm*worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m8 <-lmer(strong_leader~ unsafe_cr_hm*worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

library(stargazer)
stargazer(m1, m2, m7, m8, type='html',
          out='merge_Oct7.htm')

browseURL(file.path('merge_Oct7.htm'))


#conditional effect
summary(m3 <-lmer(trust_govt~ unsafe_cr_hm*strong_leader +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m4 <-lmer(strong_leader~ unsafe_cr_hm*trust_govt +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +sgi_go+
                    (1 | country/ country_year),
                  data = WVS_merge1)) 



library(stargazer)
stargazer(m1, m2, m3, m4, type='html',
          out='merge_Oct7.htm')

browseURL(file.path('merge_Oct7.htm'))


summary(m1 <-lmer(trust_govt~ strong_leader + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 
summary(m2 <-lmer(strong_leader~ trust_govt + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m3 <-lmer(trust_govt~ strong_leader + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = autocracies_WVS)) 

summary(m4 <-lmer(trust_govt~ strong_leader + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = democracies_WVS)) 

summary(m5 <-lmer(strong_leader~ trust_govt + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = democracies_WVS)) 

summary(m6 <-lmer(strong_leader~ trust_govt + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead  + e_wbgi_pve + gtdscore +
                    (1 | country/ country_year),
                  data = autocracies_WVS)) 


library(stargazer)
stargazer(m1, m2, m3, m4, m5, m6, type='html',
          out='merge_Oct7.htm')

browseURL(file.path('merge_Oct7.htm'))


#Oct 21
strong_leader~ trust_govt

summary(m1 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 




summary(m2 <-lmer(trust_govt~  worry_terrorsm + unsafe_cr_hm*strong_leader+
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + 
                    (1 | v2exl_legitlead ) + (1+ v2x_libdem)+ (1 | gtdscore ),
                  data = autocracies_WVS))

library(interplot)
interplot(m = m2, var1 = 'unsafe_cr_hm', var2 = 'strong_leader', point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() 

summary(m1 <-lmer(trust_govt~  worry_terrorsm + unsafe_cr_hm*strong_leader+
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + 
                    (1 | v2exl_legitlead ) + (1+ v2x_libdem)+ (1 | gtdscore ),
                  data = WVS_merge1))

summary(m2 <-lmer(strong_leader~  worry_terrorsm + unsafe_cr_hm*trust_govt+
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + 
                    (1 | v2exl_legitlead ) + (1+ v2x_libdem)+ (1 | gtdscore ),
                  data = WVS_merge1))

summary(m3 <-lmer(trust_govt~  worry_terrorsm + unsafe_cr_hm*strong_leader+
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + 
                    (1 | v2exl_legitlead ) + (1+ v2x_libdem)+ (1 | gtdscore ),
                  data = democracies_WVS))

summary(m4 <-lmer(strong_leader~  worry_terrorsm + unsafe_cr_hm*trust_govt+
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + 
                    (1 | v2exl_legitlead ) + (1+ v2x_libdem)+ (1 | gtdscore ),
                  data = democracies_WVS))

library(interplot)
interplot(m = m1, var1 = 'unsafe_cr_hm', var2 = 'strong_leader', point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() 

library(interplot)
interplot(m = m2, var1 = 'unsafe_cr_hm', var2 = 'trust_govt', point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() 

library(interplot)
interplot(m = m3, var1 = 'unsafe_cr_hm', var2 = 'strong_leader', point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw() 

library(interplot)
interplot(m = m4, var1 = 'unsafe_cr_hm', var2 = 'trust_govt', point = T) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw()


library(stargazer)
stargazer(m1, m2, type='html',
          out='merge_Oct21.htm')

browseURL(file.path('merge_Oct21.htm'))

#oct 24####

# full data
summary(m1 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore_log +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m2 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore_log +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 
library(stargazer)
stargazer(m1, m2, type = 'html', out = 'Nov1.htm')
browseURL(file.path('Nov1.htm'))


# cross-level interactions
summary(m1a <-lmer(trust_govt~ unsafe_cr_hm*worry_terrorsm*v2x_libdem + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends  +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

summary(m2a <-lmer(strong_leader~ unsafe_cr_hm*worry_terrorsm*v2x_libdem + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends  +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),
                  data = WVS_merge1)) 

library(stargazer)
stargazer(m1a, m2a, type = 'html', out = 'Nov1.htm')
browseURL(file.path('Nov1.htm'))

'''
citizens worried about their personal safety are more likley to support a strong leader
under conditions of greater liberal democracy

citizens worried about their national safety are both equally likely to support a strong leader
'''


#only democracies
summary(m3 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore+
                    (1 | country/ country_year),
                  data = democracies_WVS)) 

summary(m4 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore+
                    (1 | country/ country_year),
                  data = democracies_WVS)) 

#only autocracies

summary(m5 <-lmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),
                  data = autocracies_WVS)) 

summary(m6 <-lmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),
                  data = autocracies_WVS)) 

#robustness - probit regression#

#convert to factors
class(WVS_merge1$strong_leader)
WVS_merge1$strong_leader <-as.factor(WVS_merge1$strong_leader)

WVS_merge1$trust_govt <-as.factor(WVS_merge1$trust_govt)

summary(m1 <-glmer(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),family=binomial(link="probit"),
                  data = WVS_merge1)) 

summary(m2 <-glmer(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                    (1 | country/ country_year),family=binomial(link="probit"),
                  data = WVS_merge1)) 

library(stargazer)

stargazer(m1,m2, type='text')
stargazer(m1,m2, type='html',
          out='robustness.htm')

browseURL(file.path('robustness.htm'))

stargazer(m3, m4, m5, m6, type='html',
          out='merge-per regime-Oct24.htm')

browseURL(file.path('merge-per regime-Oct24.htm'))


stargazer(m1, m2, m3, m4, m5, m6, type='html',
          out='merge-per regime-Oct24.htm')

browseURL(file.path('merge-per regime-Oct24.htm'))

#multilevel logistic modeling

summary(m9 <-glmer(trust_govtRE~ unsafe_cr_hm +worry_terrorsm + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),family=binomial(link="logit"),
                   data = WVS_merge1)) 

summary(m10 <-glmer(strong_leaderRE~ unsafe_cr_hm +worry_terrorsm + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),family=binomial(link="logit"),
                   data = WVS_merge1)) 

library(stargazer)

stargazer(m9,m10, type='html',
          out='robustness-logit.htm')

browseURL(file.path('robustness-logit.htm'))

# different independent variables
summary(m11 <-lmer(trust_govt~ insecure_nghbrs +worry_abroad + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 
summary(m12 <-lmer(strong_leader~ insecure_nghbrs +worry_abroad  + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 



# differnet main variables
summary(m13 <-lmer(poltrust_no_prot~ unsafe_cr_hm +worry_terrorsm + 
                      satis_household_inc + interest_politics + social_trust 
                    + dem_perception+ age + female + social_class+ education + pray + 
                      info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                      info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                      (1 | country/ country_year),
                    data = WVS_merge1)) 

summary(m14 <-lmer(army_rule~ unsafe_cr_hm +worry_terrorsm + 
                          satis_household_inc + interest_politics + social_trust 
                        + dem_perception+ age + female + social_class+ education + pray + 
                          info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                          info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                          (1 | country/ country_year),
                        data = WVS_merge1)) 

library(stargazer)

stargazer(m11,m12, type='html',
          out='robustness-differentIV.htm')
browseURL(file.path('robustness-differentIV.htm'))

library(stargazer)

stargazer(m13,m14, type='html',
          out='robustness-differentDV.htm')
browseURL(file.path('robustness-differentDV.htm'))


# nov 12 specs####

# different independent variables
summary(m17 <-lmer(trust_govt~ unsafe_cr_hm +worry_abroad + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 
summary(m18 <-lmer(strong_leader~ unsafe_cr_hm +worry_abroad  + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 

library(stargazer)
stargazer(m17,m18, type='html',
          out='newspecs_nov12.htm')
browseURL(file.path('newspecs_nov12.htm'))


summary(m15 <-lmer(poltrust_no_prot~ insecure_nghbrs +worry_abroad  + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 

summary(m16 <-lmer(army_rule~ insecure_nghbrs +worry_abroad + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends + v2x_libdem +  v2exl_legitlead + gtdscore +
                     (1 | country/ country_year),
                   data = WVS_merge1)) 

library(stargazer)
stargazer(m15,m16, type='html',
          out='robustness-differentDV2.htm')
browseURL(file.path('robustness-differentDV2.htm'))

#SUbset Chapter 4####

Philippines_WVS <- subset(WVS_merge1, country_name=='Philippines')
Taiwan_WVS <-subset(WVS_merge1, country_name=='Taiwan')

#save a copy####

library(haven)
write.dta(WVS_merge1, file="Taiwan_WVS.dta")
write.dta(WVS_merge1, file="Philippines_WVS.dta")

write.dta(Wave1, file="TaiwanWave1_2021version.dta")

#philippines
summary(m20 <-lm(trust_govt~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends,
                  data = Philippines_WVS)) 

summary(m20a <-lm(trust_govt~ unsafe_cr_hm*worry_terrorsm + 
                   satis_household_inc + interest_politics + social_trust 
                 + dem_perception+ age + female + social_class+ education + pray + 
                   info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                   info_talk_friends,
                 data = Philippines_WVS)) 

summary(m21 <-lm(strong_leader~ unsafe_cr_hm +worry_terrorsm + 
                    satis_household_inc + interest_politics + social_trust 
                  + dem_perception+ age + female + social_class+ education + pray + 
                    info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                    info_talk_friends ,
                  data = Philippines_WVS)) 

summary(m21a <-lm(strong_leader~ unsafe_cr_hm*worry_terrorsm + 
                   satis_household_inc + interest_politics + social_trust 
                 + dem_perception+ age + female + social_class+ education + pray + 
                   info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                   info_talk_friends ,
                 data = Philippines_WVS)) 

summary(m22 <-lm(trust_govt~ unsafe_cr_hm +worry_abroad + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends,
                   data = Taiwan_WVS))

summary(m22a <-lm(trust_govt~ unsafe_cr_hm*worry_terrorsm + 
                   satis_household_inc + interest_politics + social_trust 
                 + dem_perception+ age + female + social_class+ education + pray + 
                   info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                   info_talk_friends,
                 data = Taiwan_WVS)) 

summary(m23 <-lm(strong_leader~ unsafe_cr_hm +worry_abroad + 
                     satis_household_inc + interest_politics + social_trust 
                   + dem_perception+ age + female + social_class+ education + pray + 
                     info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                     info_talk_friends ,
                   data = Taiwan_WVS)) 

summary(m23a <-lm(strong_leader~ unsafe_cr_hm*worry_terrorsm + 
                   satis_household_inc + interest_politics + social_trust 
                 + dem_perception+ age + female + social_class+ education + pray + 
                   info_newspaper + info_TV + info_radio + info_phone + info_email + info_internet +
                   info_talk_friends ,
                 data = Taiwan_WVS)) 

library(stargazer)
stargazer(m20,m21,m22,m23, type='html',
          out='chapter4.htm')

browseURL(file.path('chapter4.htm'))

#only phils
library(stargazer)

stargazer(m20,m20a,m21,m21a, type='html',
          out='chapter4-phils.htm')

browseURL(file.path('chapter4-phils.htm'))

#taiwan
stargazer(m22,m22a,m23,m23a, type='html',
          out='chapter4-taiwan.htm')

browseURL(file.path('chapter4-taiwan.htm'))
