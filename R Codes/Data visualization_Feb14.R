rm(list=ls())

library(haven)

#WVS <- read_dta("WVS_Cross-National_Wave_7_stata_Pernia_030222.dta")
#colnames(WVS)


##visualization####
library(ggplot2)
#attach(WVS)
library(dplyr)
library(tidyverse)


#distal threat and political trust per country

dat1 <-WVS %>% 
  ggplot(aes(x = worry_terror_attack, y = poltrust_no_prot))+
  geom_bar(stat='identity')+
  geom_vline(xintercept = 3, linetype="solid", 
             color = "blue")+
  facet_wrap(~B_COUNTRY_ALPHA) + 
  labs (x = 'Worried for Terrorist Attack',
        y = 'Political Trust',
        title = 'Distal Threat and Political Trust, 2017-2020',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw()

dat1

#save
ggsave("dat1_Distal threat_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)

dev.off()


#proximal

dat2 <-WVS %>% 
  ggplot(aes(x = insecure, y = poltrust_no_prot))+
  geom_bar(stat='identity')+
  facet_wrap(~B_COUNTRY_ALPHA) + 
  labs (x = 'Secure in Neighborhood',
        y = 'Political Trust',
        title = 'Proximal Threat and Political Trust, 2017-2020',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw()

dat2

#save
ggsave("dat2_Proximal threat_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)

dev.off()


#only pol trust

dat3 <-WVS %>% 
  ggplot(aes(x = poltrust_no_prot))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="solid", 
             color = "blue")+
  facet_wrap(~B_COUNTRY_ALPHA) + 
  labs (x = 'Political Trust',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw() 

dat3

ggsave("dat3_Political Trust_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)


dev.off()

#strong leaders


dat4 <-WVS %>% 
  ggplot(aes(x = strong_leader))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="solid", 
             color = "blue")+
  facet_wrap(~country_year) + 
  labs (x = 'Support for Strong Leadership',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw()

dat4

ggsave("dat4_Strong Leaders_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)



#insecure 

dat5 <-WVS %>% 
  ggplot(aes(x = insecure))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="solid", 
             color = "blue")+
  facet_wrap(~B_COUNTRY_ALPHA) + 
  labs (x = 'Insecure in Neighborhood',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw()

  dat5
  
ggsave("dat5_Insecure_Feb14.png",
         width=11.7, height=8.3, units="in", dpi=100)
  
#terror attack


dat6 <-WVS %>% 
  ggplot(aes(x = worry_terror_attack))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="solid", 
             color = "blue")+
  facet_wrap(~B_COUNTRY_ALPHA) + 
  labs (x = 'Worried for Terrorist Attack',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw()

dat6

ggsave("dat6_Terror_attack_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)


# political trust by regions

#recode
WVS$region_new = NA

WVS[which(WVS$regionWB == '1'), 'region_new'] = 'Sub-Saharan Africa'
WVS[which(WVS$regionWB == '2'), 'region_new'] = 'South Asia'
WVS[which(WVS$regionWB == '3'), 'region_new'] = 'North America'
WVS[which(WVS$regionWB == '4'), 'region_new'] = 'Middle East and North Africa'
WVS[which(WVS$regionWB == '5'), 'region_new'] = 'Latin America and Caribbean'
WVS[which(WVS$regionWB == '6'), 'region_new'] = 'Europe and Central Asia'
WVS[which(WVS$regionWB == '7'), 'region_new'] = 'East Asia and Pacific'


dat7 <-WVS  %>%
  ggplot(aes(x = poltrust_no_prot))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="longdash", 
             color = "blue", size=1)+ 
  labs (x = 'Political Trust by regions',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') + 
  theme_bw() + facet_wrap(~region_new)
  
dat7

#ggsave

ggsave("dat7_Pol trust by regions_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)


#pol trust per regimes


# political trust by regimes (Freedom House)

#recode

#data.complete <-WVS[complete.cases(WVS),]
WVS$polregfh <-as.factor(WVS$polregfh)
na.omit(WVS$regime_new)

WVS$regime_new = NA

WVS[which(WVS$polregfh == '1'), 'regime_new'] = 'Not free'
WVS[which(WVS$polregfh == '2'), 'regime_new'] = 'Partly free'
WVS[which(WVS$polregfh == '3'), 'regime_new'] = 'Free'


dat8 <-WVS  %>% 
  ggplot(aes(x = poltrust_no_prot))+
  geom_bar(position='stack')+
  geom_vline(xintercept = 3, linetype="longdash", 
             color = "blue", size=1, na.rm = TRUE)+ 
  labs (x = 'Political Trust by regimes (Freedom House)',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') + 
  theme_bw() + facet_wrap(~regime_new)

dat8

#ggsave

ggsave("dat8_Pol trust by regime_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)


# regime types according to 
WVS$regtype <-as.factor(WVS$regtype)
WVS$regime_type2 = NA
na.omit(WVS$regime_type2)

WVS[which(WVS$regtype == '1'), 'regime_type2'] = 'Autocracy'
WVS[which(WVS$regtype == '2'), 'regime_type2'] = 'Closed anocracy'
WVS[which(WVS$regtype == '3'), 'regime_type2'] = 'Open anocracy'
WVS[which(WVS$regtype == '4'), 'regime_type2'] = 'Democracy'
WVS[which(WVS$regtype == '5'), 'regime_type2'] = 'Full Democracy'


dat9 <-WVS  %>% 
  ggplot(aes(x = poltrust_no_prot))+
  geom_bar(position = 'stack', na.rm=TRUE)+
  geom_vline(xintercept = 3, linetype="longdash", 
             color = "blue", size=1, na.rm = TRUE)+ 
  labs (x = 'Political Trust by regimes (Polity Score)',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') + 
  theme_bw() + facet_wrap(~regime_type2)

dat9

#ggsave

ggsave("dat9_Pol trust by polity score_Feb14.png",
       width=11.7, height=8.3, units="in", dpi=100)

# only threats

dat8 <-WVS %>% 
  ggplot(aes(x = insecure))+
           geom_bar(position = 'stack')+
           geom_vline(xintercept = 3, linetype="longdash", 
                      color = "blue", size=1)+ 
           labs (x = 'Insecure in Neighborhood',
                 caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
           theme_bw()
dat8
         

dat9 <-WVS %>% 
  ggplot(aes(x = worry_terror_attack))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="longdash", 
             color = "blue", size=1)+ 
  labs (x = 'Worried for Terrorist Attack',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw()
dat9

#only strong leadership

dat10 <-WVS %>% 
  ggplot(aes(x = strong_leader))+
  geom_bar(position = 'stack')+
  geom_vline(xintercept = 3, linetype="longdash", 
             color = "blue", size=1)+ 
  labs (x = 'Support for Strong Leadership',
        caption = 'Source: Wave 7 World Values Survey (WVS 7)') +
  theme_bw() + facet_wrap(~regionWB)
dat10
