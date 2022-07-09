#Plotting Time series

rm(list=ls())


#set wd
setwd("C:/Users/User/Desktop/R projects/World Values Survey_Project/F00008389-WVS_Longitudinal_1981_2016_stata_v20180912")

library(haven)
WVS.TS <-read_dta("WVS_Longitudinal_1981_2016_stata_v20180912.dta")

names(WVS.TS)


WVS.TS$E114
library(tidyverse)

#Create variables####

WVS.TS %>% 
  select (E069_02, E069_06, E069_07, E069_08, E069_11,
          E069_12, E069_17, E114) %>% glimpse() %>%
  mutate(strong_leader = recode(E114, 
                                4 = 1,
                                3 = 2,
                                2 = 3,
                                1 = 4))
  

WVS.TS$E114

#create new variable - base R
WVS.TS$strongleaders <-WVS.TS$E114
WVS.TS$strongleaders
table(WVS.TS$strongleaders)

WVS.TS$Year <-WVS.TS$S020
table(WVS.TS$Year)

#recode
library(car)
WVS.TS$strongleadersR <-car::recode(WVS.TS$strongleaders,"1=4;2=3;3=2;4=1") 
table(WVS.TS$strongleadersR)
#Wave1$StrongLeaders[Wave1$StrongLeaders>4]<-NA
WVS.TS$strongleadersR[WVS.TS$strongleadersR<1] <-NA
table(WVS.TS$strongleadersR)
library(ggplot2)

class(WVS.TS$Year)
WVS.TS$Year2 <-as.numeric(WVS.TS$Year)

ggplot(WVS.TS, aes(x = S025, y = strongleadersR))+
         geom_point()

class(WVS.TS$S025)
WVS.TS$S025 <-as.factor(WVS.TS$S025)


# WVS - 7 ####
library(haven)
WVS_7 <- read_dta("C:/Users/User/Desktop/R projects/Dataset Sources etc/WVS7/WVS_7.dta")

names(WVS_7)

WVS_7$btigovindex[WVS_7$btigovindex == -9999] <-NA
WVS_7$btigoveperform[WVS_7$btigoveperform == -9999] <-NA
WVS_7$btiruleoflaw[WVS_7$btiruleoflaw == -9999] <-NA


library(tidyverse)
WVS_7  %>%
  ggplot(aes(x = btiruleoflaw, y =  btigovindex))+
  geom_point()+geom_text(aes(label=C_COW_ALPHA), 
    nudge_C_COW_ALPHA=-0.1, hjust=0, vjust=0)

+
  geom_text(aes(label = y), nudge_y = -0.1)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_text(aes(label = y), nudge_y = -0.1)

