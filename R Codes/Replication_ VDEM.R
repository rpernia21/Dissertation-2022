# Replication VDEM

rm(list=ls)

setwd("C:/Users/User/Desktop/R projects/Dataset Sources etc/V-Dem")

#packages
library(ggplot2)
library(dplyr)
library(tidyverse)
library(dplyr)
library(haven)
library(haven)
library(foreign)
library(car)
library(stargazer)
library(Hmisc)
library(psych)
library(ggplot2)
library(dplyr)

#load data
VDEM <-read_dta("V-Dem-CY-Full+Others-v11.1.dta")

names(VDEM)

VDEM$v2x_libdem

library(ggplot2)

class(VDEM$year)
#subset only 1900 year
VDEM2 <-subset(VDEM, year >= '1900')

na.omit(VDEM2$e_regionpol)
na.omit(VDEM2$v2x_libdem)

class(VDEM2$e_regionpol)

VDEM2$e_regionpol_6C <-as.factor(VDEM2$e_regionpol_6C)

VDEM2$e_regionpol <-as.character(VDEM2$e_regionpol)

VDEM2$e_regionpol[VDEM2$e_regionpol == '1'] <-'E. Europe and C. Asia'
VDEM2$e_regionpol[VDEM2$e_regionpol == '2'] <-'Latin America'
VDEM2$e_regionpol[VDEM2$e_regionpol == '3'] <-'MENA'
VDEM2$e_regionpol[VDEM2$e_regionpol == '4'] <-'Sub-Saharan Africa'
VDEM2$e_regionpol[VDEM2$e_regionpol == '5'] <-'W.Europe and N. America'
VDEM2$e_regionpol[VDEM2$e_regionpol == '6'] <-'East Asia'
VDEM2$e_regionpol[VDEM2$e_regionpol == '7'] <-'Southeast Asia'
VDEM2$e_regionpol[VDEM2$e_regionpol == '8'] <- 'South Asia'
VDEM2$e_regionpol[VDEM2$e_regionpol == '9'] <- 'The Pacific'
VDEM2$e_regionpol[VDEM2$e_regionpol == '10'] <- 'The Caribbean'

VDEM2$e_regionpol <-as.factor(VDEM2$e_regionpol)

#plots

ggplot(VDEM2, aes(x = year, y = v2x_libdem, color=e_regionpol))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Liberal Democracy index')+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 14, color = 'gray'),
    legend.text = element_text(size = 14, color = 'black'),
    legend.title = element_text(size = 14, face = 'bold'),
  
  )


VDEM2$e_regionpol_6C <-as.factor(VDEM2$e_regionpol_6C)

VDEM2$e_regionpol_6C <-as.character(VDEM2$e_regionpol_6C)

VDEM2$e_regionpol_6C[VDEM2$e_regionpol_6C == '1'] <- 'E. Europe and C. Asia'
VDEM2$e_regionpol_6C[VDEM2$e_regionpol_6C == '2'] <- 'Latin America and the Caribbean'
VDEM2$e_regionpol_6C[VDEM2$e_regionpol_6C == '3'] <- 'MENA'
VDEM2$e_regionpol_6C[VDEM2$e_regionpol_6C == '4'] <- 'Sub-Saharan Africa'
VDEM2$e_regionpol_6C[VDEM2$e_regionpol_6C == '5'] <- 'W. Europe and North America'
VDEM2$e_regionpol_6C[VDEM2$e_regionpol_6C == '6'] <- 'Asia and Pacific'

#plot
ggplot(VDEM2, aes(x = year, y = v2x_libdem, color=e_regionpol_6C))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Liberal Democracy index')+ xlim(1900,2020)+
  scale_shape_manual(values = c(1900, 2020, 20)) +
  scale_x_continuous(breaks=seq(1900, 2020, 20), 
                     limits = c(1900, 2020)) +
  facet_wrap(~e_regionpol_6C) + 
  theme_bw() +theme(legend.position = 'none')

#export
ggsave("VDem_regions.png",
       width=11.7, height=8.3, units="in", dpi=100)

ggsave("VDem_regions.tiff",
       width=11.7, height=8.3, units="in", dpi=100)


#per regime type

VDEM2$v2x_regime <-as.character(VDEM2$v2x_regime)

VDEM2$v2x_regime[VDEM2$v2x_regime == 0] <-"Closed Autocracy"
VDEM2$v2x_regime[VDEM2$v2x_regime == 1] <-"Electoral Autocracy"
VDEM2$v2x_regime[VDEM2$v2x_regime == 2] <- "Electoral Democracy"
VDEM2$v2x_regime[VDEM2$v2x_regime == 3] <- "Liberal Democracy"


class(VDEM2$v2x_regime)

VDEM2$v2x_regime <-as.factor(VDEM2$v2x_regime)
is.na(VDEM2$v2x_regime)

library(tidyverse)
VDEM2 %>% 
  drop_na(v2x_regime, v2x_libdem) %>%
  ggplot(aes(x = year, y = v2x_libdem, color=v2x_regime))+
           geom_line(lwd=2)+ 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Liberal Democracy index')+ xlim(1900,2020)+
  scale_shape_manual(values = c(1900, 2020, 20)) +
  scale_x_continuous(breaks=seq(1900, 2020, 20), 
                     limits = c(1900, 2020)) +
  theme_bw() + guides(color=guide_legend("Regimes of the World"))


#export
ggsave("VDem_regimes.png",
       width=10.7, height=7.3, units="in", dpi=80)

ggsave("VDem_regimes.tiff",
       width=10.7, height=7.3, units="in", dpi=80)


VDEM2$v2x_regime[VDEM2$v2x_regime] <-NA
ggplot(VDEM2, aes(x = year, y = v2x_libdem, color=v2x_regime))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Liberal Democracy index')+ xlim(1900,2020)+
  scale_shape_manual(values = c(1900, 2020, 20)) +
  scale_x_continuous(breaks=seq(1900, 2020, 20), 
                     limits = c(1900, 2020)) +
  facet_wrap(~v2x_regime) + 
  theme_classic() 


#rule of law


ggplot(VDEM2, aes(x = year, y = v2x_rule, color=e_regionpol_6C))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Rule of Law index')+ xlim(1900,2020)+
  scale_shape_manual(values = c(1900, 2020, 20)) +
  scale_x_continuous(breaks=seq(1900, 2020, 20), 
                     limits = c(1900, 2020)) +
  facet_wrap(~e_regionpol_6C) + 
  theme_bw() + theme(legend.position = 'none')

ggplot(VDEM2, aes(x = year, y = v2x_rule, color=e_regionpol_6C))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Rule of Law index')+  
  theme_bw() 

ggplot(VDEM2, aes(x = year, y = v2x_rule, color=e_regionpol_6C))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Rule of Law index')+ xlim(1900,2020)+
  scale_shape_manual(values = c(1900, 2020, 20)) +
  scale_x_continuous(breaks=seq(1900, 2020, 20), 
                     limits = c(1900, 2020)) +
  theme_bw() 



#check variables
names(VDEM)
unique(VDEM$e_regionpol)

#subset data - asian countries
Asia <-subset(VDEM, e_regionpol >= 6 & e_regionpol <= 7)
Asia2 <-subset(Asia, year >= 211)


#save a copy in stata
library(haven)
write_dta(Asia, 'Asia.VDem.dta')


#Replication Political Terror Scale####

rm(list=ls())

setwd("C:/Users/User/Desktop/R projects/Dataset Sources etc/Political Terror  Scale")

#load data

load("C:/Users/User/Desktop/R projects/Dataset Sources etc/Political Terror  Scale/PTS-2020.RData")

PTS <-PTS_2020

#check data
names(PTS)

unique(PTS$Region)

ggplot(PTS, aes(x = Country, y = NA_Status_S))+
  geom_line(lwd=2) + 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Political Terror Score')+ 
  theme_bw() 

library(tidyverse)
PTS %>% 
  drop_na(Region, PTS_A,PTS_H, PTS_S) %>%
  ggplot(aes(x = Year, y = PTS_S))+
  geom_line(lwd=2)+ 
  geom_hline(yintercept = .5, lwd=.5, color='black') +
  xlab('Year')+
  ylab('Political Terror Score - Amnesty International')+
  theme_bw() + guides(color=guide_legend("Regions"))




library(ggplot2)
library(reshape2)



set.seed(123)
Week <- c("2015_52", "2016_01", "2016_02", "2016_03", "2016_04")
y1 <- runif(5, 0, 1)
y2 <- runif(5, 0, 1)
y3 <- runif(5, 0, 1)
df <- data.frame(Week, y1, y2, y3)

mdf <- melt(df,id.vars="Week")

ggplot(mdf, aes( x=Week, y=value, colour=variable, group=variable )) + 
 geom_line() +
 scale_color_manual(values=c("y1"="black","y2"="red","y3"="orange")) +
 scale_linetype_manual(values=c("y1"="solid","y2"="solid","y3"="dashed"))




#subset

EastAsiaP <-subset(PTS, Region == 'eap')

#remove pacific countries
unique(EastAsiaP$Country)

EAP <-subset(EastAsiaP, 
             Country == 'Brunei Darussalam' | Country == 'Cambodia' |
              Country == 'China' |
              Country == 'Indonesia' |
              Country == 'Japan' |
              Country == "Korea, Democratic People's Republic of" | 
              Country == 'Korea, Republic of' |
              Country == "Lao People's Democratic Republic" |
              Country == 'Malaysia' |
              Country == 'Mongolia' |
             Country == 'Myanmar' |
              Country == 'Philippines'|
              Country == 'Taiwan, Province of China'|
              Country == 'Thailand'|
              Country == 'Timor-Leste' |
              Country == 'Viet Nam' )


#copy
#save a copy in stata
library(haven)
write_dta(EAP, 'EastAsia.PTS.dta')


