+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
"Disentangling the effect of perceived threats on institutional trust and support for strong leaders"
replication code 
2022-07-29
Ronald A. Pernia 
rpernia21@gmail.com
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+

* Wave 6
clear
use "WVS_Cross-National_Wave_6_stata.dta" 

*Step 1: Select variables
*DV(1): Institutional Trust
gen trust_army = 5-V109
gen trust_police = 5-V113
gen trust_courts = 5-V114
gen trust_govt = 5-V115
gen trust_pol_parties = 5-V116
gen trust_congress = 5- V117
gen trust_civ_serv = 5-V118
*realiability
alpha trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv 
pca trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv 
*Index of Institutional Trust
gen poltrust_no_prot = (trust_courts + trust_govt + trust_pol_parties + trust_congress + trust_civ_serv )/5
*DV(2): Authoritarian Values
gen strong_leader = 5-V127
gen army_rule = 5- V129
*Independent variables
*Proximal Threat
gen unsafe_cr_hm = 5-V189
gen insecure_nghbrs = V170
*Distal Threat
gen worry_terrorsm = 5-V184
gen worry_civ_war = 5-V185
gen worry_abroad =5-V183
*Controls
gen satis_household_inc = V59
gen interest_politics = 5-V84
gen social_trust = 3-V24
gen dem_perception = V141
gen corruption = MN_228N
*Socio-demo controls 
gen age = V242
gen female = V240
gen education = V248
gen pray = 9-V146
gen social_class = 6-V238
*Sources of information
gen info_newspaper = 6-V217
gen info_TV = 6-V219
gen info_radio = 6-V220
gen info_phone = 6-V221
gen info_email = 6-V222
gen info_internet = 6-V223
gen info_talk_friends = 6-V224

*ideology
gen ideology = V95
gen vote_natl = 4- V227

*recoding wvs6
recode V2 (12=1) (31=2) (32=3) (36=4) (51=5) (76=6) (112=7) (152=8) (156=9) (158=10) (170=11) (196=12) (218=13) (233=14) (268=15) (275=16) (276=17) (288=18) (332=19)(344=20) (356=21) (368=22) (392=23) (398=24) ///
(400=25) (410=26) (414=27) (417=28) (422=29) (434=30) (458=31) (484=32) (504=33) (528=34) (554=35) (566=36) (586=37) (604=38) (608=39) (616=40) (634=41) (642=42) (643=43) (646=44) (702=45) (705=46) (710=47) (716=48) ///
(724=49) (752=50) (764=51) (780=52) (788=53) (792=54) (804=55) (818=56) (840=57) (858=58) (860=59) (887=60), gen(country) 
gen countrymerge = country + 100
gen survey = 1
gen year = V262
sort country
*drop V1-country

keep trust_army trust_police trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv poltrust_no_prot strong_leader army_rule unsafe_cr_hm insecure_nghbrs worry_terrorsm worry_civ_war worry_abroad satis_household_inc interest_politics social_trust dem_perception corruption age female education pray social_class info_newspaper info_TV info_radio info_phone info_email info_internet info_talk_friends ideology vote_natl country countrymerge survey year 

order country, first
order countrymerge, after(country)
order survey, after(countrymerge)
order year, after(survey)

save wvs6.dta, replace

*Wave 7
clear
use "WVS_Cross-National_Wave_7_stata_v4_0.dta" 

*DV(1): Institutional Trust 
gen trust_army = 5-Q65
gen trust_police = 5-Q69
gen trust_courts = 5-Q70
gen trust_govt = 5-Q71
gen trust_pol_parties = 5-Q72
gen trust_congress = 5- Q73
gen trust_civ_serv = 5-Q74
*check for reliability and homogeneity
alpha trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv 
pca trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv 
*Index of institutional trust
gen poltrust_no_prot = (trust_courts + trust_govt + trust_pol_parties + trust_congress + trust_civ_serv )/5
*DV(2): Authoritarian Values
gen strong_leader = 5-Q235
gen army_rule = 5- Q237
*Independent variables
*Proximal Threat
gen unsafe_cr_hm = 5-Q52
gen insecure_nghbrs = Q131
*Distal Threat
gen worry_terrorsm = 5-Q147
gen worry_civ_war = 5-Q148
gen worry_abroad =5-Q146
*Controls
gen satis_household_inc = Q50
gen interest_politics = 5-Q199
gen social_trust = 3-Q57
gen dem_perception = Q251
gen corruption = Q112
*Socio-demo controls 
gen age = Q262
gen female = Q260
gen education = Q275
gen pray = 9-Q172
gen social_class = 6-Q287
*Sources of information
gen info_newspaper = 6-Q201
gen info_TV = 6-Q202
gen info_radio = 6-Q203
gen info_phone = 6-Q204
gen info_email = 6-Q205
gen info_internet = 6-Q206
gen info_talk_friends = 6-Q208

*ideology
gen ideology = Q240
gen vote_natl = 5-Q222


*Recoding wvs7
recode B_COUNTRY (20=1) (32=2) (36=3) (50=4) (51=5) (68=6) (76=7) (104=8) (124=9) (152=10) (156=11) (158=12) (170=13) (196=14) (218=15) (231=16) (276=17) (300=18) (320=19) (344=20) (360=21)(364=22) (368=23) (392=24) (398=25) (400=26) (404=27) (410=28) (417=29) (422=30) (434=31) (446=32) (458=33) (462=34)(484=35) (496=36) (504=37) (528=38) (554=39) (558=40) (566=41) (586=42) (604=43) (608=44) (630=45) (642=46) (643=47) (688=48) (702=49) (704=50) (716=51) (762=52) (764=53) (788=54) (792=55) (804=56) (818=57) (840=58)(862=59), gen(country) 

gen countrymerge = country + 200
gen survey = 2
gen year = A_YEAR
sort country
*drop A_WAVE-country
*drop version doi

keep trust_army trust_police trust_courts trust_govt trust_pol_parties trust_congress trust_civ_serv poltrust_no_prot strong_leader army_rule unsafe_cr_hm insecure_nghbrs worry_terrorsm worry_civ_war worry_abroad satis_household_inc interest_politics social_trust dem_perception corruption age female education pray social_class info_newspaper info_TV info_radio info_phone info_email info_internet info_talk_friends ideology vote_natl country countrymerge survey year

order country, first
order countrymerge, after(country)
order survey, after(countrymerge)
order year, after(survey)

save wvs7.dta, replace


//** merge the survey data **//
use .\wvs7.dta
append using .\wvs6.dta
sort countrymerge

save mergedsurveys.dta, replace

//** label and recode the merged survey data **//
use .\mergedsurveys.dta

*** adding an id variable
gen id = _n

*** labeling the data
rename countrymerge country_year

label define country_year 101 "Algeria 2014 (WVS6)" 102 "Azerbaijan 2011 (WVS6)" 103 "Argentina 2013 (WVS6)" 104 "Australia 2012 (WVS6)" 105 "Armenia 2011 (WVS6)" 106 "Brazil 2014 (WVS6)" 107 "Belarus 2011 (WVS6)" 108 "Chile 2012 (WVS6)" 109 "China 2013 (WVS6)" 110 "Taiwan 2012 (WVS6)" ///
111 "Colombia 2012 (WVS6)" 112 "Cyprus 2011 (WVS6)" 113 "Ecuador 2013 (WVS6)" 114 "Estonia 2011 (WVS6)" 115 "Georgia 2014 (WVS6)" 116 "Palestine/West Bank (WVS6)" 117 "Germany 2013 WVS6)" 118 "Ghana 2012 (WVS6)" 119 "Haiti 2016 (WVS6)" 120 "Hong Kong 2014 (WVS6)" ///
121 "India 2014 (WVS6)" 122 "Iraq 2013 (WVS6)" 123 "Japan 2010 (WVS6)" 124 "Kazakhstan 2011 (WVS6)" 125 "Jordan 2014 (WVS6)" 126 "South Korea 2010 (WVS6)" 127 "Kuwait 2014 (WVS6)" 128 "Kyrgyzstan 2011 (WVS6)" 129 "Lebanon 2013 WVS6)" 130 "Libya 2014 (WVS6)" 131 "Malaysia 2012 (WVS6)" 132 "Mexico 2012 (WVS6)" 133 "Morocco 2011 (WVS6)" 134 "Netherlands 2012 (WVS6)" 135 "New Zealand 2011 (WVS6)" 136 "Nigeria 2012 (WVS6)" 137 "Pakistan 2012 (WVS6)" 138 "Peru 2012 (WVS6)" 139 "Philippines 2012 (WVS6)" 140 "Poland 2012 (WVS6)" ///
141 "Qatar 2010  (WVS6)" 142 "Romania 2012 (WVS6)" 143 "Russia 2011 (WVS6)" 144 "Rwanda 2012 (WVS6)" 145 "Singapore 2012 (WVS6)" 146 "Slovenia 2011 (WVS6)" 147 "South Africa 2013 (WVS6)" 148 "Zimbabwe 2012 (WVS6)" 149 "Spain 2011 (WVS6)" 150 "Sweden 2011 (WVS6)" ///
151 "Thailand 2013 (WVS6)" 152 "Trinidad and Tobago 2010 (WVS6)" 153 "Tunisia 2013 (WVS6)" 154 "Turkey 2012 (WVS6)" 155 "Ukraine 2011 (WVS6)" 156 "Egypt 2012 (WVS6)" 157 "United States of America 2011 (WVS6)" 158 "Uruguay 2011 (WVS6)" 159 "Uzbekistan 2011 (WVS6)" 160 "Yemen 2014 (WVS6)" 201 "Andorra 2018 (WVS7)" 202 "Argentina 2017 (WVS7)" 203 "Australia 2018 (WVS7)" 204 "Bangladesh 2018 (WVS7)" 205 "Armenia 2018 (WVS7)" 206 "Boliva 2017 (WVS7)" 207 "Brazil 2018 (WVS7)" 208 "Burma/Myanmar 2020 (WVS7)" 209 "Canada 2020 (WVS7)" 210 "Chile 2018 (WVS7)" 211 "China 2019 (WVS7)" 212 "Taiwan 2019 (WVS7)" 213 "Colombia 2018 (WVS7)" 214 "Cyprus 2019 (WVS7)" 215 "Ecuador 2018 (WVS7)" 216 "Ethiopia 2020 (WVS7)" 217 "Germany 2017 (WVS7)" 218 "Greece 2017 (WVS7)" 219 "Guatemala 2020 (WVS7)" 220 "Hong Kong 2018 (WVS7)" 221 "Indonesia 2018 (WVS7)" 222 "Iran 2020 (WVS7)" 223 "Iraq 2018 (WVS7)" 224 "Japan 2019 (WVS7)" 225 "Kazakhstan 2018 (WVS7)" 226 "Jordan 2018 (WVS7)" 227 "Kenya 2021 (WVS7)" 228 "South Korea 2018 (WVS7)" 229 "Kyrgyzstan 2020 (WVS7)" 230 "Lebanon 2018 (WVS7)" 231 "Libya 2022 (WVS7)" 232 "Macau 2020 (WVS7)" 233 "Malaysia 2018 (WVS7)" 234 "Maldives 2021 (WVS7)" 235 "Mexico 2018 (WVS7)" 236 "Mongolia 2020 (WVS7)" 237 "Morocco 2021 (WVS7)" 238 "Netherlands 2017 (WVS7)" 239 "New Zealand 2020 (WVS7)" 240 "Nicaragua 2020 (WVS7)" 241 "Nigeria 2018 (WVS7)" 242 "Pakistan 2018 (WVS7)" 243 "Peru 2018 (WVS7)" 244 "Philippines 2019 (WVS7)" 245 "Puerto Rico 2018 (WVS7)" 246 "Romania 2018 (WVS7)" 247 "Russia 2017 (WVS7)" 248 "Serbia 2018 (WVS7)" 249 "Singapore 2020 (WVS7)" 250 "Vietnam 2020 (WVS7)" ///
251 "Zimbabwe 2020 (WVS7)" 252 "Tajikistan 2020 (WVS7)" 253 "Thailand 2018 (WVS7)" 254 "Tunisia 2019 (WVS7)" 255 "Turkey 2018 (WVS7)" 256 "Ukraine 2020 (WVS7)" 257 "Egypt 2018 (WVS7)" 258 "United States of America 2017 (WVS7)" 259 "Venezuela 2021 (WVS7)" ///

label values country_year country_year

label define survey 1 "WVS6" 2 "WVS7"

label val survey survey

save mergedsurveys.dta, replace

*************************
* adding the macro data
*************************
//** recoding the V-Dem data **//
clear 
use "V-Dem-CY-Full+Others-v11.1.dta"

drop if year <=2009

keep country_name country_id year v2x_libdem v2x_polyarchy v2x_regime e_wbgi_pve v2exl_legitlead v2exl_legitperf v2exl_legitratio

save vdem_v11_2022.dta, replace

*recode 
gen country = .
*no data for Andorra
replace country=101 if country_name=="Algeria" & year==2014
replace country=102 if country_name=="Azerbaijan" & year==2011
replace country=103 if country_name=="Argentina" & year==2013
replace country=104 if country_name=="Australia" & year==2012
replace country=105 if country_name=="Armenia" & year==2011
replace country=106 if country_name=="Brazil" & year==2014
replace country=107 if country_name=="Belarus" & year==2011
replace country=108 if country_name=="Chile" & year==2012
replace country=109 if country_name=="China" & year==2013
replace country=110 if country_name=="Taiwan" & year==2012

replace country=111 if country_name=="Colombia" & year==2012
replace country=112 if country_name=="Cyprus" & year==2011
replace country=113 if country_name=="Ecuador" & year==2013
replace country=114 if country_name=="Estonia" & year==2011
replace country=115 if country_name=="Georgia" & year==2014
replace country=116 if country_name=="Palestine/West Bank" & year==2013
replace country=117 if country_name=="Germany" & year==2013
replace country=118 if country_name=="Ghana" & year==2012
replace country=119 if country_name=="Haiti" & year==2016
replace country=120 if country_name=="Hong Kong" & year==2014

replace country=121 if country_name=="India" & year==2014
replace country=122 if country_name=="Iraq" & year==2013
replace country=123 if country_name=="Japan" & year==2010
replace country=124 if country_name=="Kazakhstan" & year==2011
replace country=125 if country_name=="Jordan" & year==2014
replace country=126 if country_name=="South Korea" & year==2010
replace country=127 if country_name=="Kuwait" & year==2014
replace country=128 if country_name=="Kyrgyzstan" & year==2011
replace country=129 if country_name=="Lebanon" & year==2013
replace country=130 if country_name=="Libya" & year==2014

replace country=131 if country_name=="Malaysia" & year==2012
replace country=132 if country_name=="Mexico" & year==2012
replace country=133 if country_name=="Morocco" & year==2011
replace country=134 if country_name=="Netherlands" & year==2012
replace country=135 if country_name=="New Zealand" & year==2011
replace country=136 if country_name=="Nigeria" & year==2012
replace country=137 if country_name=="Pakistan" & year==2012
replace country=138 if country_name=="Peru" & year==2012
replace country=139 if country_name=="Philippines" & year==2012
replace country=140 if country_name=="Poland" & year==2012

replace country=141 if country_name=="Qatar" & year==2010
replace country=142 if country_name=="Romania" & year==2012
replace country=143 if country_name=="Russia" & year==2011
replace country=144 if country_name=="Rwanda" & year==2012
replace country=145 if country_name=="Singapore" & year==2012
replace country=146 if country_name=="Slovenia" & year==2011
replace country=147 if country_name=="South Africa" & year==2013
replace country=148 if country_name=="Zimbabwe" & year==2012
replace country=149 if country_name=="Spain" & year==2011
replace country=150 if country_name=="Sweden" & year==2011

replace country=151 if country_name=="Thailand" & year==2013
replace country=152 if country_name=="Trinidad and Tobago" & year==2010
replace country=153 if country_name=="Tunisia" & year==2013
replace country=154 if country_name=="Turkey" & year==2012
replace country=155 if country_name=="Ukraine" & year==2011
replace country=156 if country_name=="Egypt" & year==2012
replace country=157 if country_name=="United States of America" & year==2011
replace country=158 if country_name=="Uruguay" & year==2011
replace country=159 if country_name=="Uzbekistan" & year==2011
replace country=160 if country_name=="Yemen" & year==2014

replace country=201 if country_name=="Andorra" & year==2018
replace country=202 if country_name=="Argentina" & year==2017
replace country=203 if country_name=="Australia" & year==2018
replace country=204 if country_name=="Bangladesh" & year==2018
replace country=205 if country_name=="Armenia" & year==2018
replace country=206 if country_name=="Bolivia" & year==2014
replace country=207 if country_name=="Brazil" & year==2018
replace country=208 if country_name=="Burma/Myanmar" & year==2020 
replace country=209 if country_name=="Canada" & year==2020
replace country=210 if country_name=="Chile" & year==2018

replace country=211 if country_name=="China" & year==2019
replace country=212 if country_name=="Taiwan" & year==2019
replace country=213 if country_name=="Colombia" & year==2018
replace country=214 if country_name=="Cyprus" & year==2019
replace country=215 if country_name=="Ecuador" & year==2018
replace country=216 if country_name=="Ethiopia" & year==2020
replace country=217 if country_name=="Germany" & year==2017
replace country=218 if country_name=="Greece" & year==2017
replace country=219 if country_name=="Guatemala" & year==2020
replace country=220 if country_name=="Hong Kong" & year==2018

replace country=221 if country_name=="Indonesia" & year==2018
replace country=222 if country_name=="Iran" & year==2020
replace country=223 if country_name=="Iraq" & year==2018
replace country=224 if country_name=="Japan" & year==2019
replace country=225 if country_name=="Kazakhstan" & year==2018
replace country=226 if country_name=="Jordan" & year==2018
*replace country=227 if country_name=="Kenya" & year==2021: no data
replace country=228 if country_name=="South Korea" & year==2018
replace country=229 if country_name=="Kyrgyzstan" & year==2020
replace country=230 if country_name=="Lebanon" & year==2018

*replace country=231 if country_name=="Libya" & year==2022: no data
*replace country=232 if country_name=="Macau" & year==2020: no data on Macau 
replace country=233 if country_name=="Malaysia" & year==2021
replace country=234 if country_name=="Maldives" & year==2018
replace country=235 if country_name=="Mexico" & year==2018
replace country=236 if country_name=="Mongolia" & year==2020
*replace country=237 if country_name=="Morocco" & year==2021: no data
replace country=238 if country_name=="Netherlands" & year==2017
replace country=239 if country_name=="New Zealand" & year==2020
replace country=240 if country_name=="Nicaragua" & year==2020

replace country=241 if country_name=="Nigeria" & year==2018
replace country=242 if country_name=="Pakistan" & year==2018
replace country=243 if country_name=="Peru" & year==2018
replace country=244 if country_name=="Philippines" & year==2019
*replace country=245 if country_name=="Puerto Rico" & year==2018: no data
replace country=246 if country_name=="Romania" & year==2018
replace country=247 if country_name=="Russia" & year==2017
replace country=248 if country_name=="Serbia" & year==2018
replace country=249 if country_name=="Singapore" & year==2019
replace country=250 if country_name=="Vietnam" & year==2020

replace country=251 if country_name=="Zimbabwe" & year==2020
replace country=252 if country_name=="Tajikistan" & year==2020
replace country=253 if country_name=="Thailand" & year==2018
replace country=254 if country_name=="Tunisia" & year==2019
replace country=255 if country_name=="Turkey" & year==2018
replace country=256 if country_name=="Ukraine" & year==2020
replace country=257 if country_name=="Egypt" & year==2018
replace country=258 if country_name=="United States of America" & year==2017
*replace country=259 if country_name=="Venezuela" & year==2021: no data

sort country
rename year vdemyear
rename country_id vdemid

rename country country_year
drop if country_year==.
tab country_year
*drop if country==.
*tab country

*define

label define country_year 101 "Algeria 2014 (WVS6)" 102 "Azerbaijan 2011 (WVS6)" 103 "Argentina 2013 (WVS6)" 104 "Australia 2012 (WVS6)" 105 "Armenia 2011 (WVS6)" 106 "Brazil 2014 (WVS6)" 107 "Belarus 2011 (WVS6)" 108 "Chile 2012 (WVS6)" 109 "China 2013 (WVS6)" 110 "Taiwan 2012 (WVS6)" 111 "Colombia 2012 (WVS6)" 112 "Cyprus 2011 (WVS6)" 113 "Ecuador 2013 (WVS6)" 114 "Estonia 2011 (WVS6)" 115 "Georgia 2014 (WVS6)" 116 "Palestine/West Bank 2013 (WVS6)" 117 "Germany 2013 WVS6)" 118 "Ghana 2012 (WVS6)" 119 "Haiti 2016 (WVS6)" 120 "Hong Kong 2014 (WVS6)" 121 "India 2014 (WVS6)" 122 "Iraq 2013 (WVS6)" 123 "Japan 2010 (WVS6)" 124 "Kazakhstan 2011 (WVS6)" 125 "Jordan 2014 (WVS6)" 126 "South Korea 2010 (WVS6)" 127 "Kuwait 2014 (WVS6)" 128 "Kyrgyzstan 2011 (WVS6)" 129 "Lebanon 2013 WVS6)" 130 "Libya 2014 (WVS6)" 131 "Malaysia 2012 (WVS6)" 132 "Mexico 2012 (WVS6)" 133 "Morocco 2011 (WVS6)" 134 "Netherlands 2012 (WVS6)" 135 "New Zealand 2011 (WVS6)" 136 "Nigeria 2012 (WVS6)" 137 "Pakistan 2012 (WVS6)" 138 "Peru 2012 (WVS6)" 139 "Philippines 2012 (WVS6)" 140 "Poland 2012 (WVS6)" 141 "Qatar 2010  (WVS6)" 142 "Romania 2012 (WVS6)" 143 "Russia 2011 (WVS6)" 144 "Rwanda 2012 (WVS6)" 145 "Singapore 2012 (WVS6)" 146 "Slovenia 2011 (WVS6)" 147 "South Africa 2013 (WVS6)" 148 "Zimbabwe 2012 (WVS6)" 149 "Spain 2011 (WVS6)" 150 "Sweden 2011 (WVS6)" 151 "Thailand 2013 (WVS6)" 152 "Trinidad and Tobago 2010 (WVS6)" 153 "Tunisia 2013 (WVS6)" 154 "Turkey 2012 (WVS6)" 155 "Ukraine 2011 (WVS6)" 156 "Egypt 2012 (WVS6)" 157 "United States of America 2011 (WVS6)" 158 "Uruguay 2011 (WVS6)" 159 "Uzbekistan 2011 (WVS6)" 160 "Yemen 2014 (WVS6)" 201 "Andorra 2018 (WVS7)" 202 "Argentina 2017 (WVS7)" 203 "Australia 2018 (WVS7)" 204 "Bangladesh 2018 (WVS7)" 205 "Armenia 2018 (WVS7)" 206 "Boliva 2017 (WVS7)" 207 "Brazil 2018 (WVS7)" 208 "Burma/Myanmar 2020 (WVS7)" 209 "Canada 2020 (WVS7)" 210 "Chile 2018 (WVS7)" 211 "China 2019 (WVS7)" 212 "Taiwan 2019 (WVS7)" 213 "Colombia 2018 (WVS7)" 214 "Cyprus 2019 (WVS7)" 215 "Ecuador 2018 (WVS7)" 216 "Ethiopia 2020 (WVS7)" 217 "Germany 2017 (WVS7)" 218 "Greece 2017 (WVS7)" 219 "Guatemala 2020 (WVS7)" 220 "Hong Kong 2018 (WVS7)" 221 "Indonesia 2018 (WVS7)" 222 "Iran 2020 (WVS7)" 223 "Iraq 2018 (WVS7)" 224 "Japan 2019 (WVS7)" 225 "Kazakhstan 2018 (WVS7)" 226 "Jordan 2018 (WVS7)" 227 "Kenya 2021 (WVS7)" 228 "South Korea 2018 (WVS7)" 229 "Kyrgyzstan 2020 (WVS7)" 230 "Lebanon 2018 (WVS7)" 231 "Libya 2022 (WVS7)" 232 "Macau 2020 (WVS7)" 233 "Malaysia 2018 (WVS7)" 234 "Maldives 2021 (WVS7)" 235 "Mexico 2018 (WVS7)" 236 "Mongolia 2020 (WVS7)" 237 "Morocco 2021 (WVS7)" 238 "Netherlands 2017 (WVS7)" 239 "New Zealand 2020 (WVS7)" 240 "Nicaragua 2020 (WVS7)" 241 "Nigeria 2018 (WVS7)" 242 "Pakistan 2018 (WVS7)" 243 "Peru 2018 (WVS7)" 244 "Philippines 2019 (WVS7)" 245 "Puerto Rico 2018 (WVS7)" 246 "Romania 2018 (WVS7)" 247 "Russia 2017 (WVS7)" 248 "Serbia 2018 (WVS7)" 249 "Singapore 2020 (WVS7)" 250 "Vietnam 2020 (WVS7)" ///
251 "Zimbabwe 2020 (WVS7)" 252 "Tajikistan 2020 (WVS7)" 253 "Thailand 2018 (WVS7)" 254 "Tunisia 2019 (WVS7)" 255 "Turkey 2018 (WVS7)" 256 "Ukraine 2020 (WVS7)" 257 "Egypt 2018 (WVS7)" 258 "United States of America 2017 (WVS7)" 259 "Venezuela 2021 (WVS7)" ///

label values country_year country_year

gen row = .
replace row=1 if (v2x_regime==2 | v2x_regime==3) // * 2 and 3 are democracies//
replace row=0 if (v2x_regime==0 | v2x_regime==1) // * 0 and 1 are autocracies//
tab row //* 48 autocracies, 71 democracies//
save vdem_v11_2022.dta, replace 
save macrodata.dta, replace

//** merging the macrodata with the survey data **//
use .\mergedsurveys.dta
merge m:1 country_year using macrodata.dta
drop _merge
save mergesurveyandmacro.dta, replace



*Adding Actual Terror Scale

//** recoding the Global Terrorism data **//
*clear
*use globalterrorism_2022.dta
*drop if Year <=2009
*save globalterror_2022.dta, replace

*put the scores/counts; it is not a scale; it is a count data: from 0 to ~ 
*just add in manually

recode country_year (101=13) (106=3) (107=1) (108=2) (109=13) (119=1) (124=5) (125=3) (130=729) (131=2) (132=16) (133=1) ///
(136=616) (137=1654) (138=9) (139=249) (143=211) (144=6) (154=188) (155=3) (156=49) (157=10) (160=763) (207=18) (217=27) (218=44) ///
(221=43) (223=1362) (226=2) (228=1) (230=7) (233=7) (235=27) (238=1) (241=645) (242=480) (243=5) (253=182) (255=94) (257=54) (258=66), gen(gtdscore)

tab gtdscore
lab var gtdscore "Global Terrorism Database: recorded frequency of terrorist attacks"
*lab define gtdscore "recorded frequency of terrorist attacks"
lab values gtdscore gtdscore

//*no data Azerbaijan// (103=2) //*no data australia 2012, armenia 2011//
//*no data for taiwan 2012// (111=115)
//*no data cyprus 2011// (113=2) (114=1) (115=2) //*no data for Palestine, germany 2013, ghana 2012//
//*no data hongkong// (121=860) (122=2852) //*no data japan 2010// 
//*no data south korea 2010, kuwait 2014// (128=1) (129=121)
//*no data netherlands 2012, new zealand 2011//
//*no data poland 2012, qatar 2010, romania 2012//
//*no data singapore 2012, slovenia 2011// (147=13)
//*no data zimbabwe 2012, spain 2011// (150=1) (151=472) (152=1) (153=29)
//*no data uruguay 2011, uzbekistan 2011// 
//*no data andorra 2018// (202=3) (203=3) (204=33) (205=1) //*no data bolivia 2017//
//*no data in myanmar 2020; only 2018// (210=45) (213=205) (215=18)//

save mergesurveysvdemgtd.dta, replace

*End of replication

