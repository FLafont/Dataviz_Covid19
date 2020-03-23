##################################################################################################
# Packages
##################################################################################################
library(readxl)
library(lubridate)
library(dplyr)
library(httr)
library(stringr)
library(purrr)
#library(tidyverse)

##################################################################################################
# lecture des sources
##################################################################################################
# 1) Base avec le nombre de cas de covid-19

#source : https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# Option 1 : fichier en date du 20/03/2020
#covid_base <- read_excel("data/COVID-19-geographic-disbtribution-worldwide-2020-03-19 (1).xlsx")

# Option 2: En téléchargeant direct le bon fichier sur le site de la commission
# permet d'être actualisé
# On créée l'url du site où la base de données est actualisée chaque jour

countries_selec <- c('FRA','GBR','ITA','ESP','DEU','CHN','JPN','KOR','MEX','USA')
#2) on importe les donnees OCDE les plus recentes
# source https://data.oecd.org/
setwd('C:/Users/francois.lafont@ccomptes.fr/Documents/DEV/shiny/coronavirus')
# oecd_data <- list.files('./','*.csv')
oecd_data <- Sys.glob('./data/oecd*.xlsx')

read_oecd <- function(x){

  read_xlsx(x) %>% rename(country=1) %>% group_by(country, INDICATOR) %>%
    filter(TIME==max(TIME)) %>% ungroup()  
  #keep only most recent year
}

df_oecd <- map_df(oecd_data,read_oecd) %>%
  filter(country %in% countries_selec) %>%
  filter(SUBJECT =='TOT') %>%
  filter(INDICATOR=='HEALTHEXP'&MEASURE=='USD_CAP'|
           INDICATOR %in% c('MEDICALDOC','NURSE')|
           INDICATOR =='GDP'&MEASURE=='USD_CAP'|
           INDICATOR=='ELDLYPOP'&MEASURE=='PC_POP'|
           INDICATOR =='INTERNET'&MEASURE=='PC_HH') 

oecd_indicator <- list()
for (i in df_oecd$INDICATOR){
  oecd_indicator[[i]] <- df_oecd %>% filter(INDICATOR==i) %>% select(country, Value) 
}

## on renomme 
health_exp <- oecd_indicator[['HEALTHEXP']] %>% rename(HEALTHEXP=Value)
internet_acc <- oecd_indicator[['INTERNET']] %>% rename(internet=Value)
med_doc <- oecd_indicator[['MEDICALDOC']] %>% rename(nb_doc=Value)
nurses <- oecd_indicator[['NURSE']] %>% rename(nb_nurse = Value)

df_oecd <- nurses %>%
  left_join(health_exp)%>%
  left_join(internet_acc) %>%
  left_join(med_doc)


wb_data <- Sys.glob('./data/wb*.xlsx')
read_wb<- function(x){
  
  read_xlsx(x, col_types = 'text') %>% select(country=1, country_code=2, INDICATOR=3, val2018=`2018`)
  #keep only most recent year
}


df_wb <- map_df(wb_data,read_wb) %>%
  
  filter(country_code %in% countries_selec | INDICATOR=='KOR') %>%
  mutate(val2018 = as.numeric(val2018),
         country_code = case_when(
           country_code=='Rep.'~'KOR',
           T~as.character(country_code)),
         INDICATOR = case_when(
           INDICATOR=='KOR'~'GDP per capita (current US$)',
           T~as.character(INDICATOR))
         ) 
  
wb_indicators <- list()

for (i in df_wb$INDICATOR){
  wb_indicators[[i]] <- df_wb %>% filter(INDICATOR==i) %>% select(country_code, Value=val2018)
}

eldly_pop <- wb_indicators[[1]] %>% rename(eldly_pop = Value) 
pop <- wb_indicators[[3]] %>% rename(pop=Value)
gdp_pc <- wb_indicators[[2]] %>% rename(gdp_pc = Value)

df_wb <- eldly_pop %>%
  left_join(pop) %>%
  left_join(gdp_pc)
df_world <- df_wb %>%
  left_join(df_oecd, by = c('country_code'='country'))

rm(df_oecd,df_wb,eldly_pop,gdp_pc,health_exp,internet_acc,med_doc,nurses,oecd_indicator,pop,wb_indicators)

###############################################"
########### DATA COVID 
###############################################
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date()-1, "%Y-%m-%d"), ".xlsx", sep = "")
# On télécharge le fichier depuis l'URL et on le stocke en local
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
# On lit le fichier temporaire comme on lirait un excel classique
covid_base <- read_excel(tf)

left = function(text, num_char) {
  substr(text, 1, num_char)
}


mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

##################################################################################################
# Mise en forme des données covid
##################################################################################################

# 1) On calcule le nombre de jours depuis le premier cas dans chaque pays
# On calcule le nombre de cas cumulés au fil du temps
covid <-  covid_base %>% 
  select(1,5:7)%>%
  rename(country=`Countries and territories`)%>%
  mutate(DateRep = date(DateRep))%>%
  
  group_by(country)%>%
  arrange(country,DateRep)%>%
  mutate(cum_cases = cumsum(Cases),
         cum_deaths = cumsum(Deaths),
         death_case_ratio = cum_deaths/cum_cases*100,
         #abrevation des pays pour fit avec l'OECD data
         country_abbrv = toupper(left(country,3)))
#######################################################²######## 
### traitement covid  
### creation 5, 10, 20, 40e jour ou dernier sinon
###############################################################


# covid <- covid %>%
#   filter(country_abbrv %in% countries_selec) %>%

#### gotta rename some abbreviations 
covid <- covid %>%
  mutate(country_abbrv2 = case_when(
    country=='United_States_of_America'~'USA',
    country=='Spain'~'ESP',
    country =='United_Kingdom'~'GBR',
    country=='Germany'~'DEU',
    country=='Japan'~'JPN',
    country=='South_Korea'~'KOR',
    country=='China'~'CHN',
    TRUE~country_abbrv)) %>%
  
  filter(country_abbrv2 %in% countries_selec) %>%
  
  group_by(country) %>%
  mutate(covid_daynr = DateRep - min(DateRep)+1) %>%
  filter(covid_daynr %in% c(5,10,20,40,50,60,max(covid_daynr)))

#### Calcul indicator covid par habitants 
covid_calc <- covid %>% left_join(df_world, by =c('country_abbrv2'='country_code')) %>%
  
  mutate(case_100khab = cum_cases / pop *1e5,
         death_100khab = cum_deaths/pop*1e5)

rm(covid_base)

# 
# ##3) On va lier la base de COVID avec les données OECD
# covid_oecd <- covid %>%
#   
#   inner_join(df_all, by = c('country_abbrv2'='country')) %>%
#   
#   mutate(Value = case_when (
#     INDICATOR == 'POP'~Value*1e6,
#     TRUE~Value)) %>%
#   
#   filter(SUBJECT =='TOT') %>%
#   filter(INDICATOR=='HEALTHEXP'&MEASURE=='PC_GDP'|
#            INDICATOR %in% c('MEDICALDOC','NURSE')|
#            INDICATOR =='POP'&MEASURE=='MLN_PER'|
#            INDICATOR =='GDP'&MEASURE=='USD_CAP') 
# 
# 
# # 4) creation de plusieurs tables
# 
# # 
# # oecd_indicator[['GDP']] <- oecd_indicator[['GDP']] %>%
# #   add_row(country='China',Value=9770.85)
# 
# #5) 
# covid_oecd <- covid_oecd %>%
#   
#   group_by(country) %>% 
#   mutate(pop = subset(covid_oecd, INDICATOR=='POP', select=Value))
# 
# # china_gdp <- read.csv2('./china_data.csv', sep=',', NA=T)
# # 
# # 
# # covid_oecd['INDICATOR']
# # ### Shiny 
# # covid_oecd[which(covid_oecd$INDICATOR=='POP')]
# #covid_oecd$pop <-  subset(covid_oecd, INDICATOR=='POP', select=Value)
# # 
# # x <- subset(covid_oecd, INDICATOR=='POP', select=Value)           
