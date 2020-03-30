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
setwd('C:/Users/francois.lafont@ccomptes.fr/Documents/DEV/shiny/coronavirus/Dataviz_Covid19')
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

### need mising health exp
wb_health<- read_xls('./data/wb_health_exp.xls', col_types = 'text') %>% select(country=1, country_code=2, INDICATOR=3, val2016=`2016`) %>%
  filter(country_code %in% c('JPN','KOR','USA')) %>%
  mutate(HEALTHEXP = as.numeric(val2016)) %>% 
  select(country_code,HEALTHEXP)

df_wb <- eldly_pop %>%
  left_join(pop) %>%
  left_join(gdp_pc) %>%
  left_join(wb_health)

df_world <- df_wb %>%
  left_join(df_oecd, by = c('country_code'='country')) %>%
  mutate(HEALTHEXP.x= ifelse(is.na(HEALTHEXP.x),HEALTHEXP.y,HEALTHEXP.x))%>%
  rename(HEALTHEXP= HEALTHEXP.x) %>%
  select(-HEALTHEXP.y) %>%
  mutate(HEALTHEXP=as.numeric(HEALTHEXP),
         nb_nurse=as.numeric(nb_nurse),
         internet=as.numeric(internet),
         nb_doc=as.numeric(nb_doc))

rm(df_oecd,df_wb,eldly_pop,gdp_pc,health_exp,internet_acc,med_doc,nurses,oecd_indicator,pop,wb_indicators)

###############################################"
########### DATA COVID 
###############################################
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.Date()-1, "%Y-%m-%d"), ".xlsx", sep = "")
# On télécharge le fichier depuis l'URL et on le stocke en local
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))
# On lit le fichier temporaire comme on lirait un excel classique
covid_base <- read_excel(tf)%>%
  rename(country= countriesAndTerritories, country_code = countryterritoryCode)%>%
  mutate(dateRep = date(dateRep))%>%
  
  select(1,5:10)%>%
  
  group_by(country)%>%
  arrange(country,dateRep)%>%
  mutate(cum_cases = cumsum(cases),
         cum_deaths = cumsum(deaths),
         death_case_ratio = cum_deaths/cum_cases*100) %>%
  filter(country_code %in% countries_selec) %>%
  
  group_by(country) %>%
  mutate(first_case = cum_cases>=1)%>%
  group_by(country,first_case) %>%
  mutate(covid_daynr = dateRep - min(dateRep)+1) %>%
  filter(covid_daynr>=1) %>%
  ungroup()


##################################################################################################
# Mise en forme des données covid
##################################################################################################

# 1) On calcule le nombre de jours depuis le premier cas dans chaque pays
# On calcule le nombre de cas cumulés au fil du temps

covid_calc<- covid_base %>%
  filter(covid_daynr %in% c(seq(1,61,2),max(covid_daynr))) %>%
  left_join(df_world, by =('country_code')) %>%
  
  mutate(case_100khab = cum_cases / pop *1e5,
         death_100khab = cum_deaths/pop*1e5,
         ln_cases= ifelse(cum_cases >0,log(cum_cases),0),
         ln_deaths = ifelse(cum_deaths>0,log(cum_deaths),0)) 



#######################################################²######## 
### traitement covid  
### creation 5, 10, 20, 40e jour ou dernier sinon
###############################################################


# covid <- covid %>%
#   filter(country_abbrv %in% countries_selec) %>%

#### gotta rename some abbreviations 

#### Calcul indicator covid par habitants 


covid_over10 <- covid_base%>%
  left_join(df_world, by =('country_code'))%>%
  group_by(country)%>%
  
  mutate(over_10_deaths = cum_deaths>=10) %>%
  group_by(country,over_10_deaths)%>%
  mutate(nb_day_since_10deaths = ifelse(over_10_deaths==T,dateRep -min(dateRep),0)) %>%
  filter(nb_day_since_10deaths >=1) %>%
  
  mutate(case_100khab = cum_cases / pop *1e5,
         death_100khab = cum_deaths/pop*1e5,
         ln_cases= ifelse(cum_cases >0,log(cum_cases),0),
         ln_deaths = ifelse(cum_deaths>0,log(cum_deaths),0)) %>%
  ungroup()


list_covid <- list('à partir de 10 décès'= covid_over10, 
                   'à partir du premier cas'= covid_calc)


rm(covid_base, wb_health)

