# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(gdxrrw)
library(imputeTS)
library(foreach)
library(openxlsx)


# Setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/')
odir <- paste0(root,'/output/')
xdir <- paste0(root,'/xlsx/')


### Input data ###

# IEA Energy Balance
IEA_EB <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020) %>% 
  select(Sector,Year,Total) %>% rename(value=Total)

# IEA Energy Balance -Historical share
IEA_EB_SHR <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020,Sector%in%c('Industry','Transport','Residential','Commercial and public services')) %>%
  transmute(Sector,Year,
            COL=`Coal and coal products`,
            OIL=`Crude, NGL and feedstocks`+`Oil products`,
            GAS=`Natural gas`,
            BMS=`Biofuels and waste`,
            ELE=`Electricity`) %>% 
  mutate(across(-c(Sector,Year),~./(COL+OIL+GAS+BMS+ELE))) %>%
  pivot_longer(cols=-c(Sector,Year),names_to='FIN',values_to='SHR_FIN')

IEA_EB_SHR_ELE1 <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020,Sector%in%c('Electricity output (GWh)')) %>%
  transmute(Sector,Year,
            COL=`Coal and coal products`,
            OIL=`Crude, NGL and feedstocks`+`Oil products`,
            GAS=`Natural gas`,
            NUC=`Nuclear`,
            HYD=`Hydro`,
            GEO=`Geothermal`,
            BMS=`Biofuels and waste`)

IEA_EB_SHR_ELE2 <- read_csv(paste0(ddir,'IEA_EB_ELE.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020,Sector%in%c('Electricity output (GWh)')) %>% 
  transmute(Sector,Year,
            PV=`Solar photovoltaics`,
            WIN=`Wind`)

IEA_EB_SHR_ELE <- full_join(IEA_EB_SHR_ELE1,IEA_EB_SHR_ELE2) %>% 
  mutate(across(-c(Sector,Year),~./(COL+OIL+GAS+NUC+HYD+GEO+BMS+PV+WIN)),
         COLX=0,OILX=0,GASX=0,BMSX=0) %>% 
  pivot_longer(cols=-c(Sector,Year),names_to='PRM',values_to='SHR_SEC') %>% 
  mutate(SEC='ELE') %>% 
  select(Year,PRM,SEC,SHR_SEC) %>% 
  bind_rows(foreach(i=2010:2020,.combine=rbind) %do%
              data.frame(Year=i,PRM=c('COL','OIL','GAS','BMS'),SEC=c('COL','OIL','GAS','BMS'),SHR_SEC=1))

# SSP2 indicator -GDP
SSP2_GDP <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>% 
  rename(R=1,Sv=2,Year=3,GDP=4) %>% 
  filter(R=='JPN') %>% select(-R) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year%in%2010:2050,Sv=='GDP_MER') %>% 
  select(-Sv)

# SSP2 indicator -Population
SSP2_POP <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>% 
  rename(R=1,Sv=2,Year=3,POP=4) %>% 
  filter(R=='JPN') %>% select(-R) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year%in%2010:2050,Sv=='POP') %>% 
  select(-Sv)

# SSP2 indicator -Commercial floor space
SSP2_COMFLOOR <- rgdx.param(paste0(ddir,'SSP2_JPN.gdx'),'ind_t') %>%  # billion sqm
  select(-1) %>% rename(Sv=1,Year=2,COMFLOOR=ind_t) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Sv=='Commercial_floor_space',Year<=2050) %>% 
  select(-Sv)

# SSP2 indicator -Residential floor space
SSP2_RESFLOOR <- rgdx.param(paste0(ddir,'SSP2_JPN.gdx'),'ind_t') %>%  # billion sqm
  select(-1) %>% rename(Sv=1,Year=2,RESFLOOR=ind_t) %>%
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Sv=='Residential_floor_space',Year<=2050) %>% 
  select(-Sv)


### Future energy demand estimation ###

# Industry sector ---------------------------------------------------------

# IEA Energy Balance -Industry final energy consumption
IEA_EB_IND <- IEA_EB %>% 
  filter(Sector=='Industry')

# For output
output_IND <- full_join(IEA_EB_IND,SSP2_GDP) %>% 
  replace_na(list(Sector='Industry')) %>% 
  mutate(intensity=value/GDP) %>% 
  select(-value,-GDP) %>% 
  drop_na()

# IEA Energy Balance -Blast furnaces
output_IND_BF <- IEA_EB %>% 
  filter(Sector%in%c('Blast furnaces','Coke ovens')) %>% 
  group_by(Year) %>% 
  summarise(value=sum(value)) %>% 
  full_join(SSP2_GDP) %>% 
  mutate(intensity=-value/GDP) %>% 
  select(-value,-GDP) %>% 
  drop_na()

# Transport sector --------------------------------------------------------

# IEA Energy Balance -Transport final energy consumption
IEA_EB_TRA <- IEA_EB %>% 
  filter(Sector=='Transport')

# For output
output_TRA <- full_join(IEA_EB_TRA,SSP2_POP) %>% 
  replace_na(list(Sector='Transport')) %>% 
  mutate(intensity=value/POP) %>% 
  select(-value,-POP) %>% 
  drop_na()


# Commercial sector -------------------------------------------------------

# IEA Energy Balance -Commercial final energy consumption
IEA_EB_COM <- IEA_EB %>% 
  filter(Sector=='Commercial and public services')

# For output
output_COM <- full_join(IEA_EB_COM,SSP2_GDP) %>% 
  full_join(SSP2_COMFLOOR) %>% 
  replace_na(list(Sector='Commercial and public services')) %>% 
  transmute(Sector,Year,intensity=value/GDP/COMFLOOR) %>% 
  drop_na()


# Residential sector ------------------------------------------------------

# IEA Energy Balance -Residential final energy consumption
IEA_EB_RES <- IEA_EB %>% 
  filter(Sector=='Residential')

# For output
output_RES <- full_join(IEA_EB_RES,SSP2_POP) %>% 
  replace_na(list(Sector='Residential')) %>% 
  transmute(Sector,Year,intensity=value/POP) %>% 
  drop_na()


### Energy sector ###

# Final energy ------------------------------------------------------------

# Define
DEF_FIN <- data.frame(FIN=c('COL','OIL','GAS','BMS','ELE'),
                      SEC=c('COL','OIL','GAS','BMS','ELE')) %>% 
  mutate(FIN=factor(FIN,levels=c('COL','OIL','GAS','BMS','ELE')),
         SEC=factor(SEC,levels=c('COL','OIL','GAS','BMS','ELE')))

# Historical energy share
SHR_HIS <- IEA_EB_SHR %>% 
  select(Sector,Year,FIN,SHR_FIN) %>% 
  pivot_wider(names_from = FIN, values_from = SHR_FIN)


# Distribution loss -------------------------------------------------------

output_LOSS <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020) %>% 
  select(Sector,Year,Electricity) %>% rename(value=Electricity) %>% 
  filter(Sector%in%c('Statistical differences',
                     'Main activity producer electricity plants',
                     'Autoproducer electricity plants',
                     'Electric boilers',
                     'Energy industry own use',
                     'Losses')) %>% 
  pivot_wider(names_from=Sector,values_from=value) %>% 
  mutate(TOTAL_GEN=`Statistical differences`+`Main activity producer electricity plants`+`Autoproducer electricity plants`,
         TOTAL_LOSS=`Electric boilers`+`Energy industry own use`+`Losses`) %>% 
  transmute(Year,ELE=(TOTAL_GEN+TOTAL_LOSS)/TOTAL_GEN)


# Power generation efficiency ---------------------------------------------

GEN_EFF <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020,Sector=='Memo: Efficiency of electricity only plants (main and auto) (%)') %>% 
  select(2,3,6,8:11,13) %>% 
  rename(COL=2,OIL=3,GAS=4,NUC=5,HYD=6,GEO=7,BMS=8) %>% 
  mutate(HYD=100,WIN=100,PV=100,GEO=100,COLX=COL,OILX=OIL,GASX=GAS,BMSX=BMS) %>% 
  complete(Year=2010:2050) %>% 
  mutate(across(-Year,~na_locf(.))) %>% 
  pivot_longer(cols=-Year,names_to='PRM',values_to='GEN_EFF') %>% 
  mutate(GEN_EFF=GEN_EFF/100,SEC='ELE')

output_GENEFF <- GEN_EFF %>% 
  select(-SEC) %>%
  mutate(PRM=factor(PRM,levels=c('COL','COLX','OIL','OILX','GAS','GASX',
                                 'NUC','BMS','BMSX','HYD','GEO','WIN','PV'))) %>% 
  arrange(PRM) %>% 
  pivot_wider(names_from=PRM,values_from=GEN_EFF)


# Emission factor ---------------------------------------------------------

EMF_PRM <- data.frame(PRM=c('COL','COLX','OIL','OILX','GAS','GASX',
                            'NUC','BMS','BMSX','HYD','GEO','WIN','PV'),
                      EMF=c(94.6,94.6*0.05,77.4,77.4*0.05,56.1,56.1*0.05,0,0,-100*0.95,0,0,0,0)) %>%  # !endogenous parameter
  pivot_wider(names_from=PRM,values_from=EMF)


# Emission  ---------------------------------------------------------------

### Non-energy sector ###

# Cement sector
EMI_CEM <- rgdx.param(paste0(ddir,'emissions_JPN.gdx'),'emi_jpn') %>% 
  filter(VAR=='Emi_CO2_Ene_Dem_Ind_Cem') %>% 
  select(Y,emi_jpn) %>% 
  rename(Year=Y,EMI_CEM=emi_jpn) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  full_join(SSP2_POP) %>% 
  filter(Year>=2010) %>%
  mutate(intensity=EMI_CEM/POP) %>%
  select(-EMI_CEM) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(EMI_CEM=POP*intensity)

output_CEM <- EMI_CEM <- rgdx.param(paste0(ddir,'emissions_JPN.gdx'),'emi_jpn') %>% 
  filter(VAR=='Emi_CO2_Ene_Dem_Ind_Cem') %>% 
  select(Y,emi_jpn) %>% 
  rename(Year=Y,EMI_CEM=emi_jpn) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  full_join(SSP2_POP) %>% 
  filter(Year>=2010) %>%
  mutate(intensity=EMI_CEM/POP) %>%
  select(-EMI_CEM,-POP) %>% 
  drop_na()

# Waste sector
EMI_WASTE0 <- read_xlsx(paste0(ddir,'L5-7gas_2022_gioweb_ver1.1.xlsx'),sheet=4) %>% 
  slice(3,55) %>% 
  select(-c(1:10)) 
colnames(EMI_WASTE0) <- EMI_WASTE0[1,]
EMI_WASTE <- EMI_WASTE0 %>% 
  slice(-1) %>% select(1:31) %>% 
  pivot_longer(cols=everything(),names_to='Year',values_to='EMI_WASTE',names_transform=as.numeric) %>% 
  full_join(SSP2_POP) %>% 
  filter(Year>=2010) %>%
  mutate(intensity=EMI_WASTE/POP) %>%
  select(-EMI_WASTE) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(EMI_WASTE=POP*intensity)

output_WASTE <- EMI_WASTE0 %>% 
  slice(-1) %>% select(1:31) %>% 
  pivot_longer(cols=everything(),names_to='Year',values_to='EMI_WASTE',names_transform=as.numeric) %>% 
  full_join(SSP2_POP) %>% 
  filter(Year>=2010) %>%
  mutate(intensity=EMI_WASTE/POP) %>%
  select(-EMI_WASTE,-POP) %>% 
  drop_na()

# LULUCF sector
EMI_LULUCF <- data.frame(Year=2010:2050,EMI_LULUCF=-46.0)  # from GIO. value of 2020. Mt-CO2/yr


### Power generation cost (LCOE) ###

output_LCOE <- read_xlsx(paste0(ddir,'cost_wg_20210908_03.xlsx'),sheet=1) %>% # NEDO,2021. exclude social and policy cost (eg. carbon price)
  slice(11:33) %>% select(1,19:21) %>%
  rename(PRM=1,CAPEX=2,OPEX=3,FUEL=4) %>% 
  mutate(across(2:4,~as.numeric(.))) %>% 
  mutate(value=CAPEX+OPEX+FUEL) %>% 
  select(PRM,value) %>% 
  mutate(PRM=recode(PRM,'原子力'='NUC',
                    '石炭火力'='COL',
                    'LNG火力'='GAS',
                    '石油火力'='OIL',
                    '太陽光（事業用）'='PV',
                    '陸上風力'='WIN',
                    '中水力'='HYD',
                    '地熱'='GEO',
                    'バイオマス（木質専焼）'='BMS',
                    'CO2分離回収型石炭火力'='COLX',
                    'CO2分離回収型LNG火力'='GASX')) %>% 
  filter(PRM%in%c('NUC','COL','GAS','OIL','PV','WIN','HYD','GEO','BMS','COLX','GASX')) %>% 
  mutate(value=case_when(PRM=='COLX'~value+2.1,
                         PRM=='GASX'~value+0.9,
                         TRUE~value)) %>%  # add CO2 transport and storage cost. NEDO,2021
  bind_rows(data.frame(PRM=c('OILX','BMSX'),
                       value=c(21.1+(11.3-8.5)*77.4/56.1,28.1+(11.3-8.5)*100/56.1))) %>% 
  mutate(PRM=factor(PRM,levels=c('COL','COLX','OIL','OILX','GAS','GASX',
                                 'NUC','BMS','BMSX','HYD','GEO','WIN','PV'))) %>% 
  arrange(PRM)


### Energy Price ###  

output_EPT <- read_csv(paste0(ddir,'IEA_EPT_JP.csv')) %>% # IEA Energy Price and Tax. unit:JPY/unit
  slice(34,44) %>% select(1,4,12,15) %>% mutate(across(everything(),~as.numeric(.))) %>%
  rename(Year=1,OIL=2,GAS=3,ELE=4) %>% 
  mutate(OIL=OIL/1000/8718/4.2*10^6,GAS=GAS/3.6,ELE=ELE/3.6) %>%  # unit conversion to JPY/GJ
  mutate(COL=)

# output ------------------------------------------------------------------

SSP2_OUT <- list(GDP=SSP2_GDP,POP=SSP2_POP,COMFLOOR=SSP2_COMFLOOR,
                 iIND=output_IND,iTRA=output_TRA,iCOM=output_COM,iRES=output_RES,
                 LOSS=output_LOSS,
                 sIND=filter(SHR_HIS,Sector=='Industry'),
                 sTRA=filter(SHR_HIS,Sector=='Transport'),
                 sCOM=filter(SHR_HIS,Sector=='Commercial and public services'),
                 sRES=filter(SHR_HIS,Sector=='Residential'),
                 sELE=IEA_EB_SHR_ELE,
                 eELE=output_GENEFF,
                 EMF=EMF_PRM,
                 EMI_CEM=output_CEM,
                 EMI_WASTE=output_WASTE,
                 EMI_LULUCF=EMI_LULUCF,
                 IND_BF=output_IND_BF,
                 LCOE=output_LCOE)
write.xlsx(SSP2_OUT, file = paste0(xdir,'data.xlsx'))

