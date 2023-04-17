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


# Setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/')
odir <- paste0(root,'/output/')


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

# Industry Final Energy Consumption in TJ
df_IND <- full_join(IEA_EB_IND,SSP2_GDP) %>% 
  replace_na(list(Sector='Industry')) %>% 
  mutate(intensity=value/GDP) %>% 
  select(-value) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(value_Lv1=GDP*intensity) %>% 
  mutate(intensity_Lv2=case_when(Year%in%2010:2020 ~ intensity,
                             Year==2050 ~ intensity*0.8),
         intensity_Lv3=case_when(Year%in%2010:2020 ~ intensity,
                             Year==2050 ~ intensity*0.5)) %>% 
  mutate(intensity_Lv2=na_interpolation(intensity_Lv2),
         intensity_Lv3=na_interpolation(intensity_Lv3)) %>% 
  mutate(value_Lv2=GDP*intensity_Lv2,
         value_Lv3=GDP*intensity_Lv3) %>% 
  select(-starts_with('intensity_')) %>% 
  pivot_longer(cols=c(value_Lv1,value_Lv2,value_Lv3),names_to='Sv',values_to='value',names_prefix='value_') %>% 
  select(-GDP,-intensity)

# Plot
g <- df_IND %>% 
  ggplot() +
  geom_line(aes(x=Year,y=value,color=Sv)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)



# Transport sector --------------------------------------------------------

# IEA Energy Balance -Transport final energy consumption
IEA_EB_TRA <- IEA_EB %>% 
  filter(Sector=='Transport')

#  Future extension
df_TRA <- full_join(IEA_EB_TRA,SSP2_POP) %>%
  replace_na(list(Sector='Transport')) %>%
  mutate(intensity=value/POP) %>% 
  select(-value) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(value_Lv1=POP*intensity) %>% 
  mutate(intensity_Lv2=case_when(Year%in%2010:2020 ~ intensity,
                                 Year==2050 ~ intensity*0.8),
         intensity_Lv3=case_when(Year%in%2010:2020 ~ intensity,
                                 Year==2050 ~ intensity*0.5)) %>% 
  mutate(intensity_Lv2=na_interpolation(intensity_Lv2),
         intensity_Lv3=na_interpolation(intensity_Lv3)) %>% 
  mutate(value_Lv2=POP*intensity_Lv2,
         value_Lv3=POP*intensity_Lv3) %>% 
  select(-starts_with('intensity_')) %>% 
  pivot_longer(cols=c(value_Lv1,value_Lv2,value_Lv3),names_to='Sv',values_to='value',names_prefix='value_') %>% 
  select(-POP,-intensity)

# Plot
g <- df_TRA %>% 
  ggplot() +
  geom_line(aes(x=Year,y=value,color=Sv)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)


# Commercial sector -------------------------------------------------------

# IEA Energy Balance -Commercial final energy consumption
IEA_EB_COM <- IEA_EB %>% 
  filter(Sector=='Commercial and public services')

# Future extension
df_COM <- full_join(IEA_EB_COM,SSP2_COMFLOOR) %>%
  replace_na(list(Sector='Commercial and public services')) %>%
  transmute(Sector,Year,value=value/COMFLOOR) %>% 
  full_join(SSP2_GDP) %>% 
  mutate(intensity=value/GDP) %>% 
  select(-value) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(value_Lv1=GDP*intensity) %>% 
  mutate(intensity_Lv2=case_when(Year%in%2010:2020 ~ intensity,
                                 Year==2050 ~ intensity*0.8),
         intensity_Lv3=case_when(Year%in%2010:2020 ~ intensity,
                                 Year==2050 ~ intensity*0.5)) %>% 
  mutate(intensity_Lv2=na_interpolation(intensity_Lv2),
         intensity_Lv3=na_interpolation(intensity_Lv3)) %>% 
  mutate(value_Lv2=GDP*intensity_Lv2,
         value_Lv3=GDP*intensity_Lv3) %>% 
  select(-starts_with('intensity_')) %>% 
  pivot_longer(cols=c(value_Lv1,value_Lv2,value_Lv3),names_to='Sv',values_to='value',names_prefix='value_') %>% 
  select(-GDP,-intensity)
  
# Plot
g <- df_COM %>% 
  ggplot() +
  geom_line(aes(x=Year,y=value,color=Sv)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)


# Residential sector ------------------------------------------------------

# IEA Energy Balance -Residential final energy consumption
IEA_EB_RES <- IEA_EB %>% 
  filter(Sector=='Residential')

# Future extension
df_RES <- full_join(IEA_EB_RES,SSP2_RESFLOOR) %>% 
  replace_na(list(Sector='Residential')) %>%
  mutate(intensity=value/RESFLOOR) %>% 
  select(-value) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(value_Lv1=RESFLOOR*intensity) %>% 
  mutate(intensity_Lv2=case_when(Year%in%2010:2020 ~ intensity,
                                 Year==2050 ~ intensity*0.8),
         intensity_Lv3=case_when(Year%in%2010:2020 ~ intensity,
                                 Year==2050 ~ intensity*0.5)) %>% 
  mutate(intensity_Lv2=na_interpolation(intensity_Lv2),
         intensity_Lv3=na_interpolation(intensity_Lv3)) %>% 
  mutate(value_Lv2=RESFLOOR*intensity_Lv2,
         value_Lv3=RESFLOOR*intensity_Lv3) %>% 
  select(-starts_with('intensity_')) %>% 
  pivot_longer(cols=c(value_Lv1,value_Lv2,value_Lv3),names_to='Sv',values_to='value',names_prefix='value_') %>% 
  select(-RESFLOOR,-intensity)

# Plot
g <- df_RES %>% 
  ggplot() +
  geom_line(aes(x=Year,y=value,color=Sv)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)


# Summarise ---------------------------------------------------------------

IEA_EB_DEM <- bind_rows(df_IND,df_TRA,df_COM,df_RES)



### Energy sector ###

# Final energy ------------------------------------------------------------

# Define
DEF_FIN <- data.frame(FIN=c('COL','OIL','GAS','BMS','ELE'),
                      SEC=c('COL','OIL','GAS','BMS','ELE')) %>% 
  mutate(FIN=factor(FIN,levels=c('COL','OIL','GAS','BMS','ELE')),
         SEC=factor(SEC,levels=c('COL','OIL','GAS','BMS','ELE')))

# Historical energy share
SHR_HIS <- IEA_EB_SHR %>% 
  mutate(SEC=FIN) %>% 
  select(Sector,Year,FIN,SEC,SHR_FIN)

# Share in 2030
SHR_IND_2030 <- data.frame(Sector='Industry',Year=2030,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_TRA_2030 <- data.frame(Sector='Transport',Year=2030,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_COM_2030 <- data.frame(Sector='Residential',Year=2030,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_RES_2030 <- data.frame(Sector='Commercial and public services',Year=2030,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_2030 <- bind_rows(SHR_IND_2030,SHR_TRA_2030,SHR_COM_2030,SHR_RES_2030)

# Share in 2050
SHR_IND_2050 <- data.frame(Sector='Industry',Year=2050,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_TRA_2050 <- data.frame(Sector='Transport',Year=2050,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_COM_2050 <- data.frame(Sector='Residential',Year=2050,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_RES_2050 <- data.frame(Sector='Commercial and public services',Year=2050,DEF_FIN,SHR_FIN=c(0.2,0.2,0.4,0.1,0.1)) # ! exogenous parameter
SHR_2050 <- bind_rows(SHR_IND_2050,SHR_TRA_2050,SHR_COM_2050,SHR_RES_2050)

# Interpolation share
SHR_FIN <- bind_rows(SHR_HIS,SHR_2030,SHR_2050) %>% 
  group_by(Sector,FIN,SEC) %>% 
  complete(Year=c(2010:2050)) %>% 
  mutate(SHR_FIN=na_interpolation(SHR_FIN))

# Example -industry sector
FIN_IND <- df_IND %>% 
  full_join(SHR_FIN %>% filter(Sector=='Industry')) %>% 
  mutate(FEC=value*SHR_FIN) %>% 
  select(-value,-SHR_FIN)

g <- FIN_IND %>% 
  ggplot() +
  geom_area(aes(x=Year,y=FEC,fill=FIN)) +
  scale_fill_brewer(palette='Set3') +
  facet_wrap(vars(Sv))
plot(g)


# Distribution loss -------------------------------------------------------

DIS_LOSS <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
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
  transmute(Year,ELE=(TOTAL_GEN+TOTAL_LOSS)/TOTAL_GEN) %>% 
  mutate(COL=1,OIL=1,GAS=1,BMS=1) %>% 
  complete(Year=2010:2050) %>% 
  mutate(across(-Year,~na_locf(.))) %>% 
  pivot_longer(cols=-Year,names_to='FIN',values_to='DIS_LOSS')


# Power generation --------------------------------------------------------

# Historical energy share
SHR_HIS <- IEA_EB_SHR %>% 
  mutate(SEC=FIN) %>% 
  select(Sector,Year,FIN,SEC,SHR_FIN)

# Share in 2030
SHR_SEC_2030 <- data.frame(Year=2030,
                      PRM=c('COL','COLX','OIL','OILX','GAS','GASX',
                            'NUC','BMS','BMSX','HYD','GEO','WIN','PV'),
                      SEC=c('ELE'),
                      SHR_SEC=c(0.1,0,0.1,0,0.3,0,0.1,0,0,0.1,0,0.2,0.1)) %>%   # ! exogenous parameter
  bind_rows(data.frame(Year=2030,
                       PRM=c('COL','OIL','GAS','BMS'),
                       SEC=c('COL','OIL','GAS','BMS'),
                       SHR_SEC=1))

# Share in 2050
SHR_SEC_2050 <- data.frame(Year=2050,
                      PRM=c('COL','COLX','OIL','OILX','GAS','GASX',
                            'NUC','BMS','BMSX','HYD','GEO','WIN','PV'),
                      SEC=c('ELE'),
                      SHR_SEC=c(0.1,0,0.1,0,0.3,0,0.1,0,0,0.1,0,0.2,0.1)) %>%   # ! exogenous parameter
  bind_rows(data.frame(Year=2050,
                       PRM=c('COL','OIL','GAS','BMS'),
                       SEC=c('COL','OIL','GAS','BMS'),
                       SHR_SEC=1))

# Interpolation share
SHR_SEC <- bind_rows(IEA_EB_SHR_ELE,SHR_SEC_2030,SHR_SEC_2050) %>% 
  group_by(PRM,SEC) %>% 
  complete(Year=c(2010:2050)) %>% 
  mutate(SHR_SEC=na_interpolation(SHR_SEC))


# Example -industry sector
SEC_IND <- FIN_IND %>% 
  left_join(DIS_LOSS) %>% 
  left_join(DEF_SEC) %>% 
  mutate(SEP=FEC/DIS_LOSS*SHR_SEC)
  

# Power generation efficiency ---------------------------------------------

GEN_EFF <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020,Sector=='Memo: Efficiency of electricity only plants (main and auto) (%)') %>% 
  select(2,3,6,8:11,13) %>% 
  rename(COL=2,OIL=3,GAS=4,NUC=5,HYD=6,GEO=7,BMS=8) %>% 
  mutate(HYD=1,WIN=1,PV=1,COLX=COL,OILX=OIL,GASX=GAS,BMSX=BMS) %>% 
  complete(Year=2010:2050) %>% 
  mutate(across(-Year,~na_locf(.))) %>% 
  pivot_longer(cols=-Year,names_to='PRM',values_to='GEN_EFF') %>% 
  mutate(GEN_EFF=GEN_EFF/100,SEC='ELE')

OTH_EFF <- data.frame(PRM=c('COL','OIL','GAS','BMS'),SEC=c('COL','OIL','GAS','BMS'),Year=2010) %>% 
  group_by(PRM,SEC) %>% 
  complete(Year=2010:2050) %>% 
  mutate(GEN_EFF=1)

PRM_EFF <- bind_rows(GEN_EFF,OTH_EFF)


# Primary energy supply ---------------------------------------------------

# Example -industry sector
PRM_IND <- SEC_IND %>% 
  left_join(PRM_EFF) %>% 
  mutate(PES=SEP/GEN_EFF)


# Emission factor ---------------------------------------------------------

EMF_PRM <- data.frame(PRM=c('COL','COLX','OIL','OILX','GAS','GASX',
                            'NUC','BMS','BMSX','HYD','GEO','WIN','PV'),
                      EMF=c(94.6,94.6*0.05,77.4,77.4*0.05,56.1,56.1*0.05,0,-100*0.95,0,0,0,0,0)) # !endogenous parameter


# Emission  ---------------------------------------------------------------

# Example -industry sector
EMI_IND <- PRM_IND %>% 
  left_join(EMF_PRM) %>% 
  mutate(EMI=PES*EMF/1000)


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

# LULUCF sector
EMI_LULUCF <- data.frame(Year=2010:2050,EMI_LULUCF=-54.3) # from GIO. value of 2020. Mt-CO2/yr

