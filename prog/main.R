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
IEA_EB <- IEA_EB_IND <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Sector=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Year%in%2010:2020) %>% 
  select(Sector,Year,Total) %>% rename(value=Total) %>% 
  complete(Year=2010:2050)

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

#  Future extension
df_IND <- full_join(IEA_EB_IND,SSP2_GDP) %>% 
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
  transmute(Year,value=value/COMFLOOR) %>% 
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



