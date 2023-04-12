# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(gdxrrw)
library(imputeTS)


# Setting -----------------------------------------------------------------

root <- getwd()
ddir <- paste0(root,'/data/')
odir <- paste0(root,'/output/')


# Input data --------------------------------------------------------------

# IEA Energy Balance
IEA_EB <- read_csv(paste0(ddir,'IEA_EB_JP.csv')) %>% 
  slice(-1) %>% rename(Flow=1,Year=2) %>% 
  mutate(across(-1,~as.numeric(.))) %>% 
  filter(Flow=='Industry',Year%in%2005:2020) %>%
  select(Year,Total) %>% rename(value=Total) %>% 
  complete(Year=2005:2050)

# # IEA Energy Balance indicator
# IEA_IND <- read_csv(paste0(ddir,'IEA_EB_IND.csv')) %>% 
#   slice(-1) %>% rename(Flow=1) %>% select(-`2021`) %>% 
#   filter(Flow%in%c('Industry/GDP (toe per thousand 2015 USD)')) %>% 
#   pivot_longer(cols=-c(Flow),names_to='Year',values_to='value',names_transform=as.numeric) %>% 
#   pivot_wider(names_from=Flow,values_from=value) %>% 
#   rename(Ind_GDP=2) %>% filter(Year>=2005) %>% 
#   complete(Year=2005:2050) %>% 
#   mutate(Ind_GDP=na_locf(Ind_GDP))
  
# SSP2 indicator
SSP2 <- rgdx.param(paste0(ddir,'serv_global_SSP2.gdx'),'ind_t') %>% 
  rename(R=1,Sv=2,Year=3,ind=4) %>% 
  filter(R=='JPN') %>% select(-R) %>% 
  mutate(across(where(is.factor),~as.character(.))) %>% 
  mutate(Year=as.numeric(Year)) %>% 
  filter(Year%in%2005:2050,Sv=='GDP_MER') %>% 
  select(-Sv)

#  GDP and Energy demand regression
df_reg <- full_join(IEA_EB,SSP2) %>% 
  mutate(intensity=value/ind) %>% 
  select(-value) %>% 
  mutate(intensity=na_locf(intensity)) %>% 
  mutate(value_Lv1=ind*intensity) %>% 
  mutate(intensity_Lv2=case_when(Year%in%2005:2020 ~ intensity,
                             Year==2050 ~ intensity*0.8),
         intensity_Lv3=case_when(Year%in%2005:2020 ~ intensity,
                             Year==2050 ~ intensity*0.5)) %>% 
  mutate(intensity_Lv2=na_interpolation(intensity_Lv2),
         intensity_Lv3=na_interpolation(intensity_Lv3)) %>% 
  mutate(value_Lv2=ind*intensity_Lv2,
         value_Lv3=ind*intensity_Lv3) %>% 
  select(-starts_with('intensity_')) %>% 
  pivot_longer(cols=c(value_Lv1,value_Lv2,value_Lv3),names_to='Sv',values_to='value',names_prefix='value_') %>% 
  select(-ind,-intensity)


# Plot --------------------------------------------------------------------

g <- df_reg %>% 
  ggplot() +
  geom_line(aes(x=Year,y=value,color=Sv)) +
  scale_y_continuous(limits=c(0,NA))
plot(g)

