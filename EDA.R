library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(MASS)
library(pscl)
library(janitor)

# Import dataset
agri_rural_raw <- read_excel("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Data/Agriculture & Rural Development.xls")

#Data Clean
colnames(agri_rural_raw) <- as.character(unlist(agri_rural_raw[3, ]))
agri_rural_clean <- agri_rural_raw[-c(1:3), ]
agri_rural_clean <- agri_rural_clean %>%
  janitor::clean_names()

agri_philippines <- agri_rural_clean %>%
  filter(country_name == "Philippines")

##EDA 

#1. rural population and growth rate: 

#2. agriculture employement total vs men vs female 

#3. Agriculture, forestry, and fishing, value added (% of GDP and US$)

#4. Cerial Yeild Kg/hectare 

#5. food vs crop vs livestock index 

#6. Average precipitation in depth 

#7. Annual freshwater withdrawls & arable land 

#8. Agriculture land (%land area) 

#9. Agriculture GDP share 

#10. Agriculture raw material import 

#11. Rural population vs rural access to electricity 

#12. Fertiliser concumption 



#Import dataset 
climate_rain <- read_excel("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Data/Natural disasters - climate and rainfall.xlsx")

#Data clean
climate_rain_philippines <- climate_rain %>%
  filter(Country == "Philippines")

#1. Disaster type / subtype

#2. location -- Philipines heatmap 

#3. start and end dates 

#4. magnitude of disaster 

#5. Total affected 

#6. Total deaths 

#7. start / end months (frequency)

#8. total damage (CPI adjusted)

#9. total damage vs insurance damage = protection gap (severity)

#10. Reconstruction cost (severity)

#Import data
temp_series <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Data/Observed Weather Time Series.csv")

precip_20mm <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Days_with_Precipitation_over_20mm.csv")
precip_20mm_philippines <- precip_20mm %>%
  filter(REF_AREA_LABEL == "Philippines")


agri_govt_exp <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/The agriculture orientation index for government expenditures.csv")
agri_govt_exp_phl <- agri_govt_exp %>%
  filter(REF_AREA_LABEL == "Philippines")

official_flows_agri <- read.csv("/Users/anuvanadkar/Documents/GitHub/ACTL4002/Total official flows official development assistance plus other official flows to the agriculture sector 2000-2023 Millions US dollars 2022 constant prices.csv")
official_flows_agri_phl <- official_flows_agri %>%
  filter(REF_AREA_LABEL == "Philippines")