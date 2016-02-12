#===================================================================
# Predicting Prices 
# Juliana Herrera
# data =  prices.xlsx - time series from 1990 until 2013 of prices of "pargo", "pargo seda" and "pargo pequeno", per region and month in Costa Rica. 
#===================================================================
#==PART 1===========================================================
# loading packages
library(readr)
library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)

#==PART 2===========================================================
# Loading data from excel and correction for inflation based on the Consumer Price Index (CPI)
#===================================================================

data <- read_excel('prices.xlsx') 
colnames(data) <- tolower(colnames(data))
CPI <- read_excel('CPI2.xlsx') # Consumer Price Index - source: World Bank
CPIm <- read_excel('CPI.xlsx') # Consumer Price Index by month : World Bank
# Biomass <- read_csv()
 
yrprices <- (data) %>%
  select(-fishery, -spp) %>%
  group_by(year) %>% 
  summarize(meanprice = mean(price, na.rm=T)) %>% 
  bind_cols(CPI) %>% 
  mutate(corrected = meanprice/cpi*cpi[24])
head(yrprices) # corrected prices for inflation. Baseline year = 2013

p <-ggplot(yrprices) + ## Graph of corrected prices per year
  geom_line(aes(x=year, y=meanprice, colour = 'meanprice')) +
  geom_line(aes(x=year, y=corrected, colour ='corrected')) +
  theme_bw()
p

monthprices <- (data) %>%
  select(-fishery, -spp) %>%
  group_by(month, year) %>% 
  summarize(meanprice = mean(price, na.rm=T)) %>% 
  bind_cols(CPIm) %>% 
  mutate(corrected = meanprice/cpi*cpi[289])
head(monthprices) # corrected prices for inflation. Baseline year = 2013

# pm<- ggplot(monthprices) %>% # graph for corrected prices per month
#   geom_line(aes(x=year, y=meanprice, colour='meanprice')) +
#   geom_line(aes(x=year, y=corrected, colour='corrected')) +
#   theme_bw()
# pm

#==PART ===========================================================

#==PART ===========================================================
# Projection of prices - regression of price as a function of catch 



