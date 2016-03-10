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
library(lubridate)

#==PART 2===========================================================
# Loading data from excel and correction for inflation based on the Consumer Price Index (CPI)
#===================================================================

data <- read_excel('prices.xlsx') 
colnames(data) <- tolower(colnames(data))
CPI <- read_excel('CPI2.xlsx') # Consumer Price Index - source: World Bank
CPIm <- read_excel('CPI.xlsx') # Consumer Price Index by month : World Bank
catch <- read_excel('/Users/julianaherrera/Documents/UCSB/Winter 2016/Informatics/fishmgt.github.io/fishmgt.github.io/data/For CRSeafood code/catches.xlsx') %>%
  gather('month','catch',Jan:Dec) %>%
  select(-catch, -month) %>% 
  filter(Region == 'Guanacaste', Species == c('Pargo', 'Pargo Seda')) %>% 
  group_by(Region, Year) %>% 
  summarise(sum=sum(Total)) # total catch of pargo and pargo seda per region and year
colnames(catch) <- tolower(colnames(catch))
 
yrprices <- (data) %>%
  select(-fishery, -spp) %>%
  group_by(year) %>% 
  summarize(meanprice = mean(price, na.rm=T)) %>% 
  left_join(CPI, by = 'year') %>% 
  mutate(corrected = meanprice/cpi*cpi[24])
head(yrprices) # corrected prices for inflation. Baseline year = 2013

# plot for corrected prices and mean prices per year
p <-ggplot(yrprices) + 
  geom_line(aes(x=year, y=meanprice, colour = 'meanprice')) +
  geom_line(aes(x=year, y=corrected, colour ='corrected')) +
  theme_bw()
p

#regression for prices and catches by year
yreg <-yrprices %>% 
  left_join(catch, by = 'year') %>% 
  select(year, corrected, sum) %>% 
  as.numeric()
 
plot(yreg$sum, yreg$meanprice)
yreg_lm <- lm(log(yreg$meanprice) ~ log(yreg$sum)) # y = mx + c ---> y = 2.097x10^3 x - 1.202x10^-5 p>0.05 therefore catch is not good predictor of prices. p = 0.88, linear model doesn't describe correctly the data. 
yreg_lm
summary(yreg_lm)


library(lubridate)

catchmonth <- read_excel('/Users/julianaherrera/Documents/UCSB/Winter 2016/Informatics/fishmgt.github.io/fishmgt.github.io/data/For CRSeafood code/catches.xlsx') %>%
  gather('month','catch',Jan:Dec) %>%
  filter(Region == 'Guanacaste', Species == c('Pargo', 'Pargo Seda')) %>% 
  select(-catch, -Species) %>% 
  group_by(Year, month) %>% 
  summarise(
    sumcatch=sum(Total, na.rm=T)) %>% # total catch of pargo and pargo seda per region and month
  ungroup() %>% 
  mutate(ym = ymd(sprintf('%d-%s-01', year, month))) %>%
  arrange(ym)
colnames(catchmonth) <- tolower(colnames(catch))


prices_ym <- data %>%
  select(-fishery, -spp) %>%
  group_by(year, month) %>%
  summarise(
    meanprice = mean(price, na.rm=T)) %>%
  ungroup() %>%
  mutate(
    ym = ymd(sprintf('%d-%s-01', year, month))) %>%
  arrange(ym) %>% 
  left_join(CPI, by = 'year') %>% 
  mutate(corrected = meanprice/cpi*cpi[288]) # corrected prices for inflation. Baseline year = 2013

pm<- ggplot(prices_ym) %>% # graph for corrected prices per month
  geom_line(aes(x=year, y=meanprice, colour='meanprice')) +
  geom_line(aes(x=year, y=corrected, colour='corrected')) +
  theme_bw()
pm

#==PART ===========================================================

#==PART ===========================================================
# Projection of prices - regression of price as a function of catch 



