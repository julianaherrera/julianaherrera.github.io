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
#===================================================================
# 
data <- read_excel('prices.xlsx') 
colnames(data) <- tolower(colnames(data))

prices <- (data) %>%
  gather()
  summary(mean())
head(prices)
