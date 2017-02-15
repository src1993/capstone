#rm(list=ls())

#Set working directory          
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

#Read/load data

events <- read.csv('data/events.csv', as.is = T)
fiscal <- read.csv('data/fscl.csv', as.is = T)
hier <- read.csv('data/hier.csv')
catalog <- read.csv('data/pctlg_sku.csv', as.is = T)
load('data/price_type.rda')
tran <- read.csv('data/tran.csv', as.is = T)

#tran_backup <- tran

#EVENTS

events <- events %>%
    mutate(start = as.Date(DT_BEG_ACT,
           format = "%m/%d/%Y"),
           end = as.Date(DT_END_ACT,
           format = "%m/%d/%Y"),
           duration = end-start+1) %>%
    select(-c(DT_BEG_ACT,DT_END_ACT))

#FISCAL

fiscal <- fiscal %>%
    mutate(DAY_DT = as.Date(DAY_DT)) 

#CATALOG

catalog <- catalog %>%
    mutate(CRT_TMSTP = as.Date(CRT_TMSTP),
           RCD_UPDT_TMSTP = as.POSIXct(RCD_UPDT_TMSTP))

#PRICE

price_type <- price_type %>%
    mutate(SKU_IDNT = as.integer(SKU_IDNT))

#TRANSACTIONS

tran <- tran %>%
    mutate(DAY_DT = as.Date(TRAN_DT),
           UNITS = as.numeric(UNITS),
           DEMAND = as.numeric(DEMAND),
           RTRN_UNITS = as.numeric(RTRN_UNITS),
           RTRN_AMT = as.numeric(RTRN_AMT)) %>%
    select(-TRAN_DT)

n_demand_none = sum(is.na(tran$UNITS))

demand <- tran %>%
    filter(!is.na(UNITS)) %>%
    select(DAY_DT, SKU_IDNT, UNITS, DEMAND)

#Analyze demand

demand_sku <- demand %>%
    group_by(SKU_IDNT) %>%
    summarize(total_units = sum(UNITS), total_demand = sum(DEMAND))%>%
    arrange(-total_units)

summary(demand_sku)

demand_date <- demand %>%
    group_by(DAY_DT) %>% 
    summarize(total_units = sum(UNITS), total_demand = sum(DEMAND))%>%
    arrange(-total_units)
  
summary(demand_date)  

ggplot(data = demand_date, aes(total_units)) + geom_histogram(bins=30)
ggplot(data = demand_date, aes(1,total_units)) + geom_boxplot()
ggplot(data = demand_date, aes(1,total_demand)) + geom_boxplot()
ggplot(data = demand_date, aes(total_demand)) + geom_histogram(bins=30)

#Agreggate by yearmonth

demand_date_month <- demand %>%
    group_by(DATE = as.yearmon(DAY_DT)) %>% 
    summarize(total_units = n(), total_demand = sum(DEMAND))%>%
    arrange(-total_units)

summary(demand_date_month)

ggplot(data = demand_date_month, aes(total_units)) + geom_histogram(bins = 5)
ggplot(data = demand_date_month, aes(1,total_units)) + geom_boxplot()
ggplot(data = demand_date, aes(1,total_demand)) + geom_boxplot()
ggplot(data = demand_date, aes(total_demand)) + geom_histogram(bins=30)

ggplot(data = demand_date_month, aes(DATE, total_units)) + geom_line()+
    scale_x_yearmon()
ggplot(data = demand_date_month, aes(DATE, total_demand)) + geom_line()+
    scale_x_yearmon()

demand_date_month_agg <- demand %>%
    group_by(month = month(DAY_DT)) %>% 
    summarize(total_units = mean(UNITS), total_demand = mean(DEMAND))%>%
    arrange(month)



#Join transactions with catalog

tran_cat <- demand %>%
    inner_join(catalog, by = "SKU_IDNT") %>%
    left_join(price_type)

tran_cat_color <- tran_cat %>%
    group_by(SKU_IDNT) %>%
    summarize(total_units = n(), total_demand = sum(DEMAND))%>%
    arrange(-total_units)

