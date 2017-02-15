#rm(list=ls())

#Set working directory          
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Libraries
library(dplyr)
library(ggplot2)

#Read/load data

events <- read.csv('data/events.csv', as.is = T)
fiscal <- read.csv('data/fscl.csv', as.is = T)
hier <- read.csv('data/hier.csv')
catalog <- read.csv('data/pctlg_sku.csv', as.is = T)
load('data/price_type.rda')
tran <- read.csv('data/tran.csv', as.is = T)

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
    mutate(DATE = as.Date(DAY_DT)) %>%
    select(-DAY_DT)

#CATALOG

catalog <- catalog %>%
    mutate(CRT_TMSTP = as.Date(CRT_TMSTP),
           RCD_UPDT_TMSTP = as.POSIXct(RCD_UPDT_TMSTP))

#TRANSACTIONS

tran <- tran %>%
    mutate(TRAN_DT = as.Date(TRAN_DT),
           UNITS = as.numeric(UNITS),
           DEMAND = as.numeric(DEMAND),
           RTRN_UNITS = as.numeric(RTRN_UNITS),
           RTRN_AMT = as.numeric(RTRN_AMT))

n_demand_none = sum(is.na(tran$UNITS))

demand <- tran %>%
    filter(!is.na(UNITS)) %>%
    select(TRAN_DT, SKU_IDNT, UNITS, DEMAND)

#Analyze demand

demand_sku <- demand %>%
    group_by(SKU_IDNT) %>%
    summarize(total_units = n(), total_demand = sum(DEMAND))%>%
    arrange(-total_units)

summary(demand_sku)

#Join transactions with catalog

tran_cat <- demand %>%
    inner_join(catalog, by = "SKU_IDNT") %>%
    left_join(price_type, by = c("SKU_IDNT","DAY_DT"=="TRAN_DT"))

tran_cat_color <- tran_cat %>%
    group_by(SKU_IDNT) %>%
    summarize(total_units = n(), total_demand = sum(DEMAND))%>%
    arrange(-total_units)

