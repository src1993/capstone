#rm(list=ls())

#Set working directory          
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(rpart)
library(rpart.plot)
library(sqldf)
library(tidyr)
library(qcc)

####################################   Read/load data  ###############################################

events <- read.csv('data/events.csv', as.is = T)
fiscal <- read.csv('data/fscl.csv', as.is = T)
hier <- read.csv('data/hier.csv', as.is = T)
catalog <- read.csv('data/pctlg_sku.csv', as.is = T)
load('data/price_type.rda')
tran <- read.csv('data/tran.csv', as.is = T)

####################################  Cleaning/wrangling  ###############################################

#EVENTS

events <- events %>%
    mutate(EVENT = EVENT_NM, 
           start = as.Date(DT_BEG_ACT,
                           format = "%m/%d/%Y"),
           end = as.Date(DT_END_ACT,
                         format = "%m/%d/%Y"),
           duration = end-start+1,
           year = YR_454) %>%
    select(EVENT, start, end, duration, year)

#FISCAL

fiscal <- fiscal %>%
    mutate(DAY_DT = as.Date(DAY_DT),
           YEAR = YR_454) %>%
    select(-YR_454)

#HIERARCHY

hier <- hier %>%
    mutate(N_BRAND = ifelse(SUPP_NPG_CDE==1,1,0),
           LOAD_DT = as.Date(DM_RECD_LOAD_DT,
                           format = "%Y-%m-%d"),
           DEPT = as.factor(ifelse(DEPT_DESC=='CONTEMP HBG/SLG','Contemporary','Bridge')),
           CLOSE_DT = pmin(as.Date(max(DM_RECD_LOAD_DT)),as.Date(DM_RECD_CLOSE_DT)),
           LENGTH = as.numeric(CLOSE_DT-LOAD_DT+1)) %>%
    select(SKU_IDNT,SUPP_NAME, SUPP_NAME, N_BRAND, BRAND_NAME,
           CLASS_DESC, CLASS_IDNT, DEPT,LOAD_DT, CLOSE_DT, LENGTH)

#CATALOG

catalog <- catalog %>%
    mutate(ORIG_PRICE = ORIG_PRC_AMT,
           COLOR = gsub("TONES","",CLR_FMLY_DESC),
           CREATE_DT = as.Date(CRT_TMSTP)) %>%
    select(SKU_IDNT, ORIG_PRICE, COLOR,CREATE_DT)

#PRICE

price_type <- price_type %>%
    mutate(SKU_IDNT = as.integer(SKU_IDNT))

#TRANSACTIONS

tran <- tran %>%
    mutate(DAY_DT = as.Date(TRAN_DT),
           UNITS = as.numeric(UNITS),
           DEMAND = as.numeric(DEMAND),
           RTRN_UNITS = as.numeric(RTRN_UNITS),
           RTRN_AMT = as.numeric(RTRN_AMT),
           UNIT_PRICE_DEMAND =  ifelse(is.na(UNITS), 0, DEMAND/UNITS) ,
           UNIT_PRICE_RETURN = ifelse(is.na(RTRN_UNITS), 0, -RTRN_AMT/RTRN_UNITS)
           )%>%
    select(SKU_IDNT, DAY_DT, UNITS, DEMAND, UNIT_PRICE_DEMAND, RTRN_UNITS, RTRN_AMT,UNIT_PRICE_RETURN)
tran[is.na(tran)] <- 0


####################################  Create datasets ###############################################

#Create data frame of dates

DAY <- seq(as.Date(min(tran$DAY_DT)),as.Date(max(tran$DAY_DT)),by = "day")
calendar <- data.frame(DAY = DAY, dummy = TRUE)

day_events <- events %>% 
    mutate(dummy=TRUE) %>%
    right_join(calendar, by = "dummy") %>%
    filter(DAY >=start, DAY<=end) %>%
    select(DAY,EVENT)

# Create product description
description <- hier %>%
    full_join(catalog, by = "SKU_IDNT")

demand <- tran %>%
    subset(UNITS>=1) %>%
    left_join(price_type, by = c("SKU_IDNT", "DAY_DT"))%>%
    mutate(PRICE_TYPE = as.factor(ifelse(is.na(PRICE_TYPE),"REG",PRICE_TYPE)))%>%
    left_join(description, by =c("SKU_IDNT")) %>% 
    left_join(day_events, by = c("DAY_DT"="DAY")) %>%
    mutate(EVENT = as.factor(ifelse(is.na(EVENT),"NONE",EVENT)))%>%
    left_join(fiscal, by = "DAY_DT") %>%
    select(-c(RTRN_UNITS, RTRN_AMT, UNIT_PRICE_RETURN, CLASS_IDNT, LOAD_DT, CLOSE_DT, LENGTH, CREATE_DT))

save(demand, file = 'data/created/demand.Rdata')
save(description, file = 'data/created/information.Rdata')
save(tran, file = "data/created/tran.Rdata")

#Filter price_types

demand <- demand %>%
    filter(PRICE_TYPE %in% c('PROMO','REG'))

bridge <- demand %>%
    filter(DEPT == 'Bridge') %>%
    arrange(DAY_DT, SKU_IDNT)

save(bridge, file = 'data/created/bridge.Rdata')

contemporary <- demand %>%
    filter(DEPT == 'Contemporary') %>%
    arrange(DAY_DT, SKU_IDNT)

save(contemporary, file = 'data/created/contemporary.Rdata')

############# Time series #############

start_SKU <- demand %>% 
    group_by(SKU_IDNT) %>% 
    summarize(first = min(DAY_DT))%>%
    left_join(description %>% select(SKU_IDNT, CREATE_DT, LOAD_DT), by = "SKU_IDNT") %>%
    mutate(catalog = as.Date(ifelse(is.na(CREATE_DT), LOAD_DT,CREATE_DT)),
           start = pmin(first, catalog)) %>%
    left_join(fiscal, by = c("start" = "DAY_DT"))%>%
    select(SKU_IDNT, DAY_IDNT, WK_IDNT, MTH_IDNT, YEAR)

#####  month ####

months <- fiscal %>%
    filter(between(YEAR, 2013, 2016)) %>%
    select(MTH_IDNT, YEAR) %>%
    unique() %>%
    arrange(YEAR, MTH_IDNT) 

agg_month <- demand %>%
    filter(between(YEAR, 2013,2016)) %>%
    group_by(SKU_IDNT, MTH_IDNT, YEAR) %>%
    summarise(UNITS = sum(UNITS))

time_series_month <- merge(months, start_SKU %>% select(SKU_IDNT)) %>%
    left_join(agg_month, by = c("SKU_IDNT", "YEAR", "MTH_IDNT")) %>%
    left_join(start_SKU, by = c("SKU_IDNT")) %>%
    filter(YEAR.x*100+MTH_IDNT.x >= YEAR.y*100+MTH_IDNT.y) %>%
    mutate(MTH_IDNT = MTH_IDNT.x,
           YEAR = YEAR.x) %>%
    select(SKU_IDNT, YEAR, MTH_IDNT, UNITS)

time_series_month[is.na(time_series_month)] <- 0

save(time_series_month, file = "data/created/time_series_month.Rdata")

## Weekly


weeks <- fiscal %>%
    filter(between(YEAR, 2013, 2016)) %>%
    select(WK_IDNT, YEAR) %>%
    unique() %>%
    arrange(YEAR, WK_IDNT)

SKU_time <- SKU_year %>%
    left_join(weeks, by = "YEAR")

agg_week <- bridge %>%
    filter(between(YEAR, 2013,2016)) %>%
    group_by(SKU_IDNT, WK_IDNT, YEAR) %>%
    summarise(UNITS = sum(UNITS))

time_series_week <- SKU_time %>%
    full_join(agg_week, by = c("SKU_IDNT", "YEAR", "WK_IDNT"))

time_series_week[is.na(time_series_week)] <- 0

save(time_series_week, file = "data/created/time_series_week.Rdata")
