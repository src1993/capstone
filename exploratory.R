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
hier <- read.csv('data/hier.csv', as.is = T)
catalog <- read.csv('data/pctlg_sku.csv', as.is = T)
load('data/price_type.rda')
tran <- read.csv('data/tran.csv', as.is = T)

#tran_backup <- tran

#EVENTS

events <- events %>%
    mutate(name = EVENT_NM, 
           start = as.Date(DT_BEG_ACT,
                           format = "%m/%d/%Y"),
           end = as.Date(DT_END_ACT,
                         format = "%m/%d/%Y"),
           duration = end-start+1,
           year = YR_454) %>%
    select(-c(DT_BEG_ACT,DT_END_ACT,EVENT_NM,YR_454))

#FISCAL

fiscal <- fiscal %>%
    mutate(DAY_DT = as.Date(DAY_DT)) 

#HIERARCHY

hier <- hier %>%
    select(-c(DIV_IDNT,DIV_DESC,SKU_DESC,STYLE_DESC))

table(hier$DM_RECD_CLOSE_DT)


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
           RTRN_AMT = as.numeric(RTRN_AMT),
           UNIT_PRICE = DEMAND/UNITS) %>%
    select(-TRAN_DT)

sum(is.na(tran$UNITS)) #n_demand_none 

#Separate in demand and returns

p <- tran%>%
    mutate(UNITS+ RTRN_UNITS)%>%
    arrange(-UNITS+ RTRN_UNITS)

demand <- tran %>%
    filter(!is.na(UNITS)) %>%
    select(DAY_DT, SKU_IDNT, UNITS, DEMAND, UNIT_PRICE) 

returns <- tran %>%
    filter(!is.na(RTRN_UNITS)) %>%
    select(DAY_DT, SKU_IDNT, UNITS, DEMAND, UNIT_PRICE) 

#Analyze demand

demand_sku <- demand %>%
    group_by(SKU_IDNT) %>%
    summarize(total_units = sum(UNITS), total_demand = sum(DEMAND))%>%
    arrange(-total_units)

summary(demand_sku)

#Demand by date

demand_date <- demand %>%
    group_by(DAY_DT) %>% 
    summarize(total_units = sum(UNITS), total_demand = sum(DEMAND))%>%
    arrange(DAY_DT) 

startTime <- as.Date("2013-01-01")
endTime <- as.Date("2017-02-01")
# create a start and end time R object
start.end <- c(startTime,endTime)

p <- ggplot(data = demand_date, aes(DAY_DT, total_units)) + geom_line() + xlab("Time") + ylab("Sales")+
    scale_x_date(limits=start.end, date_breaks = "3 months", date_labels = "%b %Y")+
    ggtitle("Total sales over time ")

p + geom_vline(data = subset(events, name %in% c("THANKSGIVING")),
               aes(xintercept = as.numeric(start), colour = "blue")) +
    geom_vline(data = subset(events, name == "THANKSGIVING"),
               aes(xintercept = as.numeric(end), colour = "green"))
                   
demand_events <- demand_date%>%
    mutate(ym = as.yearmon(DAY_DT)) %>%
    left_join(events) %>%
    mutate(event = ifelse(DAY_DT>= start & DAY_DT <= end,1,0)) %>%
    filter(event == 1) %>%
    group_by(name) %>%
    summarize(total_units = sum(total_units),
              total_demand = sum(total_demand),
              duration = as.numeric(sum(duration))) %>%
    mutate(units_per_day = total_units/duration,
           demand_per_day = total_demand/duration)%>%
    select(-duration) %>%
    arrange(name)

ggplot(data=demand_events, aes(factor(name),total_units))+
    geom_bar(stat="identity")+coord_flip() + ylab('Event')+
    xlab('Total units')+ggtitle('Units sold per day for special events')

ggplot(data=demand_events, aes(factor(name),units_per_day))+
    geom_bar(stat="identity")+coord_flip() + ylab('Event')+
    xlab('Units per day')+ggtitle('Units sold per day for special events')

#
  
summary(demand_date)  

ggplot(data = demand_date, aes(total_units)) + geom_histogram(bins=15)
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
    scale_x_yearmon()+ylab('Total units')

ggplot(data = demand_date_month, aes(DATE, total_demand)) + geom_line()+
    scale_x_yearmon()


#Join transactions with catalog

tran_cat <- demand %>%
    inner_join(catalog, by = "SKU_IDNT") %>%
    select(-c(CRT_TMSTP, RCD_UPDT_TMSTP)) %>%
    left_join(price_type) 

tran_cat <- tran_cat %>%
    mutate(PRICE_TYPE = ifelse(is.na(PRICE_TYPE),'REGULAR',PRICE_TYPE)) %>%
    group_by(SKU_IDNT, PRICE_TYPE)%>%
    summarize(total_units = sum(UNITS))

tran_cat_hor <- tran_cat %>%
    spread(PRICE_TYPE,total_units)


#####

color <- hier %>%
    group_by(COLR_IDNT, COLR_DESC) %>%
    summarize(tot = n()) %>%
    arrange(-tot)

prop.table(table(hier$SIZE_1_IDNT))
    
ggplot(color, aes(x=tot))+geom_histogram(bins=10)

summary(color)
    
