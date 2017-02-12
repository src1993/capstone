#rm(list=ls())

#Set working directory          
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Libraries
library(dplyr)

#Read/load data

events <- read.csv('data/events.csv', as.is = T)
# fiscal <- read.csv('data/fscl.csv')
# hier <- read.csv('data/hier.csv')
# catalog <- read.csv('data/pctlg_sku.csv')
# load('data/price_type.rda')
# tran <- read.csv('data/tran.csv')

#EVENTS

events <- events %>%
    mutate(start = as.Date(DT_BEG_ACT,
           format = "%m/%d/%Y"),
           end = as.Date(DT_END_ACT,
           format = "%m/%d/%Y"),
           duration = end-start+1) %>%
    select(-c(DT_BEG_ACT,DT_END_ACT))


