#rm(list=ls())

#Set working directory          
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Read/load data

events <- read.csv('data/events.csv')
fiscal <- read.csv('data/fscl.csv')
hier <- read.csv('data/hier.csv')
catalog <- read.csv('data/pctlg_sku.csv')
price <- load('data/price_type.rda')
tran <- read.csv('data/tran.csv')
