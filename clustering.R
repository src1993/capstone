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
library(fastcluster)

####################################   Read/load data  ###############################################

load('data/created/demand.Rdata')
load('data/created/information.Rdata')

demand <- demand %>%
    filter(PRICE_TYPE %in% c('PROMO','REG')) 

bridge <- demand %>%
    filter(DEPT == 'Bridge')

contemporary <- demand %>%
    filter(DEPT == 'Contemporary')

######

brand <- bridge %>%
    group_by(BRAND_NAME,DAY_DT) %>%
    summarise(UNITS = sum(UNITS)) %>%
    mutate(YEAR = year(DAY_DT)) %>%
    filter(YEAR < 2017) %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarise(UNITS = sum(UNITS),
              START = min(DAY_DT),
              END = max(DAY_DT),
              ACTIVE_DAYS = n())

                            
                  

                  
######                  
    
tot_units <- sum(demand$UNITS)

brand1 <- bridge %>%
    group_by(BRAND_NAME) %>%
    summarise(BRAND_FREQ = sum(UNITS))

color <- bridge %>%
    group_by(COLOR) %>%
    summarise(COL_FREQ = sum(UNITS))

sku_info <- bridge %>%
    left_join(brand, by ="BRAND_NAME") %>%
    left_join(color, by ="COLOR") %>%
    group_by(SKU_IDNT,COLOR,BRAND_NAME) %>%
    summarise(units = sum(UNITS),
              mean_price = mean(UNIT_PRICE_DEMAND),
              COL_FREQ = mean(COL_FREQ),
              BRAND_FREQ = mean(BRAND_FREQ),
              start = min(DAY_DT),
              end = max(DAY_DT),
              days = n()) %>%
    mutate(length = as.numeric(end-start+1)) %>%
    select(-start,-end)

sku_info <- data.frame(sku_info)

sku_info <- sku_info %>%
    left_join(brand)

tree <- rpart(TOT_UNITS ~ mean_price + days + length + clust, data = sku_info)
rpart.plot(tree)


s <- sample_frac(sku_info, 0.6)
#d <- dist(s[,4:9])

#clusters <- hclust.vector(d)

p <- prcomp(sku_info[,4:9], center = T, scale. = T)
plot(p, type = 'l')
coord <- data.frame(p$rotation)
plot(coord$PC1, coord$PC2)

##

sku_price <- demand %>%
    group_by(SKU_IDNT) %>%
    summarise(MIN_PRICE = min(UNIT_PRICE_DEMAND),
              MAX_PRICE = max(UNIT_PRICE_DEMAND),
              MEAN_PRICE = mean(UNIT_PRICE_DEMAND),
              ORIG_PRICE = mean(ORIG_PRICE)) %>%
    mutate(MAX = pmax(MAX_PRICE,ORIG_PRICE))

sum(sku_price$MAX_PRICE > sku_price$ORIG_PRICE) #456 cases where orig price is no the maximum price that shows up

information <- description %>%
    inner_join(sku_price, by = c("SKU_IDNT", "ORIG_PRICE"))


## brand

brand_info <- bridge %>%
    group_by(BRAND_NAME,DAY_DT) %>%
    summarise(UNITS = sum(UNITS)) %>%
    mutate(YEAR = year(DAY_DT)) %>%
    filter(YEAR < 2017) %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarise(UNITS = sum(UNITS),
              START = min(DAY_DT),
              END = max(DAY_DT),
              ACTIVE_DAYS = n())

brand_info2 <- description %>%
    filter(DEPT == 'Bridge') %>% 
    group_by(BRAND_NAME) %>%
    summarise(LOAD = min(LOAD_DT),
              CLOSE = max(CLOSE_DT),
              CREATE = min(CREATE_DT))

brand_join <- brand_info %>%
    left_join(brand_info2, by = "BRAND_NAME")

brand <- brand_join %>%
    mutate(BEG = as.Date(ifelse(year(LOAD)==YEAR, 
                                pmin(START,LOAD), 
                                pmin(START,as.Date(paste(as.character(YEAR),'-01-01',sep=''))))),
           END = as.Date(ifelse(year(CLOSE)==YEAR,
                                pmax(END,CLOSE),
                                pmax(END,as.Date(paste(as.character(YEAR),'-12-31',sep=''))))),
           LENGTH = as.numeric(END-BEG+1)) %>%
    select(BRAND_NAME, UNITS, BEG, END, LENGTH) 

d <- dist(brand[,c("UNITS","LENGTH")])

clusters <- hclust(d, method = "ward.D")

plot(clusters)
clusterGroups = cutree(clusters, k = 5)
table(clusterGroups)
tapply(brand$TOT_UNITS, clusterGroups, mean)
tapply(brand$LENGTH, clusterGroups, mean)

brand$clust <-  clusterGroups

