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
library(caret)
library(ggdendro)
library(factoextra)
library(cluster)
library(NbClust)

####################################   Read/load data  ###############################################

load('data/created/bridge.Rdata')
load('data/created/information.Rdata')

fiscal <- read.csv("data/fscl.csv", as.is = T)

fiscal <- fiscal %>%
     mutate(DAY_DT = as.Date(DAY_DT),
            YEAR = YR_454) %>%
    select(-YR_454)

###### CREATE CLUSTERS FOR THE BRANDS  ######

# 119 brands in bridge

#Time effects

start_year <- fiscal %>%
    group_by(YEAR) %>%
    summarise(START = min(DAY_DT))

#Info by year:

brand_year <- bridge %>%
    group_by(BRAND_NAME,DAY_DT,YEAR) %>%
    summarise(UNITS = sum(UNITS)) %>%
    filter(between(YEAR, 2013, 2015)) %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarise(UNITS = sum(UNITS),
              ACTIVE_DAYS = n()) %>%
    left_join(start_year, by = "YEAR")

#Monthly info

brand_month <- bridge %>%
    filter(between(YEAR, 2013, 2015)) %>%
    group_by(BRAND_NAME,MTH_IDNT,YEAR) %>%
    summarise(UNITS = sum(UNITS)) %>%
    spread(MTH_IDNT, UNITS)

brand_month[is.na(brand_month)] <- 0

#Attributes:

#Color: 

brand_color1 <- bridge %>%
    filter(between(YEAR, 2013, 2015)) %>%
    group_by(BRAND_NAME, YEAR, COLOR) %>%
    summarise(freq = length(unique(SKU_IDNT)), popularity = n())

brand_color2 <- brand_color1 %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarize(tot_freq = sum(freq), tot_pop = sum(popularity))

brand_color <- brand_color1 %>%
    left_join(brand_color2, by = c("BRAND_NAME", "YEAR")) %>%
    mutate(rel_popularity = popularity/tot_pop,
           rel_frequency = round(freq/tot_freq,2)) %>%
    select(-c(freq,popularity, rel_frequency,tot_freq, tot_pop)) %>%
    spread(COLOR,rel_popularity)

#rel_popularity and rel_frequency have a correlation of .94
#we'll keep relative popularity since tells us more about selling patterns of a brand

brand_color[is.na(brand_color)] <- 0

brand_style1 <- bridge %>%
    filter(between(YEAR, 2013, 2015)) %>%
    group_by(BRAND_NAME, YEAR, CLASS_DESC ) %>%
    summarise(freq = length(unique(SKU_IDNT)), pop = n()) 

brand_style <- brand_style1 %>%
    left_join(brand_year %>% select(BRAND_NAME, YEAR, UNITS),by = c("BRAND_NAME", "YEAR")) %>%
    mutate(rel_pop = pop/UNITS) %>%
    select(-c(freq,pop,UNITS)) %>%
    spread(CLASS_DESC,rel_pop)

brand_style[is.na(brand_style)] <- 0

brand_info <- description %>%
    filter(DEPT == 'Bridge') %>% 
    group_by(BRAND_NAME) %>%
    summarise(CREATE = min(CREATE_DT),
              CREATE = as.Date(ifelse(is.na(CREATE), min(LOAD_DT), CREATE)))

elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

# Price bucket #

b <- bridge %>% select(SKU_IDNT, BRAND_NAME, ORIG_PRICE, YEAR)

b$bucket <- 1
b$bucket[between(b$ORIG_PRICE,50.01,100)] <- 2
b$bucket[between(b$ORIG_PRICE,100.01,150)] <- 3
b$bucket[between(b$ORIG_PRICE,150.01,250)] <- 4
b$bucket[between(b$ORIG_PRICE,250.01,350)] <- 5
b$bucket[b$ORIG_PRICE > 350] <- 6

b_prices <- b %>%
    group_by(bucket)%>%
    summarize(mean_price = mean(ORIG_PRICE))

b <- b %>%
    left_join(b_prices, by ="bucket")

brand_buckets <- b %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarize(bucket = mean(bucket),
              avg_price = mean(ORIG_PRICE),  #quantiles for price
              q25 = quantile(ORIG_PRICE,.25),
              q50 = quantile(ORIG_PRICE,.50),
              q75 = quantile(ORIG_PRICE,.75),
              min = min(ORIG_PRICE),
              max = max(ORIG_PRICE))

brand <- brand_year %>%
    left_join(brand_info, by = "BRAND_NAME") %>%
    mutate(seniority = pmax(0,elapsed_months(START, CREATE))) %>%
    left_join(brand_buckets, by = c("BRAND_NAME", "YEAR")) %>%
    left_join(brand_month, by = c("BRAND_NAME", "YEAR") ) %>%
    left_join(brand_color, by = c("BRAND_NAME", "YEAR")) %>%
    left_join(brand_style, by = c("BRAND_NAME", "YEAR")) %>%
    select(-c(START,CREATE))


######    CLUSTERING    #######

dataScaled <- scale(brand[c(4:24)])
d <- dist(dataScaled)

clusters <- hclust(d, method = "ward.D")
fviz_nbclust(dataScaled, hcut, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)

plot(clusters, labels = F, hang = -1)
clusterGroups = cutree(clusters, k = 3)
rect.hclust(clusters, k=3, border=2:4)

table(clusterGroups)
tapply(brand$UNITS, clusterGroups, mean)
tapply(brand$seniority,clusterGroups, mean)
tapply(brand$ACTIVE_DAYS,clusterGroups, mean)
tapply(brand$bucket,clusterGroups, mean)
tapply(brand$avg_price,clusterGroups, mean)

brand$clust <-  as.factor(clusterGroups)

save(brand, file = "data/created/brand_info_bridge")

