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

####################################   Read/load data  ###############################################

load('data/created/demand.Rdata')
load('data/created/information.Rdata')

#Filter price_types
demand <- demand %>%
    filter(PRICE_TYPE %in% c('PROMO','REG')) 

bridge <- demand %>%
    filter(DEPT == 'Bridge') %>%
    select(-DEPT)

###### CREATE CLUSTERS FOR THE BRANDS  ######

# 119 brands in bridge

#Time effects

brand_time <- bridge %>%
    group_by(BRAND_NAME,DAY_DT) %>%
    summarise(UNITS = sum(UNITS)) %>%
    mutate(YEAR = year(DAY_DT)) %>%
    filter(YEAR < 2017) %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarise(UNITS = sum(UNITS),
              START = min(DAY_DT),
              END = max(DAY_DT),
              ACTIVE_DAYS = n())

table(brand_time$YEAR)

brand_color1 <- bridge %>%
    mutate(YEAR = year(DAY_DT)) %>%
    filter(YEAR < 2017) %>%
    group_by(BRAND_NAME, YEAR, COLOR) %>%
    summarise(freq = n())

brand_color <- brand_color1 %>%
    left_join(brand_time %>% select(BRAND_NAME, YEAR, UNITS),by = c("BRAND_NAME", "YEAR")) %>%
    mutate(rel_freq = freq/UNITS) %>%
    select(-c(freq,UNITS)) %>%
    spread(COLOR,rel_freq)

brand_color[is.na(brand_color)] <- 0

brand_style1 <- bridge %>%
    mutate(YEAR = year(DAY_DT)) %>%
    filter(YEAR < 2017) %>%
    group_by(BRAND_NAME, YEAR, CLASS_DESC ) %>%
    summarise(freq = n()) 

brand_style <- brand_style1 %>%
    left_join(brand_time %>% select(BRAND_NAME, YEAR, UNITS),by = c("BRAND_NAME", "YEAR")) %>%
    mutate(rel_freq = freq/UNITS) %>%
    select(-c(freq,UNITS)) %>%
    spread(CLASS_DESC,rel_freq)

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

brand <- brand_time %>%
    left_join(brand_info, by = "BRAND_NAME") %>%
    mutate(seniority = elapsed_months(END, CREATE)) %>%
    left_join(brand_color, by = c("BRAND_NAME", "YEAR")) %>%
    left_join(brand_style, by = c("BRAND_NAME", "YEAR")) %>%
    select(-c(START,END,CREATE))

######    CLUSTERING    #######

preproc = preProcess(brand[-c(1:2)])
brandNorm = predict(preproc, brand[-c(1:2)])
summary(brandNorm)

d <- dist(brandNorm)
clusters <- hclust(d, method = "ward.D")
plot(clusters, hang = -1)
clusterGroups = cutree(clusters, k = 5)
rect.hclust(clusters, k=5, border="red")

table(clusterGroups)
tapply(brand$UNITS, clusterGroups, mean)
tapply(brand$seniority,clusterGroups, mean)
tapply(brand$SHOULDER,clusterGroups, mean)
tapply(brand$ACTIVE_DAYS,clusterGroups, mean)

brand$clust <-  clusterGroups

######   MODEL     #######

bridge <- bridge %>% 
    mutate(YEAR = year(DAY_DT)) %>%
    left_join(brand %>% select(BRAND_NAME, YEAR, clust),by = c("BRAND_NAME", "YEAR"))

control = rpart.control(cp = 0.01, minsplit = )
tree <- rpart(UNITS ~  N_BRAND + CLASS_DESC + EVENT + month(DAY_DT) + UNIT_PRICE_DEMAND + COLOR + clust, 
              data = bridge, method = 'anova', minsplit = 10000)
rpart.plot(tree)

tree2 <- rpart(UNITS ~  N_BRAND + CLASS_DESC + EVENT + month(DAY_DT) + UNIT_PRICE_DEMAND + COLOR + BRAND_NAME, 
              data = bridge, method = 'anova', minsplit = 10000)
rpart.plot(tree2)

summary(tree)


########################

sku_info <- bridge %>%
    left_join(brand, by ="BRAND_NAME") %>%
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




