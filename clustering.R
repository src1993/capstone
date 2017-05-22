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

end_year <- fiscal %>%
    group_by(YEAR) %>%
    summarise(START_YEAR = min(DAY_DT))

brand_time <- bridge %>%
    group_by(BRAND_NAME,DAY_DT,YEAR) %>%
    summarise(UNITS = sum(UNITS)) %>%
    filter(between(YEAR, 2013, 2015)) %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarise(UNITS = sum(UNITS),
              ACTIVE_DAYS = n(),
              START_SALES = min(DAY_DT)) %>%
    left_join(end_year, by = "YEAR") %>%
    mutate(START = pmax(START_YEAR, START_SALES)) %>%
    select(-c(START_YEAR, START_SALES))

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
    left_join(brand_time %>% select(BRAND_NAME, YEAR, UNITS),by = c("BRAND_NAME", "YEAR")) %>%
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
              price = mean(mean_price))

brand <- brand_time %>%
    left_join(brand_info, by = "BRAND_NAME") %>%
    mutate(seniority = elapsed_months(START, CREATE)) %>%
    left_join(brand_buckets, by = c("BRAND_NAME", "YEAR")) %>%
    #left_join(brand_color, by = c("BRAND_NAME", "YEAR")) %>%
    #left_join(brand_style, by = c("BRAND_NAME", "YEAR")) %>%
    select(-c(START,CREATE))


######    CLUSTERING    #######


dataScaled <- scale(brand[c(3:6)])
d <- dist(dataScaled)

d <- dist(dataScaled)
clusters <- hclust(d, method = "ward.D")
fviz_nbclust(dataScaled, hcut, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)

plot(clusters, labels = F, hang = -1)
clusterGroups = cutree(clusters, k = 5)
rect.hclust(clusters, k=5, border=2:4)

table(clusterGroups)
tapply(brand$UNITS, clusterGroups, mean)
tapply(brand$seniority,clusterGroups, mean)
tapply(brand$SHOULDER,clusterGroups, mean)
tapply(brand$ACTIVE_DAYS,clusterGroups, mean)
tapply(brand$bucket,clusterGroups, mean)

brand$clust <-  as.factor(clusterGroups)

save(brand, file = "data/created/brand_info_bridge")


######   MODEL     #######

train <- bridge %>% 
    filter(between(YEAR, 2013, 2015)) %>%
    left_join(brand %>% select(BRAND_NAME, YEAR, clust),by = c("BRAND_NAME", "YEAR"))

####

SKU_2013 <- data.frame(SKU_IDNT = unique(bridge$SKU_IDNT), YEAR = 2013)
SKU_2014 <- data.frame(SKU_IDNT = unique(bridge$SKU_IDNT), YEAR = 2014)
SKU_2015 <- data.frame(SKU_IDNT = unique(bridge$SKU_IDNT), YEAR = 2015)

SKU_year <- rbind(SKU_2013, SKU_2014, SKU_2015)

months <- fiscal %>%
    filter(between(YEAR, 2013, 2015)) %>%
    select(MTH_IDNT, YEAR) %>%
    unique() %>%
    arrange(YEAR, MTH_IDNT)

SKU_time <- SKU_year %>%
    left_join(months, by = "YEAR")

agg_month <- train %>%
    group_by(SKU_IDNT, MTH_IDNT, YEAR) %>%
    summarise(UNITS = sum(UNITS))

time_series <- SKU_time %>%
    full_join(agg_month, by = c("SKU_IDNT", "YEAR", "MTH_IDNT"))

time_series[is.na(time_series)] <- 0

save(time_series, file = "data/created/time_series.Rdata")

#######
experiment <- bridge %>%
    group_by(clust, CLASS_DESC, COLOR) %>%
    summarise(UNITS = sum(UNITS),
              n = n())

experiment_2 <- experiment %>%
    group_by(clust) %>%
    summarise(total = n(),
              )

ggplot(experiment, aes(x=clust, y = n, fill = clust))+geom_boxplot()+
    coord_flip()+xlab('cluster')+ylab('observations')
    
experiment_date <- bridge %>%
    mutate(DATE = as.yearmon(DAY_DT)) %>%
    group_by(clust, CLASS_DESC, COLOR, DATE,EVENT) %>%
    summarise(UNITS = sum(UNITS)) %>% 
    mutate(UNIT = !is.na(UNITS)) %>%
    spread(EVENT,UNIT)

experiment_date[is.na(experiment_date)] <- 0

names(experiment_date) <-gsub(" ","_",names(experiment_date))

var <- names(experiment_date)
var <- var[-c(1,2,3,5)]

fmla <- as.formula(paste("UNITS ~ ", paste(var, collapse= "+")))

#control = rpart.control(cp = 0.01, minsplit = )

m1 <- lm(UNITS ~ month(DATE), data = experiment_date)

tree <- rpart(UNITS ~  N_BRAND + CLASS_DESC + EVENT + month(DAY_DT) + UNIT_PRICE_DEMAND + COLOR + clust, 
              data = bridge, method = 'anova', minsplit = 10000)

###
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




