#rm(list=ls())

#Working directory
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Libraries
library(dplyr)
library(ggplot2)

#Load data sets
load("data/created/time_series_month.Rdata")
load("data/created/brand_info_bridge")
load("data/created/information.Rdata")
load('data/created/bridge.Rdata')

elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

fiscal <- read.csv("data/fscl.csv", as.is = T)

fiscal <- fiscal %>%
    mutate(DAY_DT = as.Date(DAY_DT),
           YEAR = YR_454) %>%
    select(-YR_454)

#### auxiliary data ####

avg_prices <- tapply(brand$avg_price,brand$clust, mean)
avg_seniority <- tapply(brand$seniority,brand$clust, mean)

start_year <- fiscal %>%
    group_by(YEAR) %>%
    summarise(START = min(DAY_DT))

clusters1 <- brand %>%
    select(BRAND_NAME, YEAR, clust) %>%
    mutate(Y = YEAR+1) %>%
    select(-YEAR)

clusters2 <- brand %>%
    select(BRAND_NAME, YEAR, clust) 

aux <- time_series_month %>%
    arrange(SKU_IDNT, YEAR, MTH_IDNT) %>%
    group_by(SKU_IDNT) %>%
    left_join(description %>% select(SKU_IDNT, BRAND_NAME, N_BRAND, CLASS_DESC, ORIG_PRICE, COLOR),
              by = "SKU_IDNT") %>%
    left_join(clusters1, by = c("YEAR" = "Y", "BRAND_NAME")) %>%
    left_join(clusters2, by = c("YEAR" = "YEAR", "BRAND_NAME")) %>%
    mutate(clust.aux= ifelse(is.na(clust.x),clust.y,clust.x)) %>%
    select(-c(clust.x,clust.y))

##### NEW BRANDS ####

missing_brands <- aux %>%
    filter(is.na(clust.aux) & UNITS != 0) %>%
    ungroup() %>% 
    left_join(start_year,by = "YEAR") %>%
    select(BRAND_NAME, YEAR, START) %>%
    distinct()

brand_info <- description %>%
    filter(BRAND_NAME %in% missing_brands$BRAND_NAME) %>%
    group_by(BRAND_NAME) %>%
    summarize(avg_price = mean(ORIG_PRICE, na.rm = T),
             CREATE = min(CREATE_DT),
             CREATE = as.Date(ifelse(is.na(CREATE), min(LOAD_DT), CREATE))) %>% 
    left_join(missing_brands, by = "BRAND_NAME") %>%
    mutate(seniority = pmax(0,elapsed_months(START, CREATE)),
           avg_p1 = avg_prices[1],
           avg_p2 = avg_prices[2],
           d1 = abs(avg_p1-avg_price),
           d2 = abs(avg_p2-avg_price),
           cluster = ifelse(d1<d2,1,2)) %>%
    select(BRAND_NAME, cluster)

aux2 <- aux %>%
    left_join(brand_info, by = "BRAND_NAME") %>%
    mutate(cluster = ifelse(is.na(cluster),as.numeric(clust.aux),cluster)) %>%
    filter(!is.na(cluster))

##### BRAND CLUSTER + COLOR + STYLE  #####

aux3 <- aux2 %>%
    group_by(cluster,COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
    summarise(UNITS= sum(UNITS))
  
aux4 <- aux3 %>%
    arrange(cluster, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
    group_by(cluster, COLOR, CLASS_DESC) %>%
    mutate(lagged_1 = lag(UNITS, 9),
           lagged_2 = lag(UNITS, 10),
           lagged_3 = lag(UNITS, 11),
           lagged_4 = lag(UNITS, 12))

# agg_month <- bridge %>%
#     filter(between(YEAR, 2013, 2016)) %>%
#     left_join(clusters, by = c("YEAR" = "Y", "BRAND_NAME")) %>%
#     group_by(clust, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
#     summarize(UNITS = sum(UNITS))
# 
# id <- agg_month %>%
#     ungroup() %>%
#     select(clust, COLOR, CLASS_DESC) %>%
#     unique()
# 
# id2013 <- id %>% mutate(YEAR = 2013)
# id2014 <- id %>% mutate(YEAR = 2014)
# id2015 <- id %>% mutate(YEAR = 2015)
# id2016 <- id %>% mutate(YEAR = 2016)
# 
# idyears = rbind(id2013, id2014, id2015, id2016)
# 
# months <- fiscal %>%
#     filter(between(YEAR, 2013, 2016)) %>%
#     select(MTH_IDNT, YEAR) %>%
#     unique() %>%
#     arrange(YEAR, MTH_IDNT)
# 
# aux <- idyears %>%
#     left_join(months, by = "YEAR")
# 
# time_series_month<- aux %>%
#     full_join(agg_month, by = c("clust", "COLOR", "CLASS_DESC", "YEAR", "MTH_IDNT"))
# 
# time_series_month[is.na(time_series_month$UNITS),"UNITS"] <-0
# 
# lagged_data <- time_series_month %>%
#     arrange(clust, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
#     group_by(clust, COLOR, CLASS_DESC) %>%
#     mutate(lagged_1 = lag(UNITS, 9),
#            lagged_2 = lag(UNITS, 10),
#            lagged_3 = lag(UNITS, 11),
#            lagged_4 = lag(UNITS, 12))
# 
train <- aux4 %>%
    filter(YEAR < 2016) 
test <- aux4 %>%
    filter(YEAR == 2016)

train <- train[complete.cases(train),]
test <- test[complete.cases(test),]


### REGRESSION MODEL ####

#All units 

model <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = train)
summary(model)

model1 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = subset(train, cluster == 1))
summary(model1)

model2 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = subset(train, cluster == 2))
summary(model2)

model3 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 , data = subset(train, cluster == 3))
summary(model3)

#### PREDICTION ###

estimate_SSE <- function(obs, pred){
    n <- length(obs)
    dif <- (obs-pred)^2
    error <- sum(dif)
    return(error)
}

test$test.pred <- predict(model, newdata = test)
SSE <- estimate_SSE(test$UNITS,test$test.pred)
SS <- estimate_SSE(mean(test$UNITS), test$UNITS)
1-sum(SSE)/sum(SS)
sqrt(SSE/nrow(test))

clust1 <- test %>% filter(cluster == 1)
clust2 <- test %>% filter(cluster == 2)
clust3 <- test %>% filter(cluster == 3)

test.pred <- predict(model1, newdata = clust1)
SSE <- estimate_SSE(clust1$UNITS,test.pred)
SS <- estimate_SSE(mean(clust1$UNITS), clust1$UNITS)
1-sum(SSE)/sum(SS)
sqrt(SSE/nrow(test))

test.pred <- predict(model2, newdata = clust2)
SSE <- estimate_SSE(clust2$UNITS,test.pred)
SS <- estimate_SSE(mean(clust2$UNITS), clust2$UNITS)
1-sum(SSE)/sum(SS)
sqrt(SSE/nrow(test))

test.pred <- predict(model3, newdata = clust3)
SSE <- estimate_SSE(clust3$UNITS,test.pred)
SS <- estimate_SSE(mean(clust3$UNITS), clust3$UNITS)
1-sum(SSE)/sum(SS)
sqrt(SSE/nrow(test))


all <- rbind(test, train) %>%
    group_by(YEAR, MTH_IDNT) %>%
    summarize(UNITS = sum(UNITS),
              PREDICTION = sum(test.pred))




###



control <- rpart.control(minsplit = 1500 )
tree <- rpart(UNITS ~ lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
               as.factor(MTH_IDNT), data = aux3, method = 'anova',
              control = control)
rpart.plot(tree)



model2 <- lm(MTH_IDNT ~ lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 CLASS_DESC + COLOR +  
                 as.factor(MTH_IDNT), data = subset(aux, clust == 2))
model3 <- lm(MTH_IDNT ~ lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 CLASS_DESC + COLOR 
                 , data = subset(aux, clust == 3))
summary(model2)


