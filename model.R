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

fiscal <- read.csv("data/fscl.csv", as.is = T)

fiscal <- fiscal %>%
    mutate(DAY_DT = as.Date(DAY_DT),
           YEAR = YR_454) %>%
    select(-YR_454)

clusters <- brand %>%
    select(BRAND_NAME, YEAR, clust) %>%
    mutate(Y = YEAR+1) %>%
    select(-YEAR)

aux <- time_series_month %>%
    arrange(SKU_IDNT, YEAR, MTH_IDNT) %>%
    group_by(SKU_IDNT) %>%
    left_join(description %>% select(SKU_IDNT, BRAND_NAME, N_BRAND, CLASS_DESC, ORIG_PRICE, COLOR),
              by = "SKU_IDNT") %>%
    left_join(clusters, by = c("YEAR" = "Y", "BRAND_NAME"))

aux2 <- aux %>%
    group_by(clust, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
    summarise(UNITS= sum(UNITS)) 

aux3 <- aux2 %>%
    arrange(clust, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
    group_by(clust, COLOR, CLASS_DESC) %>%
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
train <- aux3 %>%
    filter(YEAR < 2016) 

test <- aux3 %>%
    filter(YEAR >= 2016)

train <- train[complete.cases(train),]
test <- train[complete.cases(test),]


### REGRESSION MODEL ####

#All units 

model <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = train)
summary(model)

model1 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = subset(train, clust == 1))
summary(model1)

model2 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = subset(train, clust == 2))
summary(model2)

model3 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = subset(train, clust == 3))
summary(model3)

#### PREDICTION ###

estimate_SSE <- function(obs, pred){
    n <- length(obs)
    dif <- (obs-pred)^2
    error <- sum(dif)
    return(error)
}

train$test.pred <- model$fitted.values
test$test.pred <- predict(model, newdata = test)
SSE <- estimate_SSE(test$UNITS,test$test.pred)
SS <- estimate_SSE(mean(test$UNITS), test$UNITS)
1-sum(SSE)/sum(SS)
sqrt(SSE/nrow(test))

clust1 <- test %>% filter(clust == 1)
clust2 <- test %>% filter(clust == 2)
clust3 <- test %>% filter(clust == 3)

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


