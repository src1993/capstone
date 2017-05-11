#rm(list=ls())

#Working directory
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Load data sets
load("data/created/time_series.Rdata")
load("data/created/brand_info_bridge")
load("data/created/information.Rdata")

aux <- time_series %>%
    arrange(SKU_IDNT, YEAR, MTH_IDNT) %>%
    group_by(SKU_IDNT) %>%
    left_join(description %>% select(SKU_IDNT, BRAND_NAME, N_BRAND, CLASS_DESC, ORIG_PRICE, COLOR),
              by = "SKU_IDNT") %>%
    left_join(brand %>% select(BRAND_NAME, YEAR, clust), by = c("YEAR", "BRAND_NAME"))

aux2 <- aux %>%
    group_by(clust, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
    summarise(UNITS= sum(UNITS)) 

aux3 <- aux2 %>%
    arrange(clust, COLOR, CLASS_DESC, YEAR, MTH_IDNT) %>%
    group_by(clust, COLOR, CLASS_DESC) %>%
    mutate(lagged_1 = lag(UNITS, 9),
           lagged_2 = lag(UNITS, 10),
           lagged_3 = lag(UNITS, 11),
           lagged_4 = lag(UNITS, 12),
           lagged_5 = lag(UNITS, 13)) 

### REGRESSION MODEL ####

#All units 

model1 <- lm(UNITS ~ 0 + lagged_1 + lagged_2 + lagged_3 + lagged_4 + 
                 as.factor(MTH_IDNT), data = subset(aux3,clust==5))
summary(model1)



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


