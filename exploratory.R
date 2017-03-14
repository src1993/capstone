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
    select(SKU_IDNT,SUPP_NAME, SUPP_NPG_CDE, BRAND_NAME, SBCLASS_DESC, SBCLASS_IDNT,
           CLASS_DESC, CLASS_IDNT, DEPT_DESC, DEPT_IDNT)

#CATALOG

catalog <- catalog %>%
    select(-c(CRT_TMSTP, RCD_UPDT_TMSTP))

#PRICE

price_type <- price_type %>%
    mutate(SKU_IDNT = as.integer(SKU_IDNT))

#TRANSACTIONS

SKU <- unique(tran$SKU_IDNT)
sum(!SKU %in% hier$SKU_IDNT) #all apear in hierarchy
sum(!SKU %in% catalog$SKU_IDNT) #21 don't apear in hierarchy

tran <- tran %>%
    mutate(DAY_DT = as.Date(TRAN_DT),
           UNITS = as.numeric(UNITS),
           DEMAND = as.numeric(DEMAND),
           RTRN_UNITS = as.numeric(RTRN_UNITS),
           RTRN_AMT = as.numeric(RTRN_AMT),
           UNIT_PRICE_DEMAND = DEMAND/UNITS,
           UNIT_PRICE_RETURN = -RTRN_AMT/RTRN_UNITS) %>%
    select(-TRAN_DT)

# Create product description

description <- hier %>%
    left_join(catalog, by = "SKU_IDNT")



#Separate in demand and returns

demand <- tran %>%
    filter(!is.na(UNITS)) %>%
    mutate(DATE = as.yearmon(DAY_DT)) %>%
    select(DATE, DAY_DT, SKU_IDNT, UNITS, DEMAND, UNIT_PRICE_DEMAND)

returns <- tran %>%
    filter(!is.na(RTRN_UNITS)) %>%
    mutate(DATE = as.yearmon(DAY_DT)) %>%
    select(DATE, DAY_DT, SKU_IDNT, UNITS, DEMAND, UNIT_PRICE_RETURN) 

#Analyze demand

demand_sku <- demand %>%
    group_by(SKU_IDNT, DATE) %>%
    summarize(total_units = sum(UNITS), total_demand = sum(DEMAND))%>%
    arrange(-total_units) %>%
    inner_join(description, by = "SKU_IDNT")

summary(demand_sku)


####

bridge <- demand_sku %>%
    filter(DEPT_IDNT == 4) 

contemporary <- demand_sku %>%
    filter(DEPT_IDNT == 470) 

b_tree1 <- rpart(total_units ~ month(DATE) + BRAND_NAME + SUPP_NPG_CDE + CLASS_DESC + CLR_FMLY_DESC, data = bridge, method = 'anova')
printcp(b_tree1)
plotcp(b_tree1, compress = TRUE) 
plot(b_tree1, uniform = TRUE)
text(b_tree1, use.n=TRUE, all=TRUE, cex=.8)
summary(b_tree1)
prp(b_tree1,type=3)


###

trial <- demand_sku %>%
    group_by(CLASS_DESC) %>%
    summarize(total_units = sum(total_units), skus = n())
summary(trial)
ggplot(trial, aes(x=1, y=skus))+geom_boxplot()

ggplot(demand_sku, aes(x=CLASS_DESC, y=total_units))+geom_boxplot()+ylim(0,200)


ggplot(demand_sku, aes(x=SUPP_NPG_CDE, y=total_units))+geom_boxplot()+ylim(0,500)


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
    group_by(SUPP_COLOR) %>%
    summarize(tot = n()) %>%
    arrange(-tot)
summary(color)

prop.table(table(hier$SIZE_1_IDNT))

style <- hier %>%
        group_by(sb) %>%  #change name variable
    summarize(tot = n()) %>%
    arrange(-tot)
summary(style)

ggplot(style, aes(x=tot))+geom_histogram(bins=10)

summary(color)
    
prop.table(table(hier$NDIRECT_SIZE1))
prop.table(table(hier$SUPP_NPG_CDE))*100

hier2 <- hier %>%
    mutate(aux = paste(SUPP_PRT_NBR, SUPP_COLOR, sep = "_")) %>%
    filter(aux !=CC)



#### BRANDS ###

sku_price <- demand %>%
    group_by(SKU_IDNT, UNIT_PRICE_DEMAND) %>%
    summarize(total_units = sum(UNITS), total_demand = sum(DEMAND))%>%
    arrange(-total_units) %>%
    inner_join(description, by = "SKU_IDNT") %>%
    mutate(PRICE = round(max(UNIT_PRICE_DEMAND, ORIG_PRC_AMT),2)) %>%
    select(-c(UNIT_PRICE_DEMAND, ORIG_PRC_AMT)) %>%
    group_by(SKU_IDNT) %>%
    summarize(UNITS = sum(total_units), price = mean(PRICE))

brands <- description %>%
    select(SKU_IDNT, BRAND_NAME) %>%
    inner_join(sku_price, by = "SKU_IDNT")

ggplot(brands, aes(UNITS, price)) + geom_point(aes(color=BRAND_NAME)) + guides(colour=FALSE)

color <- description %>%
    select(SKU_IDNT, CLR_FMLY_DESC) %>%
    inner_join(sku_price, by = "SKU_IDNT")

ggplot(color, aes(log(UNITS), log(price))) + geom_point(aes(color=CLR_FMLY_DESC)) + guides(colour=FALSE)
