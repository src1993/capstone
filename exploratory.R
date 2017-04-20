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

####################################   Read/load data  ###############################################

events <- read.csv('data/events.csv', as.is = T)
fiscal <- read.csv('data/fscl.csv', as.is = T)
hier <- read.csv('data/hier.csv', as.is = T)
catalog <- read.csv('data/pctlg_sku.csv', as.is = T)
load('data/price_type.rda')
tran <- read.csv('data/tran.csv', as.is = T)

####################################  Cleaning/wrangling  ###############################################

#EVENTS

events <- events %>%
    mutate(EVENT = EVENT_NM, 
           start = as.Date(DT_BEG_ACT,
                           format = "%m/%d/%Y"),
           end = as.Date(DT_END_ACT,
                         format = "%m/%d/%Y"),
           duration = end-start+1,
           year = YR_454) %>%
    select(EVENT, start, end, duration, year)

#FISCAL

fiscal <- fiscal %>%
    mutate(DAY_DT = as.Date(DAY_DT)) 

#HIERARCHY

hier <- hier %>%
    mutate(N_BRAND = ifelse(SUPP_NPG_CDE==1,1,0),
           LOAD_DT = as.Date(DM_RECD_LOAD_DT,
                           format = "%Y-%m-%d"),
           DEPT = as.factor(ifelse(DEPT_DESC=='CONTEMP HBG/SLG','Contemporary','Bridge')),
           CLOSE_DT = pmin(as.Date(max(DM_RECD_LOAD_DT)),as.Date(DM_RECD_CLOSE_DT)),
           LENGTH = as.numeric(CLOSE_DT-LOAD_DT+1)) %>%
    select(SKU_IDNT,SUPP_NAME, SUPP_NAME, N_BRAND, BRAND_NAME,
           CLASS_DESC, CLASS_IDNT, DEPT,LOAD_DT, CLOSE_DT, LENGTH)

#CATALOG

catalog <- catalog %>%
    mutate(ORIG_PRICE = ORIG_PRC_AMT,
           COLOR = gsub("TONES","",CLR_FMLY_DESC),
           CREATE_DT = as.Date(CRT_TMSTP)) %>%
    select(SKU_IDNT, ORIG_PRICE, COLOR,CREATE_DT)

#PRICE

price_type <- price_type %>%
    mutate(SKU_IDNT = as.integer(SKU_IDNT))

#TRANSACTIONS

tran <- tran %>%
    mutate(DAY_DT = as.Date(TRAN_DT),
           UNITS = as.numeric(UNITS),
           DEMAND = as.numeric(DEMAND),
           RTRN_UNITS = as.numeric(RTRN_UNITS),
           RTRN_AMT = as.numeric(RTRN_AMT),
           UNIT_PRICE_DEMAND =  ifelse(is.na(UNITS), 0, DEMAND/UNITS) ,
           UNIT_PRICE_RETURN = ifelse(is.na(RTRN_UNITS), 0, -RTRN_AMT/RTRN_UNITS),
           UNIT_PRICE_RETURN = -RTRN_AMT/RTRN_UNITS) %>%
    select(SKU_IDNT, DAY_DT, UNITS, DEMAND, UNIT_PRICE_DEMAND, RTRN_UNITS, RTRN_AMT,UNIT_PRICE_RETURN)
tran[is.na(tran)] <- 0


####################################  Create datasets ###############################################

#Create data frame of dates

DAY <- seq(as.Date(min(tran$DAY_DT)),as.Date(max(tran$DAY_DT)),by = "day")
calendar <- data.frame(DAY = DAY, dummy = TRUE)

day_events <- events %>% 
    mutate(dummy=TRUE) %>%
    right_join(calendar, by = "dummy") %>%
    filter(DAY >=start, DAY<=end) %>%
    select(DAY,EVENT)

# Create product description
description <- hier %>%
    full_join(catalog, by = "SKU_IDNT")

demand <- tran %>%
    subset(UNITS>=1) %>%
    left_join(price_type, by = c("SKU_IDNT", "DAY_DT"))%>%
    mutate(PRICE_TYPE = as.factor(ifelse(is.na(PRICE_TYPE),"REG",PRICE_TYPE)))%>%
    left_join(description, by =c("SKU_IDNT")) %>% 
    left_join(day_events, by = c("DAY_DT"="DAY")) %>%
    mutate(EVENT = as.factor(ifelse(is.na(EVENT),"NONE",EVENT)))%>%
    select(-c(RTRN_UNITS, RTRN_AMT, UNIT_PRICE_RETURN, CLASS_IDNT, LOAD_DT, CLOSE_DT, LENGTH, CREATE_DT))

save(demand, file = 'data/created/demand.Rdata')
save(description, file = 'data/created/information.Rdata')

sku_length <- demand %>%
    group_by(SKU_IDNT) %>%
    summarise(start = min(DAY_DT),
              end = max(DAY_DT)) %>%
    mutate(length = end-start+1)

##

sku_price <- demand %>%
    group_by(SKU_IDNT) %>%
    summarise(MIN_PRICE = min(UNIT_PRICE_DEMAND),
              MAX_PRICE = max(UNIT_PRICE_DEMAND),
              MEAN_PRICE = max(UNIT_PRICE_DEMAND),
              ORIG_PRICE = mean(ORIG_PRICE)) %>%
    mutate(MAX = pmax(MAX_PRICE,ORIG_PRICE))

sum(sku_price$MAX_PRICE > sku_price$ORIG_PRICE) #456 cases where orig price is no the maximum price that shows up

information <- description %>%
    inner_join(sku_price, by = c("SKU_IDNT", "ORIG_PRICE"))


## brand

brand_info <- demand %>%
    filter(PRICE_TYPE %in% c('PROMO','REG')) %>%
    filter(DEPT == 'Bridge') %>% 
    mutate(YEAR = year(DAY_DT)) %>%
    filter(YEAR < 2017) %>%
    group_by(BRAND_NAME, YEAR) %>%
    summarise(TOT_UNITS = sum(UNITS),
              START = min(DAY_DT),
              END = max(DAY_DT))

brand_info2 <- description %>%
    filter(DEPT == 'Bridge') %>% 
    group_by(BRAND_NAME) %>%
    summarise(LOAD = min(LOAD_DT),
              CLOSE = max(CLOSE_DT),
              CREATE = min(CREATE_DT))

brand_join <- brand_info %>%
    left_join(brand_info2, by = "BRAND_NAME") %>%
    mutate(length = as.numeric(END-START))

#################################### EXPLORATORY  ###############################################


demand_agg_date <- demand %>%
    filter(PRICE_TYPE != 'CLRC') %>%  #filter clearance price
    group_by(DAY_DT) %>%
    summarise(TOTAL_UNITS = sum(UNITS),
              TOTAL_DEMAND = sum(DEMAND))

minD <- as.Date(min(tran$DAY_DT))
maxD <- as.Date(max(tran$DAY_DT))
    
p1 <- ggplot(demand_agg_date) + geom_line(aes(x=DAY_DT, y=TOTAL_UNITS), colour='Dark Blue')+
    xlab('')+ylab('Units')+
    scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "4 months", date_labels = "%b %Y")+
    ggtitle("Total Sales Over Time ")+
    theme(
        plot.title = element_text( size=14, face="bold",hjust=0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold")
    )
p1 + geom_rect(data=events,aes(xmin=start, 
                               xmax=end, ymin=0, ymax=max(demand_agg_date$TOTAL_UNITS),fill=EVENT),
               alpha=0.3, na.rm = T)    

demand_agg_date_aux <- demand_agg_date %>% 
    mutate(year = year(DAY_DT)) %>%
    filter(year< 2017) 
year(demand_agg_date_aux$DAY_DT) <- 2000

events_aux <- events 
year(events_aux$start)<-year(events_aux$start)-events_aux$year+2000
year(events_aux$end)<-year(events_aux$end)-events_aux$year+2000
     
p2 <- ggplot(demand_agg_date_aux) + geom_line(aes(x=DAY_DT, y=TOTAL_UNITS), colour='Dark Blue')+
    xlab('')+ylab('Total Units')+
    facet_grid(year~.)+
    scale_x_date(limits=as.Date(c("2000-01-01","2000-12-31")), date_breaks = "1 months", date_labels = "%b")+
    ggtitle("Total Sales Over Time ")+
    theme(
        plot.title = element_text( size=14, face="bold",hjust=0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"))+
    ggtitle("Total Sales in Years ")

p2 + geom_rect(data= subset(events_aux,year<2017),
              aes(xmin=start, xmax=end, ymin=0, ymax=max(demand_agg_date$TOTAL_UNITS), 
                  group=EVENT, fill=EVENT),alpha=0.3, na.rm = T)

#Department

demand_agg_date_dept <- demand %>%
    filter(PRICE_TYPE != 'CLRC') %>%  #filter clearance price
    group_by(DAY_DT, DEPT) %>%
    summarise(TOTAL_UNITS = sum(UNITS),
              TOTAL_DEMAND = sum(DEMAND))

p3 <- ggplot(demand_agg_date_dept) + geom_line(aes(x=DAY_DT, y=TOTAL_UNITS,color=DEPT))+
    xlab('')+ylab('Units')+guides(color=F)+
    facet_grid(DEPT~.)+
    scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "6 months", date_labels = "%b %Y")+
    ggtitle("Total Sales Over Time by department")+
    theme(
        plot.title = element_text( size=14, face="bold",hjust=0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold")
    )
p3

### BAG attributes

# COLOR

demand_color <- demand %>%
    group_by(COLOR) %>%
    summarise(total_units = sum(UNITS)) 

color.count <- demand_color$total_units
names(color.count) <- demand_color$COLOR

pareto.chart(color.count) 
abline(h=(sum(color.count)*.8),col="blue",lwd=2)

library(reshape2)
dfm <- melt(color_total_sale[,c('color','total_unit','total_demand')],id.vars = 1)
#####color with sale
ggplot(dfm,aes(x = reorder(color,value),y = value)) + 
    geom_bar(aes(fill = variable),position = "dodge",stat="identity")+
    xlab("Color") + ylab("Amount of Sales")+
    scale_y_continuous( sec.axis = sec_axis(~ . /100, name = "Units of sales"))+
    ggtitle("Total Sales of Different Color ")+
    guides(colour = guide_legend(title = NULL))+theme(
        plot.title = element_text( size=14, face="bold",hjust=0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text( size=14, face="bold"),
        axis.text.x = element_text(size = 10,  vjust = 0.5, hjust = 0.5, angle = 30),
        legend.text = element_text( size = 10),
        legend.justification=c(0.02,0.95), legend.position=c(0.02,0.95)
    )




####################################   ###############################################

trial1$PRICE_TYPE <- ifelse(is.na(trial1$PRICE_TYPE),"REG",trial1$PRICE_TYPE)


trial1 <- trial1 %>%
    left_join(catalog, by = c("SKU_IDNT"))

trial2 <- subset(trial1, UNIT_PRICE_DEMAND > ORIG_PRICE)




####################################  Cleaning/wrangling  ###############################################


headSKU <- unique(tran$SKU_IDNT)
sum(!SKU %in% hier$SKU_IDNT) #all apear in hierarchy
sum(!SKU %in% catalog$SKU_IDNT) #21 don't apear in hierarchy







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
