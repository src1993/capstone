library(dplyr)
library(geosphere)
library(tidyr)
library(lubridate)

library("grDevices")
library("forecast")
library("TTR")
library(zoo)

library(ggplot2)
library(maps)
library(ggmap)
setwd("~/Desktop/Nordstrom/Data/Selection Optimization Capstone/data")
tran= read.csv("tran.csv",header=TRUE,stringsAsFactors = FALSE)
tran= data.frame(tran,stringsAsFactors = FALSE)

str(tran)

tran <- tran %>%
    mutate(TRAN_DT = as.Date(TRAN_DT),
           UNITS = as.numeric(UNITS),
           DEMAND = as.numeric(DEMAND),
           RTRN_UNITS = as.numeric(RTRN_UNITS),
           RTRN_AMT = as.numeric(RTRN_AMT))

#"nil" is coerced to "NA" when changing from "chr" to "numeric"
tran%>%filter(is.na(UNITS))%>%
  summarize(n_demand_none=n())

# demand amount
demand = tran %>%
  filter(!is.na(UNITS) & !is.na(DEMAND)) %>%
  select(TRAN_DT, SKU_IDNT, UNITS, DEMAND) %>%
  mutate(tran_date = as.POSIXct(as.character(TRAN_DT),format= "%Y-%m-%d"),
         unit_price = (DEMAND/UNITS))%>%
  arrange(tran_date)

head(demand)


############   create the zoo time series
z_d_unit =read.zoo(demand[ ,1:3], split = "SKU_IDNT", index = "TRAN_DT", FUN = identity)
z_d_amount=read.zoo(demand[ ,c(1,2,4)], split = "SKU_IDNT", index = "TRAN_DT", FUN = identity)
z_d_price=read.zoo(demand[ ,c(1,2,6)], split = "SKU_IDNT", index = "TRAN_DT", FUN = identity)

#check the regularity of the series
is.regular(z_d_unit, strict = TRUE)
is.regular(z_d_amount, strict = TRUE)


### after some sample, some SKU has short history of selling
# should combine the "hierarchy.csv" to analyze further
z_try=z_d_unit[, 1:2]

plot(z_try, type = "l", lty = 1:2)

z2= window(z_try[, 1], start = as.Date("2014-03-01"), end =as.Date("2016-03-01"))
summary(z2)

demand$sku=as.factor(demand$SKU_IDNT)
str(demand)
### create price bucket
price=demand%>%
  group_by(SKU_IDNT,unit_price)%>%
  summarize(sale_amount= sum(UNITS))%>%
  ungroup()%>%
  select(SKU_IDNT,unit_price,sale_amount)%>%
  arrange(SKU_IDNT)%>%
  distinct
head(price)


l1=lm(price$sale_amount~price$unit_price)
summary(l1)

price_number = price%>%
  group_by(SKU_IDNT)%>%
  summarize(nprice=n())%>%
  arrange(desc(nprice))


head(price_number)
demand%>%filter(SKU_IDNT=='52901117', unit_price=='140.8571')

#after filter, we can find some strange price for item


#########################SMA
sma_try= function(y)
  SMA(y, n=30)


#########################


# Aggregate the demand by year
demand_year = demand%>% group_by(year(demand$tran_date))%>%
  summarize(total_d=sum(UNITS))
year_demand
# Aggregate the demand by month
demand_time = demand%>%
  mutate(d_year=year(demand$tran_date), 
         d_month=month(demand$tran_date),
         d_week= week(demand$tran_date),
         d_day=day(demand$tran_date))

demand_month = demand_time%>% 
  group_by(d_year,d_month,SKU_IDNT)%>%
  summarize(units_month=sum(UNITS),
            amounts_month= sum(DEMAND))



month.ts = ts(month_demand$total_d, frequency=12, start = c(2014,2))
month.ts
plot.ts(month.ts)


# return amount
return = tran %>%
  filter(!is.na(RTRN_UNITS) & !is.na(RTRN_AMT))
return = return %>%
  mutate(tran_date = as.POSIXct(as.character(TRAN_DT),format= "%Y-%m-%d"),
         return_price = ((0-RTRN_AMT)/RTRN_UNITS))%>%
  select(TRAN_DT, tran_date,SKU_IDNT,RTRN_UNITS, RTRN_AMT,return_price)%>%
  arrange(tran_date)

return_time = return%>%
  mutate(r_year= year(return$tran_date),
         r_month = month(return$tran_date),
         r_week= week(return$tran_date),
         r_day = day(return$tran_date))

# Aggregate the return by month
return_month = return_time%>%
  group_by(r_year,r_month,SKU_IDNT)%>%
  summarize(r_units_month= sum(RTRN_UNITS),
            r_amounts_month=sum(RTRN_AMT))

# intersect amount(needs to be inspected)
inters =tran%>%filter(!is.na(RTRN_UNITS) & !is.na(DEMAND))

# order the most popularSKU


sku_sale= demand%>% group_by(SKU_IDNT)%>%
  summarize(sale_sku=sum(UNITS))%>%
  arrange(desc(sale_sku))
sku_return= return%>% group_by(SKU_IDNT)%>%
  summarize(return_sku=sum(RTRN_UNITS))%>%
  arrange(desc(return_sku))


sku_popular= sku_sale%>%left_join(sku_return, by=c("SKU_IDNT"))%>%
  mutate(popular_sku=(sale_sku-return_sku))%>%
  arrange(desc(popular_sku))
head(sku_popular)


#aggregate by month and calculate the net demand and amounts

join_month = demand_month%>%
  left_join(return_month, by=c('SKU_IDNT','d_year'='r_year', 'd_month'='r_month'))


net_demand_month= join_month%>%
  mutate(r_units_month = ifelse(is.na(r_units_month ),0,r_units_month ),
         r_amounts_month=ifelse(is.na(r_amounts_month),0,r_amounts_month),
         net_number = units_month-r_units_month,
         net_amounts = amounts_month+r_amounts_month)%>%
  select(d_year,d_month,net_number,net_amounts)# return amount is a negative value
   
# returns cannot be subtratced from demand in a month  
df =demand_month%>%
  full_join(return_month, by=c('SKU_IDNT','d_year'='r_year', 'd_month'='r_month'))
return_left_month = df%>%
  filter( is.na(units_month )& is.na(amounts_month) )%>%
  select(d_year, d_month, SKU_IDNT,r_units_month, r_amounts_month)

max(return_left_month$r_units_month)

save.image(file="transaction.RData")


save.image()
load(".RData")
