library(dplyr)
library(lubridate)
library("ggplot2")
library("RColorBrewer")

demand=read.csv("demand.csv",header=TRUE,stringsAsFactors = FALSE)
demand$TRAN_DT=as.Date(demand$TRAN_DT)
demand_date =demand%>%
  group_by(TRAN_DT,PRICE_TYPE)%>%
  summarize(total_unit = sum(UNITS),total_demand= sum(DEMAND))%>%
  select(TRAN_DT,total_unit,total_demand,PRICE_TYPE)%>%
  arrange(TRAN_DT)
demand_date$year=year(demand_date$TRAN_DT)
demand_date$year_price= paste(year(demand_date$TRAN_DT), demand_date$PRICE_TYPE, sep="_")

plot_price_type=ggplot(demand_date)  +
  geom_line(aes(x=TRAN_DT,y =total_unit,group=year_price,colour=PRICE_TYPE),size=0.8) +
  xlab("Time") + ylab("Units")+
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "3 months", date_minor_breaks="1 month",date_labels = "%b %Y")+
  ggtitle("Sales Change By Different Price Type Over Time ")+ theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )


plot_total_demand
plot_total_demand+
  scale_y_continuous( sec.axis = sec_axis(~ . /1000, name = "Units"))+
  geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=3500000, group=EVENT_NM,fill=EVENT_NM),alpha=0.5)



#############################################
demand_date =demand%>%
  group_by(TRAN_DT)%>%
  summarize(total_unit = sum(UNITS),total_demand= sum(DEMAND))%>%
  mutate(year=year(TRAN_DT))%>%
  select(TRAN_DT,year,total_unit,total_demand)%>%
  arrange(TRAN_DT)
demand_year= demand_date%>%mutate(newd= TRAN_DT-365*(year-2013))
demand_year$year=as.factor(demand_year$year)

plot_every_year=ggplot(demand_year) +
  geom_line(aes(x=newd, y=total_demand, group=year, colour=year),size=0.8) + xlab("Time") + ylab("Sales")+
  scale_x_date(limits=as.Date(c("2013-01-01","2013-12-31")), date_breaks = "1 months", date_labels = "%b")+
  scale_color_brewer(palette="Set1")+
  ggtitle("Total Sales in Years ")+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text( size=14),
    axis.text.x = element_text(size = 8,  vjust = 0.5, hjust = 0.5))


plot_every_year+
  geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=3500000, group=EVENT_NM),alpha=0.3)





original_price= read.csv("pctlg_sku.csv",header=TRUE,stringsAsFactors = FALSE)
load("/Users/Ruochen/Desktop/Nordstrom/Data/Selection Optimization Capstone/data/price_type.rda")
events= read.csv("events.csv",header=TRUE,stringsAsFactors = FALSE)
price_type$TRAN_DT= as.Date(price_type$DAY_DT)
price_type$SKU_IDNT=as.integer(price_type$SKU_IDNT)


events = events%>%
  mutate(begin = as.Date(DT_BEG_ACT,format= "%m/%d/%Y"),
         end = as.Date(DT_END_ACT,format= "%m/%d/%Y"))
events$begin=as.Date(events$begin)
events$end=as.Date(events$end)
events=events%>%
  filter(!is.na(begin)&!is.na(end))


price_number=demand%>%
  group_by(SKU_IDNT,PRICE_TYPE)%>%
  summarize(u_type=sum(UNITS),a_type=sum(DEMAND),
            n_price = n_distinct(unit_price),
            max_price=max(unit_price),
            min_price=min(unit_price),
            avg_price= mean(unit_price))%>%ungroup()%>%
  arrange(SKU_IDNT)
price_number=price_number %>%left_join(original_price,by='SKU_IDNT')%>%
  select(SKU_IDNT,PRICE_TYPE,n_price,max_price,min_price, avg_price,ORIG_PRC_AMT)

price=demand%>%
  inner_join(price_number,by=c('SKU_IDNT','PRICE_TYPE'))%>%
  mutate(discount=1-(unit_price/ORIG_PRC_AMT),
         discount_back=1-(unit_price/max_price),
         max_ori=max_price-ORIG_PRC_AMT)

price[which(price$max_ori>=0),"discount"]=price[which(price$max_ori>=0),"discount_back"]
price$discount=round(price$discount,2)
price$discount_rate=sprintf("%.0f%%", 100*price$discount)
price$avg_price=round(price$avg_price)
price$DEMAND=round(price$DEMAND)

ggplot(price_number, aes(x=PRICE_TYPE, y=n_price, fill=PRICE_TYPE)) + geom_boxplot() +
  guides(fill=FALSE)+ xlab("Price Type") + ylab("Number of distinct unit price")+
  ggtitle("Numbe of distinct unit price in each price type")

# price changes over time  #58455385 for sku with many kinds of price
sku=demand%>%filter(SKU_IDNT==58455385 )
price_sku_1=ggplot(sku, aes(x=PRICE_TYPE,y=unit_price,fill=PRICE_TYPE)) + 
  geom_boxplot() + xlab("Price Type") + ylab("Price") +
  ggtitle("Price Range for SKU= 58455385")
price_sku_1


price_units=demand%>%group_by(PRICE_TYPE,unit_price)%>%
  summarize(p_units=sum(UNITS),p_amount=sum(DEMAND))

plot_price_unit= ggplot(price_units, aes(x=unit_price, y=p_units, size=0.7)) +
  geom_line() + xlab("Price") + ylab("Units") +facet_grid(PRICE_TYPE ~ .)+
  ggtitle("Price changes over time ")
plot_price_unit


plot_price_unit=ggplot(price_units,aes(x=unit_price, y=p_units,group=PRICE_TYPE,color=PRICE_TYPE),size=0.5) +
  geom_line() +
  xlab("Price") + ylab("Units")+
  facet_grid(PRICE_TYPE ~ .)+
  ggtitle("Units vs Price ")+ theme(
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )

plot_price_amounts=ggplot(price_units) +
  geom_line(aes(x=unit_price, y=p_amount,group=PRICE_TYPE,color=PRICE_TYPE),size=0.5)+
  xlab("Price") + ylab("Demand")+
  facet_grid(PRICE_TYPE ~ .)+
  ggtitle("Demand vs Price ")+ theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=12, face="bold")
  )



library(stringr)
family= str_split_fixed(original_price$CLR_FMLY_DESC , " ",2)
color_p= family[ ,1]
style = family[ ,2]
original_price$color =color_p
original_price$style =style

demand_color=demand%>%inner_join(original_price,by='SKU_IDNT')%>%
  select(-c(CRT_TMSTP,CLR_FMLY_DESC,RCD_UPDT_TMSTP,style))

demand_color2=demand_color%>%group_by(TRAN_DT,color)%>%
  summarize(total_units=sum(UNITS),total_demand=sum(DEMAND))
plot_color= ggplot(demand_color2, aes(x=TRAN_DT, y=total_units, group=color, colour=color)) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), 
               date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales")

plot_color+ scale_color_manual(values=c('#FFCC99','#000000', 'blue','#009999','brown',
                                        '#990066','#CCFFFF','#FF7F50','green',
                                        '#666666', '#FFCC99','#CCCCCC','#FFFFFF','#FF9933',
                                        '#FF6666','#CC0099','#FF0000','#CCFFFF','#FFFF00'))+
  ggtitle("Total sales of different colors over time ")



color_total_sale=demand_color%>%group_by(color)%>%
  summarize(total_unit= 100*sum(UNITS),total_demand=sum(DEMAND))%>%
  arrange(desc(total_unit))
color_total_sale$total_demand=round(color_total_sale$total_demand)
str(color_total_sale)
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

##############################################################################
#Total demand changes by bag style
sku_sale= demand%>% group_by(SKU_IDNT)%>%
  summarize(units_sku=sum(UNITS),amounts_sku=sum(DEMAND))%>%
  arrange(desc(amounts_sku))

hier_sale= hier%>%inner_join(sku_sale,by='SKU_IDNT')
style_demand= demand%>%
  inner_join(hier_sale, by='SKU_IDNT')
style_demand=style_demand%>%
  select(TRAN_DT,SKU_IDNT,UNITS, DEMAND,CLASS_IDNT,CLASS_DESC)%>%
  group_by(TRAN_DT,CLASS_DESC)%>%
  summarize(units_style=sum(UNITS),demand_style=sum(DEMAND))%>%
  arrange(TRAN_DT)

plot_style= ggplot(style_demand, aes(x=TRAN_DT, y=units_style, group=CLASS_DESC, colour=CLASS_DESC)) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), 
               date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales")+
  ggtitle("Total sales of different styles over time ")
plot_style+ scale_color_manual(values=c('green','grey', 'black','brown','red',
                                        'blue','#CC79A7','#009E73','#0072B2',
                                        'yellow','purple','pink'))

ggplot(style_demand, aes(x=CLASS_DESC, y=units_style, fill=CLASS_DESC)) + geom_boxplot() 
##############################################################################
#Total demand changes by 6 most popular brand
brand_sale = demand%>%
  inner_join(hier, by='SKU_IDNT')%>%
  select(TRAN_DT,SKU_IDNT,UNITS, DEMAND,BRAND_NAME)
brand_sale= brand_sale%>%
  group_by(BRAND_NAME)%>%
  summarize(sale_brand =sum(UNITS))%>%
  distinct()%>%
  arrange(desc(sale_brand))


brand_popular=hier[hier$BRAND_NAME %in% c("MICHAEL KORS",
                                          "TORY BURCH","KATE SPADE",
                                          "LONGCHAMP","REBECCA MINKOFF",
                                          "MARC JACOBS"), ]
brand_demand=demand%>%inner_join(brand_popular, by='SKU_IDNT')

brand_demand=brand_demand%>%
  group_by(TRAN_DT,BRAND_NAME)%>%
  summarize(total_unit =sum(UNITS),
            total_amount=sum(DEMAND))

plot_brand= ggplot(brand_demand, aes(x=TRAN_DT, y=total_unit, group=BRAND_NAME, colour=BRAND_NAME)) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), 
               date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales")
#########################
history=demand%>%
  group_by(SKU_IDNT)%>%
  summarize(start_t=min(TRAN_DT),end_t=max(TRAN_DT))%>%
  mutate(time_sale=difftime(end_t,start_t-1, units="days"))%>%
  ungroup()%>%
  arrange(time_sale)
history$total_t=as.numeric(history$time_sale)
ggplot(history, aes(x=total_t)) +
  geom_histogram(aes(y=..density..),      
                 binwidth=30,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  + 
  xlab("The length of Sale history") + ylab("Counts") +
  ggtitle("The distribution of sale history  ")
