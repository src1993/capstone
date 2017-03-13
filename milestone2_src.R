library(dplyr)
library(dplyr)
library(tidyr)
library(lubridate)
library("ggplot2")
library("RColorBrewer")

demand_all=read.csv("demand_m1.csv",header=TRUE)
demand_all=demand_all%>%select(-X)
demand_all$TRAN_DT=as.Date(demand_all$TRAN_DT)

events= read.csv("events.csv",header=TRUE,stringsAsFactors = FALSE)

events = events%>%
  mutate(begin = as.Date(DT_BEG_ACT,format= "%m/%d/%Y"),
         end = as.Date(DT_END_ACT,format= "%m/%d/%Y"))
events=events%>%select(-c( 
  YR_454, DT_BEG_ACT, DT_END_ACT ))%>%filter(!is.na(begin))

demand=read.csv("demand.csv",header=TRUE,stringsAsFactors = FALSE)
#####	Total demand changes over time (aggregate by time with events)
demand_date =demand_all%>%
  group_by(TRAN_DT)%>%
  summarize(total_unit = sum(UNITS),total_demand= sum(DEMAND))%>%
  select(TRAN_DT,total_unit,total_demand)%>%
  arrange(TRAN_DT)
plot_total_demand=ggplot(demand_date) +
  geom_line(aes(x=TRAN_DT,y=total_demand, colour = "total_demand"),size=1) +
  geom_line(aes(x=TRAN_DT,y =total_unit*100, colour = "total_unit"),size=1)+
  xlab("Time") + ylab("Sales")+
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "3 months", date_labels = "%b %Y")+
  ggtitle("Total Sales Over Time ")+ 
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )+guides(colour = guide_legend(title = NULL))+
  scale_colour_discrete(breaks=c("total_demand","total_unit"),
                        labels=c("Total Amount", "Total Units"))
plot_total_demand+
  scale_y_continuous( sec.axis = sec_axis(~ . /1000, name = "Units"))+
  geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=3500000),alpha=0.3)




demand_all$dept[which(demand_all$DEPT_IDNT==470)]="Contemporary"
demand_all$dept[which(demand_all$DEPT_IDNT==4)]="Bridge"





#  see the every year changes
demand_date$year=year(demand_date$TRAN_DT)
demand_year= demand_date%>%mutate(newd= TRAN_DT-365*(year-2013))
demand_year$year=as.factor(demand_year$year)
plot_every_year=ggplot(demand_year) +
  geom_line(aes(x=newd, y=total_unit, group=year, colour=year),size=0.8) + xlab("Time") + ylab("Sales")+
  scale_x_date(limits=as.Date(c("2013-01-01","2013-12-31")), date_breaks = "1 months", date_labels = "%b")+
  scale_color_brewer(palette="Set1")+
  ggtitle("Total Sales in Years ")+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text( size=14),
    axis.text.x = element_text(size = 8,  vjust = 0.5, hjust = 0.5))


plot_every_year+
  geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=20000, group=EVENT_NM),alpha=0.3)

#colorful with events

plot_every_year+
  geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=20000, group=EVENT_NM, fill=EVENT_NM),alpha=0.2)



demand_date2=demand_all%>%
  group_by(TRAN_DT,dept)%>%
  summarize(total_unit = sum(UNITS),total_demand= sum(DEMAND))%>%
  arrange(TRAN_DT)
demand_date2$year=year(demand_date2$TRAN_DT)


# with dpartment 

plot_dept=ggplot(demand_date2) +
  geom_line(aes(x=TRAN_DT,y=total_unit,group=dept, colour=dept ),size=1) +
  xlab("Time") + ylab("Sales")+
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "3 months", date_labels = "%b %Y")+
  ggtitle("Total Sales Over Time in 2 department")+ theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )

plot_dept

demand_year2= demand_date2%>%
  mutate(newd= TRAN_DT-365*(year-2013))
demand_year2$year=as.factor(demand_year2$year)
plot_every_year2=ggplot(demand_year2) +
  geom_line(aes(x=newd, y=total_unit, group=year, colour=year),size=0.8) + xlab("Time") + ylab("Sales")+
  scale_x_date(limits=as.Date(c("2013-01-01","2013-12-31")), date_breaks = "1 months", date_labels = "%b")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Total Sales from Year to Year in 2 department ")+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text( size=14),
    axis.text.x = element_text(size = 8,  vjust = 0.5, hjust = 0.5))+
  facet_grid(dept~.)


plot_every_year2+
  geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=10000, group=EVENT_NM),alpha=0.3)



#find out the most popular 6 SKU to see their sale patterns
sku_sale=demand_all%>%group_by(SKU_IDNT,DEPT_DESC)%>%
  summarize(total_unit=sum(UNITS))%>%arrange(desc(total_unit))

sku_top6 = demand_all[demand_all$SKU_IDNT %in% c('52900411','52901117','57408006',
                                                 '49578431','54908692','53607231'), ]
top_6=c('52900411','52901117','57408006','49578431','54908692','53607231')
rank=c("top#1","top#2","top#3","top#4","top#5","top#6")

for (i in 1:6){
  sku_top6[which(sku_top6$SKU_IDNT == top_6[i]),"ranking"]=rank[i]
}


plot_top6= ggplot(sku_top6, aes(x=TRAN_DT, y=UNITS, group=ranking,colour=ranking),size=1.2) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales") +
  scale_color_brewer(palette="Accent",direction = -1)+
  ggtitle("Sales of top 6 most popular items over time ")+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )
plot_top6




###### to see the price 
demand_date3=demand_all%>%
  group_by(TRAN_DT,PRICE_TYPE)%>%
  summarize(total_unit = sum(UNITS),total_demand= sum(DEMAND))%>%
  select(TRAN_DT,total_unit,total_demand,PRICE_TYPE)%>%
  arrange(TRAN_DT)
demand_date3$year=year(demand_date3$TRAN_DT)
demand_date3$year_price= paste(year(demand_date3$TRAN_DT), demand_date3$PRICE_TYPE, sep="_")

plot_price_type=ggplot(demand_date3)  +
  geom_line(aes(x=TRAN_DT,y =total_unit,group=year_price,colour=PRICE_TYPE),size=0.8)+
  xlab("Time") + ylab("Units")+
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), date_breaks = "3 months", date_minor_breaks="1 month",date_labels = "%b %Y")+
  ggtitle("Demand in Different Price Type over Time ")+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )

plot_price_type+geom_rect(data=events,aes(xmin=begin, xmax=end, ymin=0, ymax=15000, group=EVENT_NM),alpha=0.3)



# unit_price boxplot by 3 price_types

ggplot(demand_all, aes(x=PRICE_TYPE, y=unit_price, fill=PRICE_TYPE)) + geom_boxplot() +
  guides(fill=FALSE)+
  xlab("Price Type") + ylab("Unit Price")+
  ggtitle("Unit Price for Different Price Type" )+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5))

ggplot(demand_all, aes(x=PRICE_TYPE, y=discount, fill=PRICE_TYPE)) + geom_boxplot() +
  guides(fill=FALSE)+
  xlab("Price Type") + ylab("Discount Rate")+
  ggtitle("Discount Rate for Different Price Type" )+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5))




#price vs units/demand 

price_units1=demand_all%>%group_by(unit_price)%>%
  summarize(p_units=sum(UNITS),p_amount=sum(DEMAND))
price_units1%>%filter(p_units==max(price_units1$p_units))
ggplot(price_units1,aes(x=log(unit_price), y=p_units)) +
  geom_line()+
  xlab("log of unit price") + ylab("Units")+
  ggtitle("Units vs Price ")

price_units=demand_all%>%group_by(PRICE_TYPE,unit_price)%>%
  summarize(p_units=sum(UNITS),p_amount=sum(DEMAND))


plot_price_unit=ggplot(price_units,aes(x=unit_price, y=p_units,group=PRICE_TYPE,color=PRICE_TYPE),size=0.7) +
  geom_line() +
  xlab("Price") + ylab("Units")+
  facet_grid(PRICE_TYPE ~ .)+
  ggtitle("Units vs Price ")+ theme(
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )



sku1=demand_all%>%filter(SKU_IDNT==50448125 )
price_sku_1=ggplot(sku1, aes(x=PRICE_TYPE,y=unit_price,fill=PRICE_TYPE)) + 
  geom_boxplot() + xlab("Price Type") + ylab("Price") +
  ggtitle("Price Range for SKU= 50448125")
price_sku_1



sku2=demand_all%>%filter(SKU_IDNT==58455385 )

price_sku_2=ggplot(sku2, aes(x=PRICE_TYPE,y=unit_price,fill=PRICE_TYPE)) + 
  geom_boxplot() + xlab("Price Type") + ylab("Price") +
  ggtitle("Price Range for SKU= 58455385")
price_sku_2

#################################################################
##### analyze the handbags attributes
##################################################################

##########################
#color
#########################

demand_color=demand_all%>%group_by(TRAN_DT,color,dept)%>%
  summarize(total_units=sum(UNITS),total_demand=sum(DEMAND))
plot_color= ggplot(demand_color, aes(x=TRAN_DT, y=total_units, group=color, colour=color)) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), 
               date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales")+
  facet_grid(dept~.)

plot_color+ scale_color_manual(values=c('#FFCC99','#000000', 'blue','#009999','brown',
                                        '#990066','#CCFFFF','#FF7F50','green',
                                        '#666666', '#FFCC99','#CCCCCC','#FFFFFF','#FF9933',
                                        '#FF6666','#CC0099','#FF0000','#CCFFFF','#FFFF00'))+
  ggtitle("Total sales of different colors over time ")+ theme(
    legend.text =element_text( size=6),
    legend.title=element_text( size=8,face="bold"),
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )


color_total_sale=demand_all%>%group_by(color)%>%
  summarize(total_unit= 100*sum(UNITS),total_demand=sum(DEMAND))%>%
  arrange(desc(total_unit))
color_total_sale$total_demand=round(color_total_sale$total_demand)

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


##########################
# style
#########################


style_demand= demand_all%>%
  group_by(TRAN_DT,style,dept)%>%
  summarize(units_style=sum(UNITS),demand_style=sum(DEMAND))%>%
  arrange(TRAN_DT)

plot_style= ggplot(style_demand, aes(x=TRAN_DT, y=units_style, group=style, colour=style)) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), 
               date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales")+
  ggtitle("Total sales of different styles over time ")+
  facet_grid(dept~.)
plot_style+ scale_color_manual(values=c('green','grey', 'black','brown','red',
                                        'blue','#CC79A7','#009E73','#0072B2',
                                        'yellow','purple','pink'))+
  guides(colour = guide_legend(title = NULL))+ theme(
    legend.text =element_text( size=6),
    legend.title=element_text( size=8,face="bold"),
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )
ggplot(style_demand, aes(x=style, y=units_style, fill=style)) + geom_boxplot() 



style_total_sale=demand_all%>%group_by(style)%>%
  summarize(total_unit= 100*sum(UNITS),total_demand=sum(DEMAND))%>%
  arrange(desc(total_unit))
style_total_sale$total_demand=round(style_total_sale$total_demand)

library(reshape2)
dfm2 <- melt(style_total_sale[,c('style','total_unit','total_demand')],id.vars = 1)

#####style with sale
ggplot(dfm2,aes(x = reorder(style,value),y = value)) + 
  geom_bar(aes(fill = variable),position = "dodge",stat="identity")+
  xlab("Handbag Style") + ylab("Amount of Sales")+
  scale_y_continuous( sec.axis = sec_axis(~ . /100, name = "Units of sales"))+
  ggtitle("Total Sales of Different Handbag Style ")+
  guides(colour = guide_legend(title = NULL))+
  scale_fill_manual(values=c("#999999", "#E69F00"),
                    name=NULL)+
  theme(
    plot.title = element_text( size=14, face="bold",hjust=0.5),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text( size=14, face="bold"),
    axis.text.x = element_text(size = 8,  vjust = 0.5, hjust = 0.5, angle = 45),
    legend.text = element_text( size = 10),
    legend.position="top"
  )



########################################
###  Brand 
##################################

brand_sale= demand_all%>%
  group_by(BRAND_NAME)%>%
  summarize(sale_brand =sum(UNITS))%>%
  distinct()%>%
  arrange(desc(sale_brand))

head(brand_sale)
brand_popular=subset(demand_all,BRAND_NAME=="MICHAEL KORS"|
                       BRAND_NAME=="TORY BURCH"|
                       BRAND_NAME=="KATE SPADE"|
                       BRAND_NAME=="LONGCHAMP"|
                       BRAND_NAME=="REBECCA MINKOFF"|
                       BRAND_NAME== "MARC JACOBS")

brand_demand=brand_popular%>%
  group_by(TRAN_DT,BRAND_NAME)%>%
  summarize(total_unit =sum(UNITS),
            total_amount=sum(DEMAND))

plot_brand= ggplot(brand_demand, aes(x=TRAN_DT, y=total_unit, group=BRAND_NAME, colour=BRAND_NAME),size=0.8) +
  scale_x_date(limits=as.Date(c("2013-01-01","2017-03-01")), 
               date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales")

plot_brand+
  facet_grid(BRAND_NAME~.)+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Total sales of different tyles over time ")+theme(
    legend.text =element_text( size=8),
    legend.title=element_text( size=8,face="bold"),
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )
#########################
# sale time 
history=demand_all%>%
  group_by(SKU_IDNT,dept)%>%
  summarize(start_t=min(TRAN_DT),end_t=max(TRAN_DT))%>%
  mutate(total_t=difftime(end_t,start_t-1, units="days"))%>%
  ungroup()%>%
  arrange(total_t)
history$total_t=as.numeric(history$total_t)
ggplot(history, aes(x=total_t,colour=dept)) +
  geom_histogram(aes(y=..density..,fill=dept),      
                 binwidth=30,
                 colour="black")+
  geom_density(stat = "density", position = "identity")  + 
  xlab("The length of Sale history") + ylab("Density") +
  ggtitle("The distribution of sale history  ")
ggplot(diamonds, aes(carat)) +
  geom_density(adjust = 1/5)

####################################
##  Weekday Effects

demand_wy=demand_all%>%mutate(weekdays=weekdays(TRAN_DT))%>%
  group_by(weekdays,SKU_IDNT)%>%
  summarize(u=mean(UNITS),d=mean(DEMAND))

ggplot(demand_wy, aes(x=weekdays, y=u, fill=weekdays)) + geom_boxplot() +
  scale_colour_discrete(name = "Weekdays",
                        breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                        labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

demand_wday=demand_all%>%
  group_by(year,month, weekdays)%>%summarize(total_units=sum(UNITS))%>%filter(year<=2016)


plot_wday= ggplot(demand_wday) +
  geom_line(aes(x=month, y=total_units, group=weekdays,colour=weekdays),size=0.8) +
  facet_grid(year~.)+
  scale_x_discrete(limits=1:12)+
  scale_colour_discrete(name = "Weekdays",
                        breaks=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                        labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
  
  xlab("Month") + ylab("Sales")+ ggtitle(" Weekdays' effects on demand")+theme(
    legend.text =element_text( size=8),
    legend.title=element_text( size=8,face="bold"),
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )


brand_price=demand_all%>%
  group_by(BRAND_NAME,unit_price)%>%
  summarize(total_units=sum(UNITS),total_demand=sum(DEMAND))%>%
  arrange(desc(total_units))
brand_price=brand_price%>%select(BRAND_NAME,total_units,unit_price)
myPalette <- colorRampPalette(rev(brewer.pal(8, "Oranges")), space="Lab")


brand_price=brand_price[brand_price$BRAND_NAME%in%c("MICHAEL KORS",
                                                    "TORY BURCH","KATE SPADE",
                                                    "LONGCHAMP","REBECCA MINKOFF",
                                                    "MARC JACOBS"), ]


ggplot(brand_price,aes(x=unit_price, y=total_units, group=BRAND_NAME,colour=BRAND_NAME))+
  geom_point() +
  geo_line()+
  xlab("PRICE") + ylab("UNITS")+ ggtitle(" PRICE vs UNITS in BRANDS")+theme(
    legend.text =element_text( size=8),
    legend.title=element_text( size=8,face="bold"),
    plot.title = element_text( size=12, face="bold",hjust=0.5),
    axis.title.x = element_text(size=12, face="bold"),
    axis.title.y = element_text( size=14, face="bold")
  )




