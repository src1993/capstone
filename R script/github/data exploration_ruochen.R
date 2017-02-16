library("ggplot2")
library("RColorBrewer")

demand_date =demand%>%
  group_by(TRAN_DT)%>%
  summarize(total_unit = sum(UNITS),total_demand= sum(DEMAND))%>%
  select(TRAN_DT,total_unit,total_demand)%>%
  arrange(TRAN_DT)

startTime <- as.Date("2013-01-01")
endTime <- as.Date("2017-03-01")
# create a start and end time R object
start.end <- c(startTime,endTime)
start.end


# the plot of the total demand (units) over time
plot_total_demand=ggplot() +
  geom_line(data=demand_date, aes(TRAN_DT, total_demand)) + xlab("Time") + ylab("Sales")+
  scale_x_date(limits=start.end, date_breaks = "3 months", date_labels = "%b %Y")+
  ggtitle("Total sales over time ")
plot_total_demand


##the plot of top 6 of the most popular SKU's demand  over time 
sku_top6 = demand[demand$SKU_IDNT %in% c('52900411','52901117','57408006',
                                         '49578431','54908692','53607231'), ]
top_6=c('52900411','52901117','57408006','49578431','54908692','53607231')
rank=c("top#1","top#2","top#3","top#4","top#5","top#6")


for (i in 1:6){
  sku_top6[which(sku_top6$SKU_IDNT == top_6[i]),"ranking"]=rank[i]
}

head(sku_top6)

plot_top6= ggplot(sku_top6, aes(x=DAY_DT, y=UNITS, group=ranking,colour=ranking)) +
  scale_x_date(limits=start.end, date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales") +
  scale_color_brewer(palette="RdBu",direction = -1)+
  ggtitle("Sales of top 6 most popular items over time ")

plot_top6


# the plot of the demand changes over time by color 
#library(stringr)
color= str_split_fixed(hier$SUPP_COLOR  , "/",2)
color1= color[ ,1]
color2= color[ ,2]
hier$color1 =color1
hier$color2=color2

str(hier)
color_sale = demand%>%
  inner_join(hier, by='SKU_IDNT')%>%
  select(TRAN_DT,SKU_IDNT,UNITS, DEMAND,COLR_IDNT,COLR_DESC,color1,color2)


term=c("BLACK", "BROWN","GREY","RED","GREEN","ORANGE","CAMEL","PURPLE","WHITE",
       "PINK","COFFEE","GOLD","NAVY")

for (i in term){
  color_sale[grep(i,color_sale$color1,ignore.case=TRUE), "color_des"]<-i
}

for (i in term){
  color_sale[grep(i,color_sale$COLR_DESC,ignore.case=TRUE), "color_des"]<-i
}

#group the sales by color 1
color_sale2= color_sale%>%
  group_by(TRAN_DT,color_des)%>%
  mutate(sale_colr =sum(UNITS))%>%
  ungroup()%>%
  select(TRAN_DT,color_des,color1, color2, sale_colr)%>%
  distinct()%>%
  arrange(desc(sale_colr))
head(color_sale)
color_des_sale= color_sale2%>%
  select(TRAN_DT,color_des,sale_colr)%>%
  distinct()%>%
  arrange(TRAN_DT)


color_time= ggplot(na.omit(color_des_sale), aes(x=TRAN_DT, y=sale_colr ,group=color_des,colour=color_des),na.rm= TRUE) +
  scale_x_date(limits=start.end, date_breaks = "3 months", date_labels = "%b %Y")+
  geom_line() + xlab("Time") + ylab("Sales") +
  scale_color_manual(values=c('#000000', '#663300','#CC9933','#FFC000',
                              '#33CC33', '#666666','#003366','#FF9900',
                              '#FF6666','#CC0099','#FF3333','#CCCCCC'))+
  ggtitle("Total sales of different colors over time ")
color_time