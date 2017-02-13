setwd("~/Desktop/Nordstrom/Data/Selection Optimization Capstone/data")
hier= read.csv("hier.csv",header=TRUE,stringsAsFactors = FALSE)
str(hier)


#most popular brand and the corresponsible sale number
brand_sale = demand%>%
  inner_join(hier, by='SKU_IDNT')%>%
  select(TRAN_DT,SKU_IDNT,UNITS, DEMAND,BRAND_NAME)
brand_sale= brand_sale%>%
  group_by(BRAND_NAME)%>%
  summarize(sale_brand =sum(UNITS))%>%
  distinct()%>%
  arrange(desc(sale_brand))


sku_brand= hier%>%
  group_by(BRAND_NAME)%>%
  summarize(num_sku= n())%>%
  arrange(desc(num_sku))

#most popular style and the corresponsible sale number
style_sale= demand%>%
  inner_join(hier, by='SKU_IDNT')%>%
  select(TRAN_DT,SKU_IDNT,UNITS, DEMAND,STYLE_GROUP_IDNT,STYLE_GROUP_DESC,STYLE_GROUP_SHORT_DESC)
style_sale= style_sale%>%
  group_by(STYLE_GROUP_IDNT)%>%
  mutate(sale_style =sum(UNITS))%>%
  ungroup()%>%
  select(STYLE_GROUP_IDNT,STYLE_GROUP_DESC,STYLE_GROUP_SHORT_DESC, sale_style)%>%
  distinct()%>%
  arrange(desc(sale_style))

#### most popular color and the corresponsible sale number

#split the color description as two strings: color 1  and color2
library(stringr)
color= str_split_fixed(hier$SUPP_COLOR  , "/",2)
color1= color[ ,1]
color2= color[ ,2]
hier$color1 =color1
hier$color2=color2


color_sale = demand%>%
  inner_join(hier, by='SKU_IDNT')%>%
  select(TRAN_DT,SKU_IDNT,UNITS, DEMAND,COLR_IDNT,COLR_DESC,color1,color2)


#group the sales by color 1
color_sale= color_sale%>%
  group_by(color1)%>%
  mutate(sale_colr =sum(UNITS))%>%
  ungroup()%>%
  select(color1, color2, sale_colr)%>%
  distinct()%>%
  arrange(desc(sale_colr))
head(color_sale)
hist(color)

color1_sale= color_sale%>%
  select(color1,sale_colr)%>%
  distinct()

library("ggplot2")
ggplot(data = color_sale[2:3], aes(x = color_sale$COLR_DESC)) + geom_histogram(binwidth = 10)


ggplot(data=color_sale[2:3], aes(x=COLR_DESC, y=sale_colr, fill=sale_colr)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)

ggplot(data=color1_sale,aes(x=reorder(color1,sale_colr),y=sale_colr, fill=sale_colr)) + 
  geom_bar(colour="black", stat='identity') +
  ylim(0,12000) +
  xlab("colors") + ylab("Total Sales") +
  ggtitle("Total sales for different colors")


###
color_time= color_sale%>%





