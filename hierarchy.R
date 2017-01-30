setwd("~/Desktop/Nordstrom/Selection Optimization Capstone/data")
hier= read.csv("hier.csv",header=TRUE,stringsAsFactors = FALSE)
str(hier)


#create the brand aggreagation

sku_brand= hier%>%
  group_by( BRAND_NAME)%>%
  summarize(num_sku= n())%>%
  arrange(desc(num_sku))


style= hier%>%
  group_by(STYLE_GROUP_IDNT, SUPP_COLOR)%>%
  summarize(n_style = n())%>%
  arrange(desc(n_style))


gets