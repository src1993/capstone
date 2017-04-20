setwd("~/Desktop/Nordstrom/Data/Selection Optimization Capstone/data")
library(tidyr)
library(reshape2)
library(caret)
library(factoextra)
library(dplyr)
library(ggdendro)

pctlg= read.csv("pctlg_sku.csv",header=TRUE,stringsAsFactors = FALSE)
head(pctlg)
brand_t= pctlg%>%mutate(creat_time=as.Date(CRT_TMSTP))%>%select(SKU_IDNT,creat_time)

de
### get the sales for brand in year, month   
demand_all=demand_all%>%left_join(brand_t,by="SKU_IDNT")
brand_attribute=demand_all%>%group_by(BRAND_NAME)%>%
  summarise(n_color=n_distinct(color),
            n_style=n_distinct(style),
            max_price=max(unit_price),
            min_price=min(unit_price),
            avg_price=mean(unit_price),
            max_dis=max(discount),
            min_discount=min(discount),
            active_day_brand=n_distinct(TRAN_DT),
            total_t=difftime(max(TRAN_DT),min(creat_time)-1, units="days") )

brand_attribute$total_t=as.numeric(brand_attribute$total_t)


# event effect 
brand_event=demand_all%>%group_by(BRAND_NAME,EVENT_NM)%>%
  summarise(total_units=sum(UNITS),active_day=n_distinct(TRAN_DT))%>%
  mutate(unit_day=total_units/active_day)%>%
  select(BRAND_NAME,EVENT_NM,unit_day)%>%
  spread(EVENT_NM,unit_day)

brand_event[is.na(brand_event)]=0

brand_attribute=brand_attribute%>%left_join(brand_event,by="BRAND_NAME")

# color effect
brand_color=demand_all%>%group_by(BRAND_NAME,color)%>%
  summarise(total_units=sum(UNITS),active_day=n_distinct(TRAN_DT))%>%
  mutate(unit_day=total_units/active_day)%>%
  select(BRAND_NAME,color,unit_day)%>%
  spread(color, unit_day)

brand_color[is.na(brand_color)]=0


brand_attribute=brand_attribute%>%left_join(brand_color,by="BRAND_NAME")


# style effect
brand_style=demand_all%>%group_by(BRAND_NAME,style)%>%
  summarise(total_units=sum(UNITS),active_day=n_distinct(TRAN_DT))%>%
  mutate(unit_day=total_units/active_day)%>%
  select(BRAND_NAME,style,unit_day)%>%
  spread(style, unit_day)

brand_style[is.na(brand_style)]=0

brand_attribute=brand_attribute%>%left_join(brand_style,by="BRAND_NAME")



####price infor
price_style=demand_all%>%group_by(BRAND_NAME,style,unit_price)%>%
  summarise(total_units=sum(UNITS),active_day=n_distinct(TRAN_DT))%>%
  mutate(unit_day=total_units/active_day)


style_l=c( "t_baby","t_back","t_clutch","t_cross","t_hand","t_luggage","t_open",
           "t_pick","t_shoulder","t_small","t_tote","t_wallet" )

style_split=split(demand_all,demand_all$style)
#split by the cluster number s
for (i in 1:12){
  assign(style_l[i],style_split[[i]])
}

style_dataset=list(t_baby,t_back,t_clutch,t_cross,t_hand,t_luggage,t_open,
                   t_pick,t_shoulder,t_small,t_tote,t_wallet)
price_fivenum=tapply(demand_all$unit_price,demand_all$style, fivenum)


for(i in 1:12){
  a=price_fivenum[[i]]
  for(j in 1:4){
    b=seq(a[j],a[j+1])
    style_dataset[[i]][which(style_dataset[[i]]$unit_price%in%b),"price_label"] = j
  }
}

for (i in 1:12){
  assign(style_l[i],style_dataset[[i]])
}

price_df=rbind(t_baby,t_back,t_clutch,t_cross,t_hand,t_luggage,t_open,
               t_pick,t_shoulder,t_small,t_tote,t_wallet)

price_df=data.frame(price_df)

style_price=price_df%>%group_by(BRAND_NAME,style,price_label)%>%
  summarise(total_unit=sum(UNITS),
            active_day=n_distinct(TRAN_DT))%>%
  mutate(unit_day=total_unit/active_day,
         price_label=paste(style,price_label,sep="_"))%>%
  ungroup()%>%
  select(BRAND_NAME,price_label,unit_day)%>%
  spread(price_label,unit_day)

style_price[is.na(style_price)]=0
brand_attribute=brand_attribute%>%left_join(style_price,by="BRAND_NAME")

#############style & color combine


color_style=demand_all%>%
  mutate(color_style=paste(color,style,sep="/"))%>%
  group_by(BRAND_NAME,color_style)%>%
  summarise(total_unit=sum(UNITS),
            active_day=n_distinct(TRAN_DT))%>%
  mutate(unit_day=total_unit/active_day)%>%
  select(BRAND_NAME,color_style,unit_day)%>%
  spread(color_style,unit_day)
head(color_style)

color_style[is.na(color_style)]=0
brand_attribute=brand_attribute%>%left_join(color_style,by="BRAND_NAME")





#######sale infor

brand_new=demand_all%>%group_by(BRAND_NAME)%>%
  summarise(start_t=min(TRAN_DT),end_t=max(TRAN_DT),n=n_distinct(TRAN_DT))%>%
  mutate(onsaletime= as.numeric(difftime(end_t,start_t-1, units="days")),
         active_rate=n/onsaletime)


summary(brand_new)


brand_sale= demand_all%>%group_by(TRAN_DT, BRAND_NAME)%>%
  summarise(total_units=sum(UNITS))
brand_sale=brand_sale%>%left_join(brand_new,by="BRAND_NAME")
head(brand_sale)


b=brand_new%>%select(BRAND_NAME,n,start_t)
b$BRAND_NAME=as.character(b$BRAND_NAME)
a=c(NA)

for( i in 1:nrow(b)){
  a=seq(1:b$n[i])
  brand_sale[which(brand_sale$BRAND_NAME==b$BRAND_NAME[i]),"active_sofar"]=a
}



brand_sale2=transform(brand_sale,time_label=cut(active_sofar,  breaks=seq(0,1470,by=30),
                                      labels=c(1:49)))

brand_sale=brand_sale2%>%group_by(BRAND_NAME,time_label)%>%
  summarise(units=sum(total_units))%>%
  spread(time_label,units)

brand_sale[is.na(brand_sale)]=0

brand_attribute=brand_attribute%>%left_join(brand_sale,by="BRAND_NAME")
brand_attribute=data.frame(brand_attribute)


write.csv(brand_attribute,file="brand_new_attribute.csv")

brand=brand_attribute

library(caret)
preprocess_brand<- preProcess(brand[ ,-1], method=c("center", "scale"))
transformed <- predict(preprocess_brand, brand)
t=data.frame(transformed)
dist_brand= dist(t[ ,-1], method = "euclidean")
cluster_w = hclust(dist_brand, method = "ward.D2") 


plot(cluster_w,hang=-1)
rect.hclust(cluster_w, k=5, border="red")
cluster5 = cutree(cluster_w, k = 5)

sil_width_brand <- c(NA)
for(i in 2:30){
  silinfo=silhouette(cutree(clusterbrand_w, k = i), dist_brand)[,"sil_width"]
  sil_width_brand[i] <- mean(silinfo)
}
plot(1:30, sil_width_brand, xlab = "Number of clusters of brand",ylab = "Silhouette Width")
lines(1:30, sil_width_brand)






###### cut the tree to different clusters and to see the reuslt 

brand_cluster=c("cluster1","cluster2","cluster3","cluster4","cluster5",
                "cluster6","cluster7","cluster8","cluster9","cluster10")

for (i in 1:10){
  assign(brand_cluster[i],cutree(cluster_w, k = i))
}

BRAND_NAME=brand$BRAND_NAME

brand_cluster=data.frame(BRAND_NAME,cluster2,cluster3,cluster4,cluster5,
                         cluster6,cluster7,cluster8,cluster9,cluster10)
for(i in 2:10){
  brand_cluster[[i]]=as.factor(brand_cluster[[i]])
}

head(brand_cluster)
brand_cluster=brand%>%left_join(brand_cluster,by="BRAND_NAME")
brand_cluster5=brand_cluster[ ,-c(1,362,363,364,366,367,368,369,370)]%>%
  group_by(cluster5)%>% 
  summarise_each(funs(mean))

brand_cluster5=data.frame(t(brand_cluster5))


#######pca analysis


preprocess_brand2<- preProcess(brand[ ,-1], method=c("center", "scale","pca"))
transformed2 <- predict(preprocess_brand2, brand)
t2=data.frame(transformed2)
dist_brand2= dist(t2[ ,-1], method = "euclidean")

clusterbrand_pca= hclust(dist_brand2, method = "ward.D2")
plot(clusterbrand_pca,hang=-1)
sil_width_brand <- c(NA)
for(i in 2:30){
  silinfo=silhouette(cutree(clusterbrand_pca, k = i), dist_brand)[,"sil_width"]
  sil_width_brand[i] <- mean(silinfo)
}
plot(1:30, sil_width_brand, xlab = "Number of clusters of brand",ylab = "Silhouette Width")
lines(1:30, sil_width_brand)
