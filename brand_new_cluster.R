library(tidyr)
library(dplyr)
library(lubridate)

#############        final brand cluster model
demand_all=demand_all%>%filter(PRICE_TYPE!="ANNIV")
demand_all$orig_p=round(demand_all$orig_p)


style_l=c( "t_baby","t_back","t_clutch","t_cross","t_hand","t_luggage","t_open",
           "t_pick","t_shoulder","t_small","t_tote","t_wallet" )

style_split=split(demand_all,demand_all$style)
#split by the cluster number s
for (i in 1:12){
  assign(style_l[i],style_split[[i]])
}

style_dataset=list(t_baby,t_back,t_clutch,t_cross,t_hand,t_luggage,t_open,
                   t_pick,t_shoulder,t_small,t_tote,t_wallet)
price_fivenum=tapply(demand_all$orig_p,demand_all$style, fivenum)
head(price_fivenum)

price_fivenum

### price label by original price
a1= c(0,125,250,300,375,400,600) #baby bag 6
a2= c(0,30,50,75,150,200,350,500,1000)   # backpack 8
a3= c(0,40,50,75,150,300,400,450,1000) # clutch 7
a4= c(0,50,100,150,200,300,500,1500)     #crossboy9
a5= c(0,50,100,175,250,300,375,450,700,2500)  #handheld 10
a6= c(0,20,40,100,400,500,2000)#luggage10
a7= c(0,50,150,200,500)  #open 6
a8= c(0,15,30,50,75,100,800) #pick up gift 8
a9= c(0,5,75,125,150,250,350,400,500,1250) #shoulder 10
a10= c(0,30,50,70,100,400) #small 5
a11= c(0,25,75,125,200,300,500,1250) #tote 9
a12= c(0,30,50,100,150,200,300,500) #wallet 8
price=list(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
for(i in 1:12){
  a=price[[i]]
  l=length(price[[i]])-1
  for(j in (1:l)){
    b=seq(a[j],a[j+1])
    style_dataset[[i]][which(style_dataset[[i]]$orig_p%in%b),"price_label"] = j
  }
}

for (i in 1:12){
  assign(style_l[i],style_dataset[[i]])
}

price_df=rbind(t_baby,t_back,t_clutch,t_cross,t_hand,t_luggage,t_open,
         t_pick,t_shoulder,t_small,t_tote,t_wallet)

price_df=data.frame(price_df)

######### brand sale
start=price_df%>%select(SKU_IDNT,BRAND_NAME)%>%distinct()%>%
  left_join(pctlg,by="SKU_IDNT")%>%select(SKU_IDNT,BRAND_NAME,CRT_TMSTP)%>%
  mutate(create_time=as.Date(CRT_TMSTP))%>%
  group_by(BRAND_NAME)%>%
  summarise(DAY_DT=min(create_time))
start=data.frame(start)


brand_sale1=price_df%>%filter(year<2016)%>%group_by(BRAND_NAME)%>%
  summarise(total_units=sum(UNITS))%>%left_join(start,by="BRAND_NAME")%>%
  mutate(t=ifelse(DAY_DT<as.Date("2013-02-01"),37,
                  as.numeric(difftime(as.Date("2016-12-31"),DAY_DT,units="days"))/30))
brand_sale1=data.frame(brand_sale1)
brand_sale1$t=round(brand_sale1$t)
brand_sale1=brand_sale1%>%mutate(unit_month=total_units/t)%>%select(BRAND_NAME,unit_month)


time = price_df%>%filter(year<2016)%>%select(year,month)%>%distinct()

avg=merge(time,brand_sale1)
avg=data.frame(avg)



######## slope difference

start=price_df%>%select(SKU_IDNT,BRAND_NAME)%>%distinct()%>%
  left_join(pctlg,by="SKU_IDNT")%>%select(SKU_IDNT,BRAND_NAME,CRT_TMSTP)%>%
  mutate(create_time=as.Date(CRT_TMSTP))%>%
  group_by(BRAND_NAME)%>%
  summarise(DAY_DT=min(create_time))
start=data.frame(start)

start=start%>%
  left_join(fs, by="DAY_DT")%>%
  mutate(year_s=YR_454,week= WK_IDNT,month_s=MTH_IDNT )%>%
  select(BRAND_NAME,year_s,month_s)
avg=avg%>%left_join(start,by="BRAND_NAME")


for(i in 1:nrow(avg)){
  if (avg$year[i] < avg$year_s[i]){
    avg$unit_month[i] = 0
  }else if(avg$year[i]== avg$year_s[i] &avg$month[i]<avg$month_s[i]){
    avg$unit_month[i] =0
  }
}


avg=avg%>%mutate(time=paste(year,month,sep="_"))%>%
  select(BRAND_NAME,time,unit_month)%>%
  spread(time,unit_month)


table(is.na(avg))


brand_sale=price_df%>%filter(year<2016)%>%group_by(BRAND_NAME,year,month)%>%
  summarise(total_units=sum(UNITS))%>%
  mutate(time=paste(year,month,sep="_"))%>%
  ungroup()%>%
  left_join(brand_sale1,by="BRAND_NAME")%>%
  mutate(difference=(total_units-unit_month))%>%
  select(BRAND_NAME,time,difference)%>%
  spread(time,difference)
brand_sale=data.frame(brand_sale)
table(is.na(brand_sale))


for(i in 1:159){
  for(j in 2:38){
    brand_sale[i,j]=ifelse(is.na(brand_sale[i,j]),-avg[i,j], brand_sale[i,j])
  }
}
head(brand_sale)



#### units difference on the time scale
brand_sale2=price_df%>%filter(year<2016)%>%group_by(BRAND_NAME,year,month)%>%
  summarise(total_units=sum(UNITS))%>%
  mutate(time=paste(year,month,sep="_"))%>%
  ungroup()%>%select(BRAND_NAME,time,total_units)%>%spread(time,total_units)




brand=brand_sale%>%left_join(brand_sale2,by="BRAND_NAME")
brand[is.na(brand)]=0


#####brand color




total=price_df%>%filter(year<2016)%>%group_by(BRAND_NAME)%>%
  summarise(total_units=sum(UNITS))

brand_color=price_df%>%filter(year<2016)%>%group_by(BRAND_NAME,color)%>%summarise(units=sum(UNITS))%>%
  left_join(total,by="BRAND_NAME")%>%mutate(color_ratio=units/total_units)%>%
  select(BRAND_NAME,color,color_ratio)%>%spread(color,color_ratio)
brand_color[is.na(brand_color)]=0



####brand_style_price

style=price_df%>%filter(year<2016)%>%select(style,price_label)%>%distinct()
brand_style=merge(total,style)
brand_style2=price_df%>%group_by(BRAND_NAME,style,price_label)%>%
  summarise(units=sum(UNITS))

brand_style=brand_style%>%
  left_join(brand_style2,by=c("BRAND_NAME","style","price_label"))%>%
  mutate(ratio = units/total_units,p=paste(style,price_label,sep="_"))%>%
  select(BRAND_NAME,p,ratio)%>%
  spread(p,ratio)

brand_style[is.na(brand_style)]=0


############

colnames(brand)

brand=brand%>%left_join(brand_color,by="BRAND_NAME")%>%
  left_join(brand_style,by="BRAND_NAME")

table(is.na(brand))
brand[is.na(brand)]=0
#################################################
library(caret)
preprocess_brand<- preProcess(brand[ ,-1], method=c("center", "scale"))
transformed <- predict(preprocess_brand, brand)
t=data.frame(transformed)
colnames(t)



for( i in 2:38){
  t[[i]]=t[[i]]/(37/194)
}
for( i in 39:75){
  t[[i]]=t[[i]]/(37/184)
}
for( i in 76:93){
  t[[i]]=t[[i]]/((18/184))
}
for( i in 94:174){
  t[[i]]=t[[i]]/(81/173)
}




dist_brand= dist(t[ ,-1], method = "euclidean")
cluster_w = hclust(dist_brand, method = "ward.D2") 


plot(cluster_w,hang=-1)
rect.hclust(cluster_w, k=5, border="red")
cluster5= cutree(cluster_w, k = 5)
library(cluster)
sil_width_brand <- c(NA)
for(i in 2:30){
  silinfo=silhouette(cutree(cluster_w, k = i), dist_brand)[,"sil_width"]
  sil_width_brand[i] <- mean(silinfo)
}
plot(1:30, sil_width_brand, xlab = "Number of clusters of brand",ylab = "Silhouette Width")

lines(1:30, sil_width_brand)


#######################
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
brand_cluster4=brand_cluster%>%
  select(-c(BRAND_NAME,cluster2,cluster3,cluster5,
            cluster6,cluster7,cluster8,cluster9,cluster10))%>%
  group_by(cluster4)%>% 
  summarise_each(funs(mean))

brand_cluster4=data.frame(t(brand_cluster4))
write.csv(brand,file="brand_attribute_final.csv")


write.csv(brand_cluster5,file="brand_cluster5_final.csv")
View(brand_cluster4)

brand_cluster%>%filter(cluster4=="1")%>%select(BRAND_NAME)  #162 cluster1   6 cluster2
#######pca analysis
cluster= cutree(cluster_w, k = 4)
brand$cc=cluster
b_new=brand%>%select(BRAND_NAME,cc)
brand_c=price_df%>%select(BRAND_NAME)%>%distinct()%>%
  left_join(b_new,by="BRAND_NAME")
brand_c$cc[is.na(brand_c$cc)]=1


a=b_new$cc-cluster
write.csv(brand,file="brand_0519.csv") ########### 05.18 new brand 


write.csv(b_new,file="b_new2.csv") ########### 


p=price_df%>%left_join(b,by="BRAND_NAME")%>%group_by(c1)%>%summarise(t=sum(UNITS))
bc%>%filter(cc==3)
library(factoextra)
fviz_nbclust(t[,-1], hcut, method = "wss",k.max = 15) +
  geom_vline(xintercept = 4, linetype = 2)

