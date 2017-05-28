price_df=read.csv("price_df.csv",header = TRUE,stringsAsFactors = FALSE)

price_df=price_df%>%left_join(brand_c,by="BRAND_NAME")%>%
  left_join(new_color,by="color")

df=df%>%left_join(brand_c,by="BRAND_NAME")%>%
  left_join(new_color,by="color")%>%
  mutate(IDNT=paste(price_label,style,cc,new_color,sep = "_"))%>%
  left_join(fs,by=c("TRAN_DT"="DAY_DT"))

dept_df=df%>%select(IDNT,dept)%>%distinct()
contem_df= dept_df%>%filter(dept=="Contemporary") 

nrow(contem_df)



#new_color5=new_color
#new_color4=new_color

#brand_contem_c3=brand_c
#brand_contem_c4=brand_c



id=df%>%select(IDNT,price_label,style,new_color,cc)%>%distinct() ########769 id


calender=fs%>%select(YR_454,WK_IDNT,MTH_IDNT)%>%distinct()%>%
  rename(year=YR_454,month=MTH_IDNT,week=WK_IDNT)
y=c(2013,2014,2015,2016)
w=c(1:52)
t=c(2012,53)
time=merge(y,w)
time=rbind(t,time)


time=merge(time,id)
time=time%>%rename(year=x,week=y)%>%
  left_join(calender,by=c("year","week"))




df_w=df%>%select(YR_454,WK_IDNT,MTH_IDNT,IDNT,UNITS,style,cc,price_label,new_color)%>%
  rename(year=YR_454,month=MTH_IDNT,week=WK_IDNT)%>%
  group_by(year,month,week,IDNT,style,cc,price_label,new_color)%>%
  summarise(units=sum(UNITS))
df_w=data.frame(df_w)

df_infor=df%>%
  group_by(IDNT)%>%
  summarise(max_price_all=max(unit_price), min_price_all=min(unit_price),
            avg_price_all=mean(unit_price),max_orig=max(orig_p),
            avg_orig=mean(orig_p),min_orig=min(orig_p))
df_w=df_w%>%left_join(df_infor,by="IDNT")


sale=df_w%>%select(year,week,units,IDNT)%>%distinct()
time=time%>%left_join(sale,by=c("year","week","IDNT"))
time$units[is.na(time$units)]=0




###################  last year's sale
df_w=time
df_w$week[which(df_w$week==53)]=52

d=df_w%>%select(year,week,IDNT,units)%>%rename(u_year=units)
d$year=d$year+1
df_w=df_w%>%left_join(d,by=c("IDNT","year","week"))


d1=df_w%>%select(year,week,IDNT,units)%>%rename(u1ag1_year=units)
d1$week=d1$week+1
d1$year=d1$year+1
d1$year[which(d1$week==53)]=d1$year[which(d1$week==53)]+1
d1$week[which(d1$week==53)]=1
df_w=df_w%>%left_join(d1,by=c("IDNT","year","week"))


d2=d1%>%select(year,week,IDNT,u1ag1_year)%>%rename(u1ag2_year=u1ag1_year)
d2$week=d2$week+1
d2$year[which(d2$week==53)]=d2$year[which(d2$week==53)]+1
d2$week[which(d2$week==53)]=1
df_w=df_w%>%left_join(d2,by=c("IDNT","year","week"))




d4=df_w%>%select(year,week,IDNT,units)%>%rename(uf1_year=units)
d4$week=d4$week-1
d4$year=d4$year+1
d4$year[which(d4$week==0)]=d4$year[which(d4$week==0)]-1
d4$week[which(d4$week==0)]=52
df_w=df_w%>%left_join(d4,by=c("IDNT","year","week"))

#d5=df_w%>%select(year,week,IDNT,units)%>%rename(uf2_year=units)
#d5$week=d5$week-2
#d5$year=d5$year+1
#d5$year[which(d5$week==0)]=d5$year[which(d5$week==0)]-1
#d5$year[which(d5$week==-1)]=d5$year[which(d5$week==-1)]-1
#d5$week[which(d5$week==0)]=52
#d5$week[which(d5$week==-1)]=51
#df_w=df_w%>%left_join(d5,by=c("IDNT","year","week"))


########## color and style infor


base=df%>%select(YR_454,WK_IDNT,MTH_IDNT,IDNT,UNITS,style,cc,price_label,new_color)%>%
  rename(year=YR_454,month=MTH_IDNT,week=WK_IDNT)%>%
  group_by(year,month,week,IDNT,style,cc,price_label,new_color)%>%
  summarise(units=sum(UNITS))



base2=df%>%select(YR_454,WK_IDNT,MTH_IDNT,IDNT,UNITS,style,cc,price_label,new_color)%>%
  rename(year=YR_454,month=MTH_IDNT,week=WK_IDNT)%>%
  group_by(year,month,week)%>%
  summarise(u=sum(UNITS))


base3=df%>%select(YR_454,WK_IDNT,MTH_IDNT,IDNT,UNITS,style,cc,price_label,new_color)%>%
  rename(year=YR_454,month=MTH_IDNT,week=WK_IDNT)%>%
  group_by(year,month)%>%
  summarise(u=sum(UNITS))


base$week[which(base$week==53)]=52

base=data.frame(base)
base2$week[which(base2$week==53)]=52
base2=data.frame(base2)



style_week=base%>%group_by(year,week,style)%>%
  summarise(style_u_w=sum(units))
style_week$year=style_week$year+1


df_w=df_w%>%left_join(style_week,by=c("style","year","week"))




color_week=base%>%group_by(year,week,new_color)%>%
  summarise(color_u_w=sum(units))

color_week$year=color_week$year+1

df_w=df_w%>%left_join(color_week,by=c("new_color","year","week"))


brand_week=base%>%group_by(year,week,cc)%>%
  summarise(brand_u_w=sum(units))
brand_week$year=brand_week$year+1


df_w=df_w%>%left_join(brand_week,by=c("cc","year","week"))





style_month=df_w%>%group_by(year,month,style)%>%
  summarise(style_u_m=sum(units))%>%left_join(base3,by=c("year","month"))%>%
  mutate(style_ym=style_u_m/u)%>%select(year,month,style,style_ym)

style_month$year=style_month$year+1


df_w=df_w%>%left_join(style_month,by=c("style","year","month"))




color_month=df_w%>%group_by(year,month,new_color)%>%
  summarise(color_u_m=sum(units))%>%left_join(base3,by=c("year","month"))%>%
  mutate(color_ym=color_u_m/u)%>%select(year,month,new_color,color_ym)

color_month$year=color_month$year+1


df_w=df_w%>%left_join(color_month,by=c("new_color","year","month"))


brand_month=df_w%>%group_by(year,month,cc)%>%
  summarise(brand_u_m=sum(units))%>%left_join(base3,by=c("year","month"))%>%
  mutate(brand_ym=brand_u_m/u)%>%select(year,month,cc,brand_ym)

brand_month$year=brand_month$year+1

df_w=df_w%>%left_join(brand_month,by=c("cc","year","month"))
head(df_w)
df_w[is.na(df_w)]=0





###################################
df_w[is.na(df_w)]=0
colnames(df_w)

d_model=df_w%>%left_join(contem_df,by="IDNT")%>%filter(!is.na(dept) &year<2016)%>%
  select(-c(IDNT,year,month,dept,max_orig,min_orig,avg_orig))



d1=d_model
colnames(d1)

for( i in c(1:5)){
  d1[[i]]=as.factor(d1[[i]])
  
}

l1=lm(units~.,data=d1)
summary(l1)

d_train=df_w%>%left_join(contem_df,by="IDNT")%>%filter(!is.na(dept) &year<2016)
d_train$pred=predict(l1)
d_train$pred[which(d_train$create==0)]=0

train_month=d_train%>%group_by(year,month,style,new_color,cc)%>%summarise(total_u=sum(units),total_pre=sum(pred))

1-sum((train_month$total_pre-train_month$total_u)^2)/sum((train_month$total_u-mean(train_month$total_u))^2)
summary(l1)    
head(d2)



d_test=df_w%>%left_join(contem_df,by="IDNT")%>%filter(!is.na(dept) &year==2016)%>%
  select(-c(IDNT,year,month,dept,min_orig,max_orig,min_orig ))
for( i in c(1:5)){
  d_test[[i]]=as.factor(d_test[[i]])
}

d_test$pred=predict(l1,newdata=d_test)
1-sum((d_test$pred-d_test$units)^2)/sum((d_test$units-mean(d_test$units))^2)   # 0.5389966


d_test2=df_w%>%left_join(contem_df,by="IDNT")%>%filter(!is.na(dept) &year==2016)
d_test2$pred=d_test$pred
d_test2$pred[which(d_test2$create==0)]=0
d_test_m=d_test2%>%group_by(month,style,IDNT)%>%summarise(total_u=sum(units),total_pre=sum(pred))  
1-sum((d_test_m$total_pre-d_test_m$total_u)^2)/sum((d_test_m$total_u-mean(d_test_m$total_u))^2)
d_test_m=d_test_m%>%filter(create==1)   



