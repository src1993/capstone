#rm(list=ls())

#Set working directory          
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Load data sets

load("data/created/brand_info_bridge")
load('data/created/bridge.Rdata')

#Graphs

ggplot(brand, aes(clust, avg_price, fill = clust))+geom_boxplot()+
    guides(fill=F)+xlab("Cluster")+ylab("Average price")+scale_fill_brewer()
ggplot(brand, aes(clust, seniority, fill = clust))+geom_boxplot()+
    guides(fill=F)+xlab("Cluster")+ylab("Seniority")+scale_fill_brewer()
ggplot(brand, aes(clust, UNITS, fill = clust))+geom_boxplot()+
    guides(fill=F)+xlab("Cluster")+ylab("UNITS")+scale_fill_brewer()

# Summary 

changes <- brand %>%
    group_by(BRAND_NAME, clust) %>%
    summarize(t = n()) %>%
    group_by(BRAND_NAME) %>%
    summarize(t = n())

coach <- bridge %>%
    filter(BRAND_NAME == "COACH" & YEAR < 2017)%>%
    group_by(ym = as.yearmon(DAY_DT)) %>%
    summarize(UNITS = sum(UNITS))

ggplot(coach, aes(ym, UNITS))+geom_line(color = "steelblue2")+xlab("Time")+
    ylab("Total units")+scale_x_yearmon()+ggtitle("COACH")

pat <- bridge %>%
    filter(BRAND_NAME == "PATRICIA NASH HANDBAGS" & YEAR < 2017 )%>%
    group_by(ym = as.yearmon(DAY_DT)) %>%
    summarize(UNITS = sum(UNITS))

ggplot(pat, aes(ym, UNITS))+geom_line(color = "steelblue")+xlab("Time")+
    ylab("Total units")+scale_x_yearmon()+ggtitle("PATRICIA NASH HANDBAGS")
