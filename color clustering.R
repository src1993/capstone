#rm(list=ls())

#Working directory
setwd("/Users/loredp/Dropbox (MIT)/MIT/Capstone")

#Libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggdendro)
library(factoextra)
library(cluster)
library(NbClust)


#Load data sets
#load("data/created/information.Rdata")
load('data/created/bridge.Rdata')

color_month <- bridge %>% 
    group_by(COLOR,MTH_IDNT) %>%
    summarize(UNITS = sum(UNITS))%>%
    mutate(month = paste("m",MTH_IDNT, sep = "_")) %>%
    select(-MTH_IDNT)%>%
    spread(month, UNITS)

color_month[is.na(color_month)]<-0

M <- cor(color_month[,-1])
corrplot(M, type = "upper", tl.pos = "td",
         method = "square", tl.cex = 0.5, tl.col = 'black', diag = FALSE)                      

p <- prcomp(color_month[,-1])
print(p)
          
plot(p, type = 'l')  

coords <- data.frame(p$x)

color_month$PC1 <- coords$PC1
color_month$PC2 <- coords$PC2

ggplot(color_month, aes(PC1,PC2,color = COLOR, label = COLOR))+
    geom_point()+geom_text()+guides(color = F)

## Clustering

dataScaled <- scale(color_month[c(2:13)])
d <- dist(dataScaled)

clusters <- hclust(d, method = "ward.D")
fviz_nbclust(dataScaled, hcut, method = "wss") +
    geom_vline(xintercept = 5, linetype = 2)

plot(clusters, labels = color_month$COLOR, hang = -1)
clusterGroups = cutree(clusters, k = 5)
rect.hclust(clusters, k=5, border=2:4)

