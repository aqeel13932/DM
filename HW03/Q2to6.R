rm(list=ls())
setwd("/home/aqeel/Study/DM/HW03")
##### Second Question #######
shopsdata = read.csv("product_time_shop.txt",sep = ';',header = TRUE)
x<-table(shopsdata$product,shopsdata$shop_id)
x<-cbind(x,rowSums(x))
x<-rbind(x,colSums(x))
x

##### Third Question ##########
library(plyr)
days<-c("Sat","Sun","Sat","Tue","Mon","Tue","Sun","Sat","Fri","Fri","Tue","Wed")
shopsdata$date<- mapvalues(shopsdata$date, from = c(unique(shopsdata$date)), to = days)
png("boxplotStores",width=1600,height = 800)
boxplot(x[,(1:5)],names=paste("Store",colnames(x),sep=" "))
dev.off()
png("boxplotproducts",width = 1600,height = 800)
boxplot(t(x)[,(1:11)],names=colnames(t(x)))
dev.off()
png("boxplotdates.png",width=1600,height=800)
boxplot(table(shopsdata$product,shopsdata$date)[,c(1:6)])
dev.off()
png("matplotallvsall.png",width=1600,height=800)
matplot(x, pch = 16,  col = 1:2,xaxt="n",yaxt="n",
        ylab = "Stores",type = "l")
axis(1,at=c(1:11),labels = rownames(x))
axis(2,at=x[1,],labels =paste("Store",colnames(x),sep=" "))
dev.off()

######## Fourth Question #################
shopsdata = read.csv("product_time_shop.txt",sep = ';',header = TRUE)
products <- unique(shopsdata$product)

dd <-subset(shopsdata,date=="20140104" & shop_id==18)[,(2:3)]
dd$product<- factor(dd$product,products,c(1:11))
png("distribution.png",width = 900,height = 1100)
plot(dd,yaxt="n",main="Store 18 at  2014/01/04")
axis(2,at=c(1:11),labels =products)
dev.off()
dd<-subset(dd,product==9)
png("histdensity.png",width = 600,height = 600)
hist(dd$time,prob=TRUE,xlim =c(1000,2400),main = "Density Over Histogram",xlab="Time")
lines(density(dd$time),col="RED")
dd <-subset(shopsdata,date=="20140104" & shop_id==18)[,(2:3)]
dd$product<- factor(dd$product,products,c(1:11))
dd<-subset(dd,product==8)

lines(density(dd$time),col="GREEN")
dev.off()

############ Question 5 ##############
rm(list=ls())
shopsdata = read.csv("product_time_shop.txt",sep = ';',header = TRUE)
shopsdata$info=paste(paste(shopsdata$date, shopsdata$product, sep=";"),shopsdata$shop_id,sep = ";")
shopsdata$date=NULL
shopsdata$product=NULL
shopsdata$shop_id=NULL
for (i in c(7:23))
{
shopsdata[shopsdata$time>=(i*100)&shopsdata$time<((i+1)*100),]$time =i
}
colSums(table(shopsdata$info,shopsdata$time))
png("heatmap2.png",width=800,height = 800)
heatmap(table(shopsdata$info,shopsdata$time))
dev.off()

############### Question 6 ##############
###1st request###
rm(list=ls())
setwd("/home/aqeel/Study/DM/HW03")
shopsdata = read.csv("product_time_shop.txt",sep = ';',header = TRUE)
library(plyr)
days<-c("Sat","Sun","Sat","Tue","Mon","Tue","Sun","Sat","Fri","Fri","Tue","Wed")
shopsdata$date<- mapvalues(shopsdata$date, from = c(unique(shopsdata$date)), to = days)
###2nd request###
products_shops<-table(shopsdata$date,shopsdata$product)
products_shops<- products_shops/norm(products_shops,type ="M")
png("heatmapproductsvsdays.png",width = 500, height = 500)
heatmap(products_shops)
dev.off()
###3rd request###
for (i in c(7:23))
{
  shopsdata[shopsdata$time>=(i*100)&shopsdata$time<((i+1)*100),]$time =i
}
days<-c("Sat","Sun","Sat","Tue","Mon","Tue","Sun","Sat","Fri","Fri","Tue","Wed")
shopsdata$date<- mapvalues(shopsdata$date, from = c(unique(shopsdata$date)), to = days)
shopsdata$date<-paste(shopsdata$date, shopsdata$time, sep=" ")
shopsdata$time<-NULL
products_overtime <-table(shopsdata$date,shopsdata$product)
products_overtime<- products_overtime/norm(products_overtime)
png("frequencyoverdays.png",width=500,height=500)
matplot(products_overtime,type = "l",xlab = "Day & Time" , ylab = "Product" )
dev.off()

