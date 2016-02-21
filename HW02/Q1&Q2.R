################### #FIRST QUESTION ########################
#clean everything
rm(list = ls())
#set work place
setwd('/home/aqeel/Study/DM/HW02')
#import data
mydata = read.csv('abalone.csv',header = TRUE)
#get column names
colnames(mydata)
#get number of rows
nrow(mydata)
#print first three rows

mydata[(1:3),]
#print last two rows
mydata[c(nrow(mydata),nrow(mydata)-1),]
#print last two rows weight
mydata[c(nrow(mydata),nrow(mydata)-1),]$Weight
#print diameter of row 755
mydata[755,]$Diameter
#number of rows that don't have height value
length(mydata[is.na(mydata)])
#the mean for height column (two ways)
mean(mydata[complete.cases(mydata),]$Height)
mean(mydata[!is.na(mydata$Height),]$Height)

  #Extract subset with Gender M and weight less than 0.75
  newsubset = subset(mydata,mydata$Gender=="M" & mydata$Weight<0.75)
mean(newsubset$Diameter)
#Most frequent rings value
table(mydata$Rings)
#minimum length when rings equal to 18
min(subset(mydata,Rings==18)$Length)


################## SECOND QUESTION ########################
gender<-mydata$Gender
gender<- as.numeric(factor(mydata$Gender,c('F','M','I'),c(1:3)))
#Extract All Required Information
values <-rbind(
  #Length
  c(mean(mydata$Length),median(mydata$Length),min(mydata$Length),max(mydata$Length),sd(mydata$Length)),
  #Diameter
  c(mean(mydata$Diameter),median(mydata$Diameter),min(mydata$Diameter),max(mydata$Diameter),sd(mydata$Diameter)),
  #Height
  c(mean(mydata$Height,na.rm = TRUE),median(mydata$Height,na.rm = TRUE),min(mydata$Height,na.rm = TRUE),max(mydata$Height,na.rm = TRUE),sd(mydata$Height,na.rm = TRUE)),
  #Weight
  c(mean(mydata$Weight),median(mydata$Weight),min(mydata$Weight),max(mydata$Weight),sd(mydata$Weight)),
  #Rings
  c(mean(mydata$Rings),median(mydata$Rings),min(mydata$Rings),max(mydata$Rings),sd(mydata$Rings)),
  #Gender
  c(mean(gender),median(gender),min(gender),max(gender),sd(gender)))
colnames(mydata)
rownames(values)<-c("Length" ,  "Diameter" ,"Height" ,  "Weight"  , "Rings","Gender")
colnames(values)<-c("mean","median","min","max","SD")
values
#Rings
ggplot(mydata, aes(x=Rings))+stat_count(width=0.5)+xlim(0,30)
#Length
ggplot(mydata, aes(x=Length))+geom_bar(width=0.01)+xlim(0,0.815)
#Diameter
ggplot(mydata,aes(x=Diameter))+geom_bar(width = 0.004)
#Weight
ggplot(mydata,aes(x=Weight))+geom_bar(width = 0.004)+ylim(0,4.1)
#Height
ggplot(mydata,aes(x=Height))+geom_bar(width = 0.004)+xlim(0,0.3)
#Gender
ggplot(mydata,aes(x=Gender))+stat_count(width = 0.9)
