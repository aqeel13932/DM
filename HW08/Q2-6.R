rm(list=ls())
setwd('/home/aqeel/Study/DM/HW08/')
#### Second Question ######
playing = read.csv('data.csv')
table(playing$Play)
table(playing$Outlook,playing$Play)
table(playing$Temp,playing$Play)
table(playing$Humidity,playing$Play)
#install.packages("entropy")
library(entropy)
#outlook Gain
table(playing$Outlook,playing$Play)
entropy(c(6,9),unit = "log2")-(5/15*entropy(c(1,4),unit = "log2")+5/15*entropy(c(2,3),unit = "log2")+5/15*entropy(c(3,2),unit = "log2"))
#Temp Gain
table(playing$Temp,playing$Play)
entropy(c(6,9),unit = "log2")-(5/15*entropy(c(2,3),unit = "log2")+4/15*entropy(c(2,2),unit = "log2")+6/15*entropy(c(2,4),unit = "log2"))
#Humidity Gain
table(playing$Humidity,playing$Play)
entropy(c(6,9),unit = "log2")-(8/15*entropy(c(5,3),unit = "log2")+7/15*entropy(c(1,6),unit = "log2"))
#Windy Gain
table(playing$Windy,playing$Play)
entropy(c(6,9),unit = "log2")-(9/15*entropy(c(3,6),unit = "log2")+6/15*entropy(c(3,3),unit = "log2"))
####Level Two####
humidity.h <- subset(playing,playing$Humidity=="High")
humidity.n <- subset(playing,playing$Humidity=="Normal")
#Humidity Normal features gain
#Normal humidity
table(humidity.n$Play)
table(humidity.n$Outlook,humidity.n$Play)
entropy(c(1,6),unit = "log2")-(2/7*entropy(c(0,2),unit = "log2")+3/7*entropy(c(1,2),unit = "log2")+2/7*entropy(c(0,2),unit = "log2"))
table(humidity.n$Temp,humidity.n$Play)
entropy(c(1,6),unit = "log2")-(4/7*entropy(c(1,3),unit = "log2")+1/7*entropy(c(0,1),unit = "log2")+2/7*entropy(c(0,2),unit = "log2"))
table(humidity.n$Windy,humidity.n$Play)
entropy(c(1,6),unit = "log2")-(4/7*entropy(c(0,4),unit = "log2")+3/7*entropy(c(1,2),unit = "log2"))
#High humidity
table(humidity.h$Play)
table(humidity.h$Outlook,humidity.h$Play)
entropy(c(5,3),unit = "log2")-(3/8*entropy(c(1,2),unit = "log2")+2/8*entropy(c(1,1),unit = "log2")+3/8*entropy(c(3,0),unit = "log2"))
table(humidity.h$Temp,humidity.h$Play)
entropy(c(5,3),unit = "log2")-(1/8*entropy(c(1,0),unit = "log2")+3/8*entropy(c(2,1),unit = "log2")+4/8*entropy(c(2,2),unit = "log2"))
table(humidity.h$Windy,humidity.h$Play)
entropy(c(5,3),unit = "log2")-(5/8*entropy(c(3,2),unit = "log2")+3/8*entropy(c(2,1),unit = "log2"))
#### Level 3 #####
table(playing$Outlook)
rainy.normal<- subset(playing,playing$Humidity=="Normal" & playing$Outlook=="Rainy")
rainy.high<- subset(playing,playing$Humidity=="High" & playing$Outlook=="Rainy")
overcast.high<- subset(playing,playing$Humidity=="High" & playing$Outlook=="Overcast")
#Rainy Normal branch
table(rainy.normal$Play)
table(rainy.normal$Windy,rainy.normal$Play)
entropy(c(1,2),unit = "log2")-(2/3*entropy(c(0,2),unit = "log2")+1/3*entropy(c(1,0),unit = "log2"))
table(rainy.normal$Temp,rainy.normal$Play)
entropy(c(1,2),unit = "log2")-(2/3*entropy(c(1,1),unit = "log2")+1/3*entropy(c(0,1),unit = "log2"))
#Rainy High branch
table(rainy.high$Play)
table(rainy.high$Windy,rainy.high$Play)
entropy(c(1,1),unit = "log2")-(1/2*entropy(c(0,1),unit = "log2")+1/2*entropy(c(1,0),unit = "log2"))
table(rainy.high$Temp,rainy.high$Play)
entropy(c(1,1),unit = "log2")-(entropy(c(1,1),unit = "log2"))
#overcast high branch
table(overcast.high$Play)
table(overcast.high$Windy,overcast.high$Play)
entropy(c(1,2),unit = "log2")-(2/3*entropy(c(1,1),unit = "log2")+1/3*entropy(c(0,1),unit = "log2"))
table(overcast.high$Temp,overcast.high$Play)
entropy(c(1,2),unit = "log2")-(1/3*entropy(c(1,0),unit = "log2")+1/3*entropy(c(0,1),unit = "log2")+1/3*entropy(c(0,1),unit = "log2"))

########## Third Question#######
cars = read.csv('car.data')
library(rpart)

colnames(cars)
result <-rpart(acceptability ~.,data = cars,method = 'class')
plotcp(result)
# plot tree
png('tree.png',width = 1600,height = 800)
plot(result, uniform=TRUE,
     main="Classification Tree for Cars")
text(result, use.n=TRUE, all=TRUE, cex=.8)
dev.off()
library(arules)
rules = apriori(cars )
inspect(rules)
########### Fourth Question #############
rm(list=ls())
#install.packages("klaR")
cars = read.csv('car.data')
library(klaR)
nbayestree<- NaiveBayes(acceptability~.,data = cars)
summary(nbayestree)
nbayestree$tables$buying
nbayestree$tables$maint
nbayestree$tables$doors
nbayestree$tables$persons
nbayestree$tables$lug_boot
nbayestree$tables$safety
##Testing.
#install.packages("ROCR")
#install.packages("getopt")
library(gplots)
library(ROCR)
library(e1071)
library(getopt)
library(rpart)
Compare<-function()
{
  bcmVector<- (c(OverallAccuracy=0,recallAcc=0,recallGood=0,recallunacc=0,recallVgood=0,precisionAcc=0,
               precisionGood=0,precisionunacc=0,precisionVgood=0))
  dtcmVector<- (c(OverallAccuracy=0,recallAcc=0,recallGood=0,recallunacc=0,recallVgood=0,precisionAcc=0,
                 precisionGood=0,precisionunacc=0,precisionVgood=0))
  for(i in seq_len(10))
  {
  # 1728 * 0.9 ~ 1556 , 1728*0.1~172
  #Shuffle the list To use it later
  tmp<-cars[sample(nrow(cars)),]
  nbp<-predict(NaiveBayes(acceptability~.,data = tmp[c(1:1556),]),tmp[c(1557:1728),])
  dtreep<-predict(rpart(acceptability~.,data = tmp[c(1:1556),]),newdata =  tmp[c(1557:1728),],type = 'class')
  bcm<-table(nbp$class,tmp[c(1557:1728),]$acceptability)
  dtcm<-table(dtreep,tmp[c(1557:1728),]$acceptability)
  #print (bcm)
  bcmVector<-bcmVector +c(OverallAccuracy=sum(diag(bcm))/sum(bcm),recallAcc=bcm[1,1]/sum(bcm[1,]),
           recallGood=bcm[2,2]/sum(bcm[2,]),recallunacc=bcm[3,3]/sum(bcm[3,]),
           recallVgood=bcm[4,4]/sum(bcm[4,]),precisionAcc=bcm[1,1]/sum(bcm[,1]),
           precisionGood=bcm[2,2]/sum(bcm[,2]),precisionunacc=bcm[3,3]/sum(bcm[,3]),
           precisionVgood=bcm[4,4]/sum(bcm[,4]))
  #print (dtcm)
  dtcmVector<-dtcmVector+c(OverallAccuracy=sum(diag(dtcm))/sum(dtcm),recallAcc=dtcm[1,1]/sum(dtcm[1,]),
           recallGood=dtcm[2,2]/sum(dtcm[2,]),recallunacc=dtcm[3,3]/sum(dtcm[3,]),
           recallVgood=dtcm[4,4]/sum(dtcm[4,]),precisionAcc=dtcm[1,1]/sum(dtcm[,1]),
           precisionGood=dtcm[2,2]/sum(dtcm[,2]),precisionunacc=dtcm[3,3]/sum(dtcm[,3]),
           precisionVgood=dtcm[4,4]/sum(dtcm[,4]))
  }
  print(bcmVector/10)
  print (dtcmVector/10)
}
print (c(F11=3,F10=1,F01=2,F00=0))
## Run this method for 10 times will achieve the desired request.
Compare()
############## 5th Question #############
rm(list=ls())
setwd('/home/aqeel/Study/DM/HW08/')
titanic <- read.csv('titanic.txt')
library(klaR)
library(gplots)
library(ROCR)
library(arules)
library(rpart)
#Shuffle the list To use it later
tmp<-titanic[sample(nrow(titanic)),]
nbmodel<-NaiveBayes(Survived~.,data = tmp[c(1:1981),])
print(nbmodel$tables)
dtmodel<-rpart(Survived~.,data = tmp[c(1:1981),])
png('treetitanic.png',width = 1600,height = 800)
plot(dtmodel, uniform=TRUE,
main="Classification Tree for Titanic survivres")
text(dtmodel, use.n=TRUE, all=TRUE, cex=.8)
dev.off()
    
therules<- apriori(tmp[c(1:1981),],parameter = list(supp=0.05,conf=0.7),
                               appearance = list(rhs=c("Survived=Yes","Survived=No"),default="lhs"))
print (inspect(therules))

########### Sixth Question ##########
rm(list=ls())
connect4<-read.csv('connect-4.data',header = FALSE,sep = ",")
colnames(connect4)<-c('a1','a2','a3','a4','a5','a6','b1','b2','b3','b4','b5','b6','c1','c2','c3','c4','c5','c6','d1','d2','d3','d4','d5','d6','e1','e2','e3','e4','e5','e6','f1','f2','f3','f4','f5','f6','g1','g2','g3','g4','g5','g6','result')

##Over fitting example ##
connect4<-connect4[sample(nrow(connect4)),]
# 67557*0.8 ~ 54046 , 67557*0.2~13511
nbmodel<-NaiveBayes(result~.,connect4[c(0:54046),])
nbp<-predict(nbmodel,connect4[c(54047:67557),])
cm<-table(nbp$class,connect4[c(54047:67557),]$result)
OverallAccuracy=sum(diag(cm))/sum(cm)
OverallAccuracy
