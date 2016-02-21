rm(list=ls())
setwd("/home/aqeel/Study/DM/HW02/")
mydata = read.csv('abalone.csv',header = TRUE)
names(mydata)
############## Third Question ###################
#Save plot with high resultion
png("scatterplotall.png",width = 1600,height = 900)
#draw plot
plot(mydata[,2:6])
#write plot
dev.off()
#Print correlation
cor(mydata[complete.cases(mydata),][,2:6])
#scatterplot
plot(mydata$Rings,mydata$Length,xlim = c(0,27),xlab = "Rings",ylab = "Length")
#scatterplot Diameter,Length
plot(mydata$Diameter,mydata$Length,xlab="Diameter",ylab="Length")
########### Fourth Question #################
IQRFunction<-function(x)
{
  return (quantile(x,0.75) - quantile(x,0.25) )
}
height<- mydata[complete.cases(mydata),]
IQRALL= c(IQRFunction(mydata$Length),IQRFunction(mydata$Diameter),
          IQRFunction(height$Height),IQRFunction(mydata$Weight),IQRFunction(mydata$Rings))
names(IQRALL)<- names(mydata)[2:6]
IQRALL
#Calculate Over outlier
outliercounttop<-function(x,iqr)
{
  return (length(which(x>(quantile(x,0.75)+1.5*iqr))))
}
#Calculate Under outlier
outliercountfloor<-function(x,iqr)
{
  return (length(which(x<(quantile(x,0.25)-1.5*iqr))))
}
names(IQRALL)
matr <-rbind(c(outliercounttop(mydata$Length,IQRALL[1]),outliercounttop(mydata$Diameter,IQRALL[2]),
              outliercounttop(height$Height,IQRALL[3]),outliercounttop(mydata$Weight,IQRALL[4]),
              outliercounttop(mydata$Rings,IQRALL[5])),
            c(outliercountfloor(mydata$Length,IQRALL[1]),outliercountfloor(mydata$Diameter,IQRALL[2]),
              outliercountfloor(height$Height,IQRALL[3]),outliercountfloor(mydata$Weight,IQRALL[4]),
              outliercountfloor(mydata$Rings,IQRALL[5])))
IQRALL
colnames(matr)<-names(mydata)[2:6]
rownames(matr)<-(c("Over","Under"))
matr
################ Question 5 ###########################
#Clean Length
mydata<-mydata[(mydata$Length<(quantile(mydata$Length,0.75)+1.5*IQRALL[1])) & 
         (mydata$Length>(quantile(mydata$Length,0.25)-1.5*IQRALL[1])),]
#clean Height
mydata<-mydata[!is.na(mydata$Height),]
mydata<-mydata[(mydata$Height<(quantile(mydata$Height,0.75)+1.5*IQRALL[1])) & 
                 (mydata$Height>(quantile(mydata$Height,0.25)-1.5*IQRALL[1])),]
#clean Rings
mydata<-mydata[mydata$Rings<1500,]
nrow(mydata)
