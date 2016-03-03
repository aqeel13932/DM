##### Question 1 #####
rm(list=ls())
setwd("/home/aqeel/Study/DM/HW03")
klient1 <- read.csv('klient1.txt',header = FALSE)
klient3 <- read.csv('klient3.txt',header=FALSE)
png("densitywithoutwidth.png",width=500,height = 500)
plot(density(klient1$V1),col="RED",type = "l",main="K1 (Green) & K3(Red) Density")
lines(density(klient3$V1),col="green")
dev.off()
png("densitywithwidth.png",width = 500,height = 500)
bandwidth =11
plot(density(klient1$V1,bw=bandwidth,kernel = "gaussian"),col="RED",type = "l",main="K1 (Green) & K3(Red) Density")
lines(density(klient3$V1,bw=bandwidth,kernel = "gaussian"),col="green")
dev.off()