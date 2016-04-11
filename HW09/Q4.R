rm(list= ls())
setwd('/home/aqeel/Study/DM/HW09/')
houses<- read.csv('housing.data',header = FALSE,sep = '')
colnames(houses)<-c('CRIM','ZN','INDUS','CHAS','NOX','RM','AGE','DIS','RAD','TAX','PTRATIO','B','LSTAT','MEDV')
module<-lm(data = houses,MEDV~.)
summary(module)
#summary(lm(data = houses,MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT))
###### Fifth Question #####
correlation <-cor(houses)
correlation
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
png('correlation.png',width = 1280,height = 800)
heatmap(correlation,main='correlation',cellnote = correlation,symm = TRUE,col=my_palette)
dev.off()
max(correlation)
diag(correlation)=0
which(correlation==max(correlation),arr.ind = TRUE)
colnames(houses)
 
tt<-as.data.frame(summary(lm(data = houses,MEDV~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,CRIM~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,ZN~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,INDUS~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,CHAS~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,NOX~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,RM~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,AGE~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,DIS~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,RAD~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,TAX~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,PTRATIO~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,B~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]
tt<-as.data.frame(summary(lm(data = houses,LSTAT~.))$coefficients)
tt<-tt[order(-tt$Estimate),]
tt[c(1,2),]

