rm(list=ls())
setwd('/home/aqeel/Study/DM/HW10/')
measuresprint<-function(cm)
{
  Accuracy<-(cm[2,2]+cm[1,1])/sum(cm)
  Precision<-(cm[2,2])/(cm[2,2]+cm[1,2])
  Recall<-cm[2,2]/(cm[2,2]+cm[2,1])
  F1<-2*(Precision*Recall)/(Precision+Recall)
  print(c(Accuracy,Precision,Recall,F1))
}
###### Second Question ########
diabetes <- read.csv('pima-indians-diabetes.data',header = FALSE)
colnames(diabetes)<-c('PregnantTimes','glucos_constr','Blood_pressure','Triceps','insulin','BMI','diabts_pedigree','Age','Class')
#Shuffle The list so we pick randomly 
diabetes<- diabetes[sample(nrow(diabetes)),]
traindata<-diabetes[seq(1,floor(nrow(diabetes)*0.8),1),]
test<-diabetes[seq(floor(nrow(diabetes)*0.8)+1,nrow(diabetes)),]
module <- glm(Class~.,data = traindata)
summary(module)
png('correlationhm.png')
heatmap(cor(diabetes),symm = TRUE, Colv=NA, Rowv=NA,col=colorRampPalette(c("red", "yellow", "green"))(n = 299))
dev.off()

#prediction_prop<-
prediction_bin<-ifelse(predict(module, test)<=0.5,0,1)
measuresprint(table(real=test$Class, predictions=prediction_bin))
####### Third Question#####
library(class)
knn1prediction<-knn(traindata,test = test,cl=traindata$Class)
measuresprint(table(real=test$Class, predictions=knn1prediction))
knn3prediction<-knn(traindata,test = test,cl=traindata$Class,k = 3)
measuresprint(table(real=test$Class, predictions=knn3prediction))

########## Fourth Question ###########
rm(list=ls())
setwd('/home/aqeel/Study/DM/HW10/')
library(ggplot2)
data("diamonds")
diamonds<- diamonds[sample(nrow(diamonds)),]
trainset<-diamonds[seq(1,floor(0.8*nrow(diamonds))),]
testset<-diamonds[seq(floor(0.8*nrow(diamonds))+1,nrow(diamonds)),]
module1<-lm(price~.,data = trainset)
module2<-lm(price~.+poly(carat,2)+poly(depth,2)-carat-depth,data = trainset)
module3<-lm(price~.+poly(carat,3)+poly(depth,3)-carat-depth,data = trainset)
module4<-lm(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,data = trainset)
module1predtrn<-predict(module1, trainset)
module2predtrn<-predict(module2, trainset)
module3predtrn<-predict(module3, trainset)
module4predtrn<-predict(module4, trainset)
module1predtst<-predict(module1, testset)
module2predtst<-predict(module2, testset)
module3predtst<-predict(module3, testset)
module4predtst<-predict(module4, testset)
#install.packages("qpcR")
library(qpcR)
trainRMSE<-c(sqrt(sum((module1predtrn-trainset$price)^2)/length(trainset$price)),
sqrt(sum((module2predtrn-trainset$price)^2)/length(trainset$price)),
sqrt(sum((module3predtrn-trainset$price)^2)/length(trainset$price)),
sqrt(sum((module4predtrn-trainset$price)^2)/length(trainset$price)))

testRMSE<-c(sqrt(sum((module1predtst-testset$price)^2)/length(testset$price)),
sqrt(sum((module2predtst-testset$price)^2)/length(testset$price)),
sqrt(sum((module3predtst-testset$price)^2)/length(testset$price)),
sqrt(sum((module4predtst-testset$price)^2)/length(testset$price)))
numbers<-seq(1,4,1)
png('train_test.png')
ggplot(data.frame(cbind(numbers,trainRMSE)),aes(numbers, trainRMSE))+geom_line(col="red") +
  geom_line(aes(numbers,testRMSE),data =data.frame(cbind(numbers,testRMSE)),col="blue" )+ xlab("Module")+
  ylab("RSME")+?geom_point()
dev.off()
head(cbind(diamonds[,1:6],diamonds[8:10]))
################ Seventh Question ############
library(MASS)
module4ridge<- lm.ridge(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,data = trainset)
module4ridge.trn.prd = as.matrix(model.matrix(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,trainset))%*% coef(module4ridge)
module4ridge.tst.prd = as.matrix(model.matrix(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,testset))%*% coef(module4ridge)


#install.packages("lars")
library(lars)
module4lasso <- lars(
  model.matrix(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,trainset),
  trainset$price, type="lasso",trace = TRUE, max.steps=20)
module4lasso.trn.prd <- predict(module4lasso,
                        model.matrix(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,trainset),
                        s=module4lasso$df[which.min(module4lasso$RSS)], type="fit")$fit


module4lasso.tst.prd <- predict(module4lasso,
                                model.matrix(price~.+poly(carat,3)+poly(depth,3)+poly(x,2)+poly(y,2)+poly(z,2)-carat-depth-x-y-z,testset),
                                s=module4lasso$df[which.min(module4lasso$RSS)], type="fit")$fit

trainRMSE<-c(sqrt(sum((module1predtrn-trainset$price)^2)/length(trainset$price)),
             sqrt(sum((module2predtrn-trainset$price)^2)/length(trainset$price)),
             sqrt(sum((module3predtrn-trainset$price)^2)/length(trainset$price)),
             sqrt(sum((module4predtrn-trainset$price)^2)/length(trainset$price)),
             sqrt(sum(( module4lasso.trn.prd- trainset$price)^2)/length(trainset$price)),
             sqrt(sum((module4ridge.trn.prd- trainset$price)^2)/length(trainset$price)))

testRMSE<-c(sqrt(sum((module1predtst-testset$price)^2)/length(testset$price)),
            sqrt(sum((module2predtst-testset$price)^2)/length(testset$price)),
            sqrt(sum((module3predtst-testset$price)^2)/length(testset$price)),
            sqrt(sum((module4predtst-testset$price)^2)/length(testset$price)),
            sqrt(sum(( module4lasso.tst.prd- testset$price)^2)/length(testset$price)),
            sqrt(sum((module4ridge.tst.prd- testset$price)^2)/length(testset$price)))
numbers<-seq(1,6,1)
png('all_modules.png')
ggplot(data.frame(cbind(numbers,trainRMSE)),aes(numbers, trainRMSE))+geom_line(col="red") +
  geom_line(aes(numbers,testRMSE),data =data.frame(cbind(numbers,testRMSE)),col="blue" )+ xlab("Module")+
  ylab("RSME")
dev.off()
