rm(list=ls())
setwd('/home/aqeel/Study/DM/HW10/')
train <- read.csv('train.csv',header = TRUE)
train$sumusage<- rowSums(train[,seq(12,16,1)])
cor(train)
trainmixed<-train[sample(nrow(train)),]
trainset<-trainmixed[seq(0,floor(0.8*nrow(trainmixed)),1),]
testset<-trainmixed[seq(floor(0.8*nrow(trainmixed))+1,nrow(trainmixed),1),]

module<-lm(data = trainset,target~connections+foreign_conns+foreign_conns2+ccf+poly(conns_new,4)+poly(time,2)+
             poly(ccf_old,2)+account_age+poly(agegroup,3)+browserID+poly(days,4)+poly(usage1,2)+
             usage2+poly(usage3,4)+poly(usage4,4)+poly(usage5,2)+sumusage)
sqrt(mean((predict(module,testset)- testset$target)^2))
summary(module)
test<-read.csv('test.csv',header = TRUE)
head(test[,c(1,2)])
result<-predict(module,test)
output<-cbind(result)
colnames(output)<-c('ID','target')

write.csv(output,file='submit 003')
sort(table(train$target))
full<- train[train$target==10,]
head(full)
max(full$account_age)
empty<-train[train$target==0,]
View(empty)
max(train$account_age)
