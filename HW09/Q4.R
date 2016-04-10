rm(list= ls())
setwd('/home/aqeel/Study/DM/HW09/')
houses<- read.csv('housing.data',header = FALSE,sep = '')
colnames(houses)<-c('CRIM','ZN','INDUS','CHAS','NOX','RM','AGE','DIS','RAD','TAX','PTRATIO','B','LSTAT','MEDV')
module<-lm(data = houses,MEDV~.)
summary(module)
#summary(lm(data = houses,MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT))
###### Fifth Question #####

