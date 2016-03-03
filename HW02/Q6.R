#######################################
#           Sixth Question            #
#######################################
#USED MACHINE LEARNING EXERCISE SESSION CODE TO HELP WITH THIS TASK
rm(list=ls())
setwd("/home/aqeel/Study/DM/HW02/")
mydata = read.csv('abalone.csv',header = TRUE)
# Lets observe the data
plot(mydata[,c(3,5)])

# Lets observe the linear model
linear.model = lm(Weight ~ Diameter,mydata)
# Lets extract coeficiens of the linear model
variables <- coef(linear.model)
variables
plot(mydata[,c(3,5)])
abline(a = variables[1],b = variables[2],col="red",lwd=5)
?lm
