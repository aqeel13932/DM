rm(list=ls())
setwd('/home/aqeel/Study/DM/HW06/')
#Used to install librariries to R
#install.packages("arules")
#install.packages("arulesViz")

#Read data from files
titanic <- read.table( "titanic.txt", sep = ',' , header = TRUE)

#observe the data
##first 6 observations
head(titanic)
#types of features
str(titanic)
#dimensionality of the data
dim(titanic)

#load package for frequent set mining
library(arules)

#help with apriori
?apriori

#run apriori algorithm with default settings
rules = apriori(titanic)

#inspection of the result
inspect(rules)

#now let us assume, we want to see only those rules that have rhs as survived:
rules = apriori(titanic,appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))
inspect(rules)

#let us relax the default settings for the rules we are looking for
rules = apriori(titanic,parameter = list(minlen=2, supp=0.1, conf=0.8),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))
inspect(rules)
#visualization
library(arulesViz)
png('Q5rules.png',width = 800,height = 600)
plot(rules, method="graph", control=list(type="items"))
dev.off()
