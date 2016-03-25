rm(list=ls())
setwd('/home/aqeel/Study/DM/HW07/')
###First Question ####
library(arules)
supermarket = read.transactions('supermarket.txt',format = 'basket',sep=" ")
tim = proc.time()
rules = apriori(supermarket,parameter = list(minlen=2,supp = 0.01,conf=0.05))#,conf=0.5))
print (proc.time()-tim)
tim = proc.time()
itmset = eclat(supermarket,parameter = list(supp = 0.01, maxlen = 15))
print (proc.time()-tim)
inspect(itmset)
head(inspect(rules))
head(inspect(itmset))
?apriori
?eclat
#### Second Question #####
high.support<- sort(rules, decreasing = TRUE, na.last = NA, by = "support")[1:10,]
high.confidence<- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")[1:10,]
high.lift<- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")[1:10,]
lst<-read.csv('supermarket.txt',header = FALSE,sep=" ")

FindAllInfoV2 <- function(rule, dataset){
  # Extract the left hand side of the rule
  lhs.tbl <- itemInfo(lhs(rule))[which(as(lhs(rule), "matrix")[1, ] == 1), ]
  rhs.tbl <- itemInfo(rhs(rule))[which(as(rhs(rule), "matrix")[1, ] == 1), ]
  TP = 0
  TN= 0 
  FP =0
  FN = 0
  
  for(i in seq_len(nrow(dataset)))
  {
    #Left Hand exist
    l <- sum(lhs.tbl %in% dataset[i,])
    r <- sum(rhs.tbl %in% dataset[i,])
    l <- l>=length(lhs.tbl)
    r <- r>=length(rhs.tbl)
    if (l)
    {
      #right hand also exist
      if (r)
      {
        TP<-TP+1
      }
      else
      {
        FN<-FN+1
      }
    }
    #left hand doesn't exist
    else
    {
      #but right hand exist
      if (r)
      {
        FP<-FP+1
      }
      #also right hand doesn't exist
      else
      {
        TN<-TN+1
      }
    }
  }
  leftside =0
  if (length(lhs.tbl)>1)
  {
    leftside = paste(lhs.tbl, collapse = ',')
  }
  else
  {
    leftside = strtoi(lhs.tbl, base = 0L)
  }

  return (c(quality(rule)[1],quality(rule)[2],quality(rule)[3],left =leftside,right =strtoi(rhs.tbl, base = 0L),F11= TP,F10=FN,F01=FP,F00=TN))
}
dfsupport<- data.frame()
for (i in seq_len(length(high.support)))
{
  dfsupport<-rbind(dfsupport,FindAllInfoV2(high.support[i],lst))
}

dfconfidence<- data.frame()
for (i in seq_len(length(high.confidence)))
{
  dfconfidence<-rbind(dfconfidence,FindAllInfoV2(high.confidence[i],lst))
}
st = proc.time()
dflift<-data.frame()
for (i in seq_len(length(high.lift)))
{
  dflift<-rbind(dflift,FindAllInfoV2(high.lift[i],lst))
}
proc.time()-st
dfsupport
dfconfidence
dflift

###### Third Question #####

calculatelaplace<-function(thedata)
{
  #Jaccard = f11/f1plus+fplus1-f11
  #fplus1= f11+f01
  #f1plus= f11+f10
  thedata$Jaccard <- thedata$F11/(thedata$F11+thedata$F01+thedata$F10)
  return (thedata)
}
dfsupport<-calculatelaplace(dfsupport)
dflift<-calculatelaplace(dflift)
dfconfidence<-calculatelaplace(dfconfidence)
for (i in seq_len(10))
{

  print (c(i,dfsupport$Jaccard[i],dfconfidence$Jaccard[i],dflift$Jaccard[i]))
}

dfsupport[1,]
