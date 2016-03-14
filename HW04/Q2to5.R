rm(list=ls())
setwd('/home/aqeel/Study/DM/HW04')
#### Second Question ######
ncmp = read.csv('ncmp_1415_final_non_disclosive.csv',header = TRUE)
ncmp$ncmppseudosystemid<-NULL
ncmp$heightzscore<-NULL
ncmp$suppress_imd<-NULL
ncmp$heightpscore<-NULL
ncmp$weightzscore<-NULL
ncmp$weightpscore<-NULL
ncmp$bmizscore<-NULL
ncmp$bmipscore<-NULL
ncmp$suppress_table<-NULL
ncmp$suppress_record_low<-NULL
ncmp$schooltier1localauthority<-NULL
ncmp$suppress_record_high<-NULL
ncmp$suppress_record_high<-NULL
ncmp$pupilschooldistancebanded<-NULL
ncmp$schooltier2localauthority<-NULL
ncmp$schoolgovernmentofficeregion<-NULL
ncmp <- ncmp[ncmp$height>0,]
datasample<-ncmp[sample(nrow(ncmp),5000),]
library(ggplot2)

plot(datasample$height,datasample$weight,xlim = c(90,170),ylim = c(10,90))
IQRFunction<-function(x)
{
  return (quantile(x,0.75) - quantile(x,0.25) )
}
datasample<- datasample[datasample$height>0,]
datasample<- datasample[datasample$weight>0,]
plot(datasample$height,datasample$weight,type = 'p',ylab="Weight",xlab="Height")
plot(datasample[,])
antoersample<-ncmp[sample(nrow(ncmp),100),]

#Height Weight
png('heightweight.png',height = 800,width = 1600)
qplot(height,weight,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()
# Height Age
png('heightage',height = 800,width = 1600)
qplot(height,ageinmonths,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()
# Height BMI
png('heightBMI',height = 800,width = 1600)
qplot(height,bmi,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()
# age BMI
png('ageBMI',height = 800,width = 1600)
qplot(ageinmonths,bmi,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()

# age weight
png('ageweight',height = 800,width = 1600)
qplot(ageinmonths,weight,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()

# BMI weight
png('BMIweightstat.png',height = 800,width = 1600)
qplot(bmi,weight,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))+geom_point(alpha=0.3)
dev.off()
head(ncmp)
dim(datasample)
ncmp<-ncmp[complete.cases(datasample),]
#bmi category with age
png('bmicatage',height = 800,width = 1600)
qplot(ageinmonths,bmi,colour =ncmp$bmiclinicalcategory,data = ncmp)+
  scale_color_manual(values=c("darkgreen", "slateblue4","gold","darkred"))
dev.off()
######## Third Question ############
ncmp$heightxweight <- ncmp$height*ncmp$weight
ncmp$heightdivweight<-ncmp$height/ncmp$weight
ncmp$BSA<-sqrt(ncmp$heightxweight)/60

datasample$heightxweight <- datasample$height*datasample$weight
datasample$heightdivweight<-datasample$height/datasample$weight
datasample$BSA<-sqrt(datasample$heightxweight)/60

png ('xdiv.png',width = 1200,height = 830)
qplot(heightxweight,heightdivweight,data = ncmp,colour = ncmp$bmiclinicalcategory)+
  scale_color_manual(values=c("darkgreen", "slateblue4","gold","darkred"))
dev.off()
png ('xbsa.png',width = 1200,height = 830)
qplot(heightxweight,BSA,data = ncmp,colour = ncmp$bmiclinicalcategory)+
  scale_color_manual(values=c("darkgreen", "slateblue4","gold","darkred"))
dev.off()
png ('divbsa.png',width = 1200,height = 830)
qplot(heightdivweight,BSA,data = ncmp,colour = ncmp$bmiclinicalcategory)+
  scale_color_manual(values=c("darkgreen", "slateblue4","gold","darkred"))
dev.off()
png ('bsabmi.png',width = 1200,height = 830)
qplot(bmi,BSA,data = ncmp,colour = ncmp$bmiclinicalcategory)+
  scale_color_manual(values=c("darkgreen", "slateblue4","gold","darkred"))
dev.off()
cor(ncmp$BSA,ncmp$bmi)
png ('density.png',width = 1200,height = 830)
par(mfrow=c(2,2))
plot(density(ncmp$height,kernel = "gaussian"),col="RED",type = "l",main = 'Height')
plot(density(ncmp$weight,kernel = "gaussian"),col="RED",type = "l",main = 'Weight')
plot(density(ncmp$bmi,kernel = "gaussian"),col="RED",type = "l",main = 'BMI')
plot(density(ncmp$BSA,kernel = "gaussian"),col="RED",type = "l",main = 'BSA')
dev.off()


###### Fourth Question ######
normalize<-function(x)
{
  return ((x-mean(x))/sd(x))
}
maleunder100<-ncmp[ncmp$genderdescription=='Male'&ncmp$ageinmonth<100,]
maleunder100$height<- normalize(maleunder100$height)
maleunder100$weight<- normalize(maleunder100$weight)
maleabove100<-ncmp[ncmp$genderdescription=='Male'&ncmp$ageinmonth>100,]
maleabove100$height<- normalize(maleabove100$height)
maleabove100$weight<- normalize(maleabove100$weight)
femaleunder100<-ncmp[ncmp$genderdescription=='Female'&ncmp$ageinmonth<100,]
femaleunder100$height<- normalize(femaleunder100$height)
femaleunder100$weight<- normalize(femaleunder100$weight)
femaleabove100<-ncmp[ncmp$genderdescription=='Female'&ncmp$ageinmonth>100,]
femaleabove100$height<- normalize(femaleabove100$height)
femaleabove100$weight<- normalize(femaleabove100$weight)
normalizedncmp<-rbind(maleunder100,maleabove100,femaleunder100,femaleabove100)
##Another Way to do previous split
#gs<- split(ncmp,datasample$genderdescription)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#age,weight normalized
png('normalizedageweightcomparsion',height = 800,width = 1600)
p1<-qplot(ageinmonths,weight,colour =genderdescription,data = normalizedncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
# age weight
p2<-qplot(ageinmonths,weight,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
multiplot(p1,p2,cols = 2)
dev.off()
# BMI weight
png('BMIweightnormalied.png',height = 800,width = 1600)
qplot(bmi,weight,colour =genderdescription,data = normalizedncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()

#Height Weight
png('heightweightnormalized.png',height = 800,width = 1600)
qplot(height,weight,colour =genderdescription,data = normalizedncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()


########## Fifth Question ##############
#Function return data in quantiles
CreateQuantiles<-function(x,groupname)
{
  #Create Qunatiles
  qu <-quantile(datasample$bmi,probs = seq(0,1,0.1))
  qu.list<-list()
  for (i in seq(1,10,1))
  {
    qu.list[[i]]<-x[x$bmi>qu[i] & x$bmi<qu[i+1],]
    
  }
  result<-data.frame()
  #Create Aggregation
  for (i in seq(1,10,1))
  {
    qu.list[[i]]<-aggregate(height ~ ageinmonths,qu.list[[i]], mean)
    GroupName=paste(groupname,toString(i),sep = '')
    result<-rbind(result,cbind(qu.list[[i]],GroupName))
  }
  return(result)
}
agelimits<-c(49.1,70.0,120.8,141.4)
qqplot(datasample$height,datasample$bmi)
maleage1 <-CreateQuantiles(datasample[datasample$ageinmonths>agelimits[1] & datasample$ageinmonths<agelimits[2]&
                                      datasample$genderdescription=="Male",],'MA1')
ggplot(data=maleage1, aes(x=ageinmonths, y=height, group = GroupName, colour = GroupName)) +
  geom_smooth(se = FALSE)

maleage2 <-CreateQuantiles(datasample[datasample$ageinmonths>agelimits[3] & datasample$ageinmonths<agelimits[4]&
                                        datasample$genderdescription=="Male",],'MA2')
ggplot(data=maleage2, aes(x=ageinmonths, y=height, group = GroupName, colour = GroupName)) +
  geom_smooth(se = FALSE)
femaleage1 <-CreateQuantiles(datasample[datasample$ageinmonths>agelimits[1] & datasample$ageinmonths<agelimits[2]&
                                        datasample$genderdescription=="Female",],'FA1')
ggplot(data=femaleage1, aes(x=ageinmonths, y=height, group = GroupName, colour = GroupName)) +
  geom_smooth(se = FALSE)
femaleage2 <-CreateQuantiles(datasample[datasample$ageinmonths>agelimits[3] & datasample$ageinmonths<agelimits[4]&
                                        datasample$genderdescription=="Female",],'FA2')
ggplot(data=femaleage2, aes(x=ageinmonths, y=height, group = GroupName, colour = GroupName)) +
  geom_smooth(se = FALSE)
