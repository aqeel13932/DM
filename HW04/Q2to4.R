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



nrow(ncmp[ncmp$height<0,])
unique(ncmp$bmipopulationcategory)
ncmp <- ncmp[ncmp$height>0,]

datasample<-ncmp[sample(nrow(ncmp),5000),]
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
library(ggplot2)
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
png('BMIweight',height = 800,width = 1600)
qplot(bmi,weight,colour =genderdescription,data = ncmp)+ 
  scale_color_manual(values=c("tomato", "slateblue4"))
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
qplot(heightxweight,heightdivweight,data = ncmp,colour = ncmp$genderdescription)+
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()
png ('xbsa.png',width = 1200,height = 830)
qplot(heightxweight,BSA,data = ncmp,colour = ncmp$genderdescription)+
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()
png ('divbsa.png',width = 1200,height = 830)
qplot(heightdivweight,BSA,data = ncmp,colour = ncmp$genderdescription)+
  scale_color_manual(values=c("tomato", "slateblue4"))
dev.off()