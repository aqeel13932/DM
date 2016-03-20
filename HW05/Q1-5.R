rm(list=ls())
setwd('/home/aqeel/Study/DM/HW05')
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
test <-table(ncmp$ageinmonths)
test[test==max(test)]
library(ggplot2)
male<- ncmp[ncmp$ageinmonths==60.6&
                ncmp$genderdescription=="Male",]
female<-ncmp[ncmp$ageinmonths==60.6&
              ncmp$genderdescription=="Female",]

png ('male.png',width = 1200,height = 830)
qqplot(male$height,male$weight,ylab = 'Weight',xlab = 'Height')
dev.off()
png ('female.png',width = 1200,height = 830)
qqplot(female$height,female$weight,ylab = 'Weight',xlab = 'Height')
dev.off()

underweight<- ncmp[ncmp$bmipopulationcategory =='underweight' ,]
overweight<-ncmp[ncmp$bmipopulationcategory=='very overweight',]
png('undervsover.png',width=1200,height = 830)
qqplot(underweight$height,overweight$height,xlab = 'Under Weight Height',ylab = 'Over weight height')
dev.off()
agelimits<-c(49.1,70.0,120.8,141.4)
maleage1 <-ncmp[ncmp$ageinmonths>agelimits[1] & ncmp$ageinmonths<agelimits[2]&
                  ncmp$genderdescription=="Male",]

femaleage2 <-ncmp[ncmp$ageinmonths>agelimits[1] & ncmp$ageinmonths<agelimits[2]&
                    ncmp$genderdescription=="Female",]
png('malevsfemale.png',width=1200,height = 830)
qqplot(maleage1$height,femaleage2$height)
dev.off()
########### Second Question ############
png('malebmi.png',width = 1200,height = 830)
qqplot(male$height,male$bmi,xlab = 'height',ylab ='BMI')
dev.off()
png('femalebmi.png',width = 1200,height = 830)
qqplot(female$height,female$bmi,xlab = 'height',ylab ='BMI')
dev.off()

########Third Question #############
overweight<-ncmp[ncmp$bmipopulationcategory=='overweight'&ncmp$genderdescription=='Male' &ncmp$ageinmonths>60.6 &
                   ncmp$ageinmonths<80,]
png('QQvsdest',width = 1200,height = 830)
qqnorm(overweight$height);qqline(overweight$height,col=2);
dev.off()
##########Fifth Question #########
