#Read in data
mtss<-read.csv("~/OneDrive - Harvard University/Research/Postdoc/MT/MT Hippocampus stress sensitization/Data/mtss.csv")
#Just Neuroimaging Sample
mtss2<-mtss[-which(is.na(mtss$Hippocampus)),]

#Descriptives Table
#Sex
table(mtss2$SEX,mtss2$ANY_ABUSE_CHILD_DV)
chisq.test(mtss2$SEX,mtss2$ANY_ABUSE_CHILD_DV)
#Race
table(mtss2$RACE,mtss2$ANY_ABUSE_CHILD_DV)
chisq.test((mtss2$RACE==2),mtss2$ANY_ABUSE_CHILD_DV)
chisq.test((mtss2$RACE==3),mtss2$ANY_ABUSE_CHILD_DV)
chisq.test((mtss2$RACE==4),mtss2$ANY_ABUSE_CHILD_DV)
chisq.test((mtss2$RACE==5),mtss2$ANY_ABUSE_CHILD_DV)
#Poverty
table(mtss2$POVERTY,mtss2$ANY_ABUSE_CHILD_DV)
chisq.test(mtss2$POVERTY,mtss2$ANY_ABUSE_CHILD_DV)
#Age
t.test(mtss2$S3AGE~mtss2$ANY_ABUSE_CHILD_DV)
sd(mtss2$S3AGE[which(mtss2$ANY_ABUSE_CHILD_DV==1)])
sd(mtss2$S3AGE[which(mtss2$ANY_ABUSE_CHILD_DV==0)])
#Hippocampus
t.test(mtss2$Hippocampus~mtss2$ANY_ABUSE_CHILD_DV)
sd(mtss2$Hippocampus[which(mtss2$ANY_ABUSE_CHILD_DV==1)])
sd(mtss2$Hippocampus[which(mtss2$ANY_ABUSE_CHILD_DV==0)])
#IC Volume
t.test(mtss2$ICVol~mtss2$ANY_ABUSE_CHILD_DV)
sd(mtss2$ICVol[which(mtss2$ANY_ABUSE_CHILD_DV==1)])
sd(mtss2$ICVol[which(mtss2$ANY_ABUSE_CHILD_DV==0)])

#Impute missing poverty data
require(mice)
set.seed(123)
mtss3<-mtss2[,c(2,5,6,7,8,12,13,22,23,27,28)]
w<-is.na(mtss3)
w[,8]<-FALSE
w[,9]<-FALSE
imp<-mice(mtss3,m=100,where=w)

#Analyses
#Stress Sensitization to depression
summary(pool(with(imp, lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+scale(TotalStress,scale=F)*ANY_ABUSE_CHILD_DV))))
summary(pool(with(imp, lm(CDI_TOT_FU~CDI_TOT+SEX+S3AGE+POVERTY+nonwhite+scale(TotalStress_FU,scale=F)*ANY_ABUSE_CHILD_DV))))
summary(lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+scale(TotalStress,scale=F),data=mtss2[which(mtss2$ANY_ABUSE_CHILD_DV==1),]))
#Maltreatment and Hippocampal Volume
summary(pool(with(imp, lm(Hippocampus~SEX+POVERTY+nonwhite+ICVol+ANY_ABUSE_CHILD_DV*scale(S3AGE,scale=F)))))
#Hipocampal volume and depression
summary(pool(with(imp, lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+Hippocampus+ICVol))))
#Hippocampal volume x Stressful life events 
summary(pool(with(imp, lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+scale(TotalStress,scale=F)*scale(Hippocampus,scale=F)+ICVol))))
summary(pool(with(imp, lm(CDI_TOT_FU~CDI_TOT+SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+scale(TotalStress_FU,scale=F)*scale(Hippocampus,scale=F)+ICVol))))
