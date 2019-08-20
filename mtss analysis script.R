#Read in data
mtss<-read.csv("mtss.csv")

#Descriptives Table
#Sex
table(mtss$SEX,mtss$ANY_ABUSE_CHILD_DV)
chisq.test(mtss$SEX,mtss$ANY_ABUSE_CHILD_DV)
#Race
table(mtss$RACE,mtss$ANY_ABUSE_CHILD_DV)
chisq.test((mtss$RACE==2),mtss$ANY_ABUSE_CHILD_DV)
chisq.test((mtss$RACE==3),mtss$ANY_ABUSE_CHILD_DV)
chisq.test((mtss$RACE==4),mtss$ANY_ABUSE_CHILD_DV)
chisq.test((mtss$RACE==5),mtss$ANY_ABUSE_CHILD_DV)
#Poverty
table(mtss$POVERTY,mtss$ANY_ABUSE_CHILD_DV)
chisq.test(mtss$POVERTY,mtss$ANY_ABUSE_CHILD_DV)
#Age
t.test(mtss$S3AGE~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$S3AGE[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$S3AGE[which(mtss$ANY_ABUSE_CHILD_DV==0)])
#Stressful Life Events
t.test(mtss$S3AGE~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$S3AGE[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$S3AGE[which(mtss$ANY_ABUSE_CHILD_DV==0)])
#Hippocampus
mtss$Hippocampus<-mtss$Hippocampus/1000
t.test(mtss$Hippocampus~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$Hippocampus[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$Hippocampus[which(mtss$ANY_ABUSE_CHILD_DV==0)])
#Amygdala
mtss$Amygdala<-mtss$Amygdala/1000
t.test(mtss$Amygdala~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$Amygdala[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$Amygdala[which(mtss$ANY_ABUSE_CHILD_DV==0)])
#IC Volume
mtss$ICVol<-mtss$ICVol/100000
t.test(mtss$ICVol~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$ICVol[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$ICVol[which(mtss$ANY_ABUSE_CHILD_DV==0)])

#Stressful Life Events
t.test(mtss$TotalStress~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$TotalStress[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$TotalStress[which(mtss$ANY_ABUSE_CHILD_DV==0)])

t.test(mtss$TotalStress_FU~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$TotalStress_FU[which(mtss$ANY_ABUSE_CHILD_DV==1)],na.rm=T)
sd(mtss$TotalStress_FU[which(mtss$ANY_ABUSE_CHILD_DV==0)],na.rm=T)

#Depression
t.test(mtss$CDI_TOT~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$CDI_TOT[which(mtss$ANY_ABUSE_CHILD_DV==1)])
sd(mtss$CDI_TOT[which(mtss$ANY_ABUSE_CHILD_DV==0)])

t.test(mtss$CDI_TOT_FU~mtss$ANY_ABUSE_CHILD_DV)
sd(mtss$CDI_TOT_FU[which(mtss$ANY_ABUSE_CHILD_DV==1)],na.rm=T)
sd(mtss$CDI_TOT_FU[which(mtss$ANY_ABUSE_CHILD_DV==0)],na.rm=T)

#Impute missing poverty data
require(mice)
set.seed(123)
mtss2<-mtss[,2:14]
w<-is.na(mtss2)
w[,8]<-FALSE
w[,9]<-FALSE
w[,13]<-FALSE
imp<-mice(mtss2,m=100,where=w)

#Analyses
#Stress Sensitization to depression
summary(pool(with(imp, lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+scale(TotalStress,scale=F)*ANY_ABUSE_CHILD_DV))))
summary(pool(with(imp, lm(CDI_TOT_FU~CDI_TOT+SEX+S3AGE+POVERTY+nonwhite+scale(TotalStress_FU,scale=F)*ANY_ABUSE_CHILD_DV+DAYS_S1))))
#Violence and Hippocampal Volume
summary(pool(with(imp, lm(Hippocampus~POVERTY+nonwhite+ICVol+ANY_ABUSE_CHILD_DV))))
#Violence and Amygdala Volume
summary(pool(with(imp, lm(Amygdala~POVERTY+nonwhite+ICVol+ANY_ABUSE_CHILD_DV))))
#Hipocampal volume and depression
summary(pool(with(imp, lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+ICVol+Hippocampus))))
summary(pool(with(imp, lm(CDI_TOT_FU~CDI_TOT+SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+ICVol+DAYS_S1+Hippocampus))))
#Amygdala volume and depression
summary(pool(with(imp, lm(CDI_TOT~SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+ICVol+Amygdala))))
summary(pool(with(imp, lm(CDI_TOT_FU~CDI_TOT+SEX+S3AGE+POVERTY+nonwhite+ANY_ABUSE_CHILD_DV+ICVol+DAYS_S1+Amygdala))))
#Hippocampal volume x Stressful life events 
summary(pool(with(imp, lm(CDI_TOT_FU~SEX+S3AGE+DAYS_S1+nonwhite+POVERTY+CDI_TOT+ANY_ABUSE_CHILD_DV+ICVol+scale(TotalStress_FU,scale=F)*scale(Hippocampus,scale=F)))))
#Amygdala volume x Stressful life events
summary(pool(with(imp, lm(CDI_TOT_FU~SEX+S3AGE+DAYS_S1+nonwhite+POVERTY+CDI_TOT+ANY_ABUSE_CHILD_DV+ICVol+scale(TotalStress_FU,scale=F)*scale(Amygdala,scale=F)))))
