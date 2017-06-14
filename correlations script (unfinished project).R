###########################################################
#
# Script for multiple comparisons
#
###########################################################

#created - p.thompson 09/06/2017

#load excel data 

username="pthompson" # change if neeeded to dbishop.

location <- paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\",sep="")

############################################################


library(plyr)

library(tidyverse)
library(readxl)
library(stringr)

pauline.dat<-read_excel(paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\dataforPaul.xlsx"))

pauline.dat3<-pauline.dat[,c("ID","AgeAtTesting in months","TotalTRCScore Mutiple choice","TotalTECS_ERC Score animation task","Setence recall standard score", "DAPMAge Cognitive age in months")]
names(pauline.dat3)<-c("ID","Age","Tecse_TS","TRC_TS","sentRec_SS","DAP_m")


#remove ids with missing data, so models are comparable.
pauline.dat3A<-as.data.frame(subset(pauline.dat3,!is.na(pauline.dat3$DAP_m)))


#TRC

lm0<-lm(TRC_TS~Age,data=pauline.dat3A)
lm1<-lm(TRC_TS~Age+sentRec_SS,data=pauline.dat3A)
lm2<-lm(TRC_TS~Age+DAP_m,data=pauline.dat3A)
lm3<-lm(TRC_TS~Age+sentRec_SS+DAP_m,data=pauline.dat3A) #according to likelihood ratio test, best fitting model.
anova(lm0,lm1)
anova(lm0,lm2)
anova(lm1,lm3)
anova(lm2,lm3)

#TECSE

lm0B<-lm(Tecse_TS~Age,data=pauline.dat3A)
lm1B<-lm(Tecse_TS~Age+sentRec_SS,data=pauline.dat3A)
lm2B<-lm(Tecse_TS~Age+DAP_m,data=pauline.dat3A)
lm3B<-lm(Tecse_TS~Age+sentRec_SS+DAP_m,data=pauline.dat3A)#according to likelihood ratio test, best fitting model.
anova(lm0B,lm1B)
anova(lm0B,lm2B)
anova(lm1B,lm3B)
anova(lm2B,lm3B)


#################

summary(lm3)
summary(lm3B)
par(mfrow=c(2,2))
plot(lm3)
plot(lm3B)


anova(lm0,lm1,lm2,lm3)

anova(lm0B,lm1B,lm2B,lm3B)

