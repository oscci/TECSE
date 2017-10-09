###########################################################
#
# Script for hierarchical linear regressions on total scores 
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

####################################################################################

#Perform multiple linear regresssions using likelihood ratio test and difference in R-squared values to determine inclusion|exclusion of predictors.


#TRC

lm0<-lm(TRC_TS~Age,data=pauline.dat3A)
lm1<-lm(TRC_TS~Age+sentRec_SS,data=pauline.dat3A)
lm2<-lm(TRC_TS~Age+sentRec_SS+DAP_m,data=pauline.dat3A) #according to likelihood ratio test, best fitting model.
anova(lm0,lm1,lm2)

#TECSE

lm0B<-lm(Tecse_TS~Age,data=pauline.dat3A)
lm1B<-lm(Tecse_TS~Age+sentRec_SS,data=pauline.dat3A)
lm2B<-lm(Tecse_TS~Age+sentRec_SS+DAP_m,data=pauline.dat3A)#according to likelihood ratio test, best fitting model.
anova(lm0B,lm1B,lm2B)

#################
#Get model summaries and diagnostic plots for final regressions.

summary(lm2)
summary(lm2B)
par(mfrow=c(2,2))
plot(lm2)
plot(lm2B)

#################

lm0C<-lm(sentRec_SS~Age,data=pauline.dat3A)
lm1C<-lm(sentRec_SS~Age+DAP_m,data=pauline.dat3A)
lm2C<-lm(sentRec_SS~Age+DAP_m+Tecse_TS,data=pauline.dat3A)
lm3C<-lm(sentRec_SS~Age+DAP_m+Tecse_TS+TRC_TS,data=pauline.dat3A)#according to likelihood ratio test, best fitting model.

anova(lm0C,lm1C,lm2C,lm3C)

# adding TRC seems to be beneficial as significant in the likelihood ratio test. However, the change in R-squared is very slight, so we gain only a minor amount of explanined variance with its addition. 


