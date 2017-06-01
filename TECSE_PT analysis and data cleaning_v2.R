###########################################################
#
# Script to load and clean Pauline's data (animation task)
#
###########################################################

#Mixed effects logistic regressions (binomial contrasts for TECSE|TRC comparison)

#http://stackoverflow.com/questions/21082396/multinomial-logistic-multilevel-models-in-r
#http://stats.stackexchange.com/questions/238581/how-to-use-ordinal-logistic-regression-with-random-effects/238675#238675
#https://rpubs.com/bbolker/11703
#https://oup.silverchair-cdn.com/oup/backfile/Content_public/Journal/biomet/71/1/10.1093/biomet/71.1.11/2/71-1-11.pdf?Expires=1492176309&Signature=TrloPV4zJUvDcEab~Qj1Q2WCVW-rJ96cagZK3~YhbotcPxxOi8cc0O3KVzKDp1kbQv-VmwEWmrXJED~KlKTcnCPj7DEIEZWlbDK8eewssEZTnW2jp4u-PdbwDoSQHyM3ZJj5ruJw-F5B9i4ptdTtkhXTzDwQlKVDtwtMBCinSAgCPOFxfmh6RBhMun3VoqXVovZsI0RpeRff4-McKhraNbkrmUwjHoy83sVFLVK5F2Y6k1enlqBlL-~HqJKKSipqU5WE-JtLKyH4pcGNcbiwBUdr5ngL6EodKmbIYFjQn2pIirCfR7JS2XOlmH5Ci0OBr9qtGysgzR4zJvOastfjsg__&Key-Pair-Id=APKAIUCZBIA4LVPAVW3Q


#created by p.thompson - 13-02-2017
#updated by p.thompson - 10/04/2017, 
#changed model structure - p.thompson 01/06/2017

#load excel data 

username="pthompson" # change if neeeded to dbishop.

location <- paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\",sep="")

############################################################

library(dplyr)
library(plyr)
library(car)
#install.packages("tidyverse")
library(tidyverse)
library(readxl)

pauline.dat<-read_excel(paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\dataforPaul.xlsx"))

sub.pauline<-pauline.dat[,1:19]

library(car)

######################################################################################

sub.pauline <- as.data.frame(sub.pauline)

name1<-c("Total TRC subject intransitive score",  
  "Total TRC subject transitive score",   
  "Total TRC object score",                
  "Total TRC oblique score",              
  "Total TRC indirect object score")

for(i in name1)
{
  sub.pauline[,i]<-car::recode(sub.pauline[,i],"4=2;3=1;2=0;1=0")
}

######################################################################################

name2<-c("Total TECSE subject intransitive score",
"Total TECSE subject transitive score",
"Total TECSE object score",             
"Total TECSE oblique score",            
"Total TECSE indirect object score")

for(i in name2)
{
  sub.pauline[,i]<-car::recode(sub.pauline[,i],"10=2;9=2;8=1;7=0;6=0;5=0;4=0;3=0;2=0;1=0")
}

library(tidyr)
names(sub.pauline)[4]<-"Age_m"

sub.pauline2<-sub.pauline[,c(1,4,8:12,14:18)]


names(sub.pauline2)<-c("ID", "Age_m", "TRC_intransitive",  
 "TRC_transitive", "TRC_object",                
 "TRC_oblique", "TRC_indobject",       
 "TECSE_intransitive", "TECSE_transitive",  
 "TECSE_object", "TECSE_oblique",             
 "TECSE_indobject")             

pauline_long<-gather(sub.pauline2,factor,resp,TRC_intransitive:TECSE_indobject)
pauline_long$ttt<-pauline_long$clause<-vector(mode="numeric",length=length(sub.pauline2[,1]))
for(i in 1:length(pauline_long[,1]))
{pauline_long$ttt[i]<-unlist(strsplit(pauline_long$factor[i], split='_', fixed=TRUE))[1]
 pauline_long$clause[i]<-unlist(strsplit(pauline_long$factor[i], split='_', fixed=TRUE))[2]
}


pauline_long$clause<-factor(pauline_long$clause, levels=c("intransitive","transitive","indobject","object","oblique"))
pauline_long$ttt<-factor(pauline_long$ttt,levels=c("TRC","TECSE"))




######################################################################################
######################################################################################
######################################################################################

#install_github("lme4", user = "lme4")


library(lme4)
## Loading required package: Matrix
packageVersion("lme4")

################################################################################
#Model 1: contrast 1 and 2
################################################################################

sub1<-pauline_long

sub1$resp<-car::recode(sub1$resp,"2=1")

sub1$Age_c<-scale(sub1$Age_m)

fit1a <- glmer(resp ~ ttt + Age_c + clause + clause*ttt + Age_c*ttt + (1 | ID), family = binomial, data = sub1, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#model 1 output
summary(fit1a)

#CI for estimates
se1<-sqrt(diag(vcov(fit1a)))
tab1<-cbind(Est=fixef(fit1a),LL=fixef(fit1a)-1.96*se1,UL=fixef(fit1a)+1.96*se1)


#odds ratio model1

exp(tab1)


