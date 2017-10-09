###########################################################
#
# Script to load and clean Pauline's data (animation task)
#
###########################################################

#Mixed effects logistic regression

#created by p.thompson - 13-02-2017
#updated by p.thompson - 10/04/2017, 
#changed model structure - p.thompson 01/06/2017

#load excel data 

username="pthompson" # change if neeeded to dbishop.
#username='dbishop'
location <- paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\",sep="")
options(scipen=999) #http://stackoverflow.com/questions/25946047/how-to-prevent-scientific-notation-in-r

############################################################

library(dplyr)
library(plyr)
library(car)
#install.packages("tidyverse")
library(tidyverse)
library(readxl)

pauline.dat<-read_excel(paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\dataforPaul.xlsx"))

sub.pauline<-pauline.dat[,1:20]

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

sub.pauline$itemOrder<-car::recode(sub.pauline$itemOrder,"'backward'='backwards';'forward'='forwards'")

sub.pauline2<-sub.pauline[,c(1,4,8:12,14:18,20)]


names(sub.pauline2)<-c("ID", "Age_m", "TRC_intransitive",  
                       "TRC_transitive", "TRC_object",                
                       "TRC_oblique", "TRC_indobject",       
                       "TECSE_intransitive", "TECSE_transitive",  
                       "TECSE_object", "TECSE_oblique",             
                       "TECSE_indobject","itemorder")             


pauline_long<-gather(sub.pauline2,factor,resp,TRC_intransitive:TECSE_indobject)
pauline_long$ttt<-pauline_long$clause<-vector(mode="numeric",length=length(sub.pauline2[,1]))
for(i in 1:length(pauline_long[,1]))
{pauline_long$ttt[i]<-unlist(strsplit(pauline_long$factor[i], split='_', fixed=TRUE))[1]
pauline_long$clause[i]<-unlist(strsplit(pauline_long$factor[i], split='_', fixed=TRUE))[2]
}

pauline_long$clause<-factor(pauline_long$clause, levels=c("intransitive","transitive","indobject","object","oblique"))
pauline_long$ttt<-factor(pauline_long$ttt,levels=c("TRC","TECSE"))

pauline_long$itemorder<-factor(pauline_long$itemorder)

######################################################################################
######################################################################################
######################################################################################

#install_github("lme4", user = "lme4")

library(lme4)
## Loading required package: Matrix
packageVersion("lme4")

################################################################################
#Model 2: more strict coding
################################################################################

sub2<-pauline_long

sub2$resp<-car::recode(sub2$resp,"2=1;1=0")

sub2$Age_c<-scale(sub1$Age_m)

fit1b <- glmer(resp ~ ttt + Age_c + clause + clause*ttt + Age_c*ttt +itemorder+ (1 | ID), family = binomial, data = sub2, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

#model 1 output
summary(fit1b)

#CI for estimates
se1<-sqrt(diag(vcov(fit1b)))
tab2<-cbind(Est=fixef(fit1b),LL=fixef(fit1b)-1.96*se1,UL=fixef(fit1b)+1.96*se1)


#odds ratio model2

exp(tab2)

# added by DB 19/6/17 to format output for table
tab1<-data.frame(round(exp(tab2),2))#round to 2 decimal places
p<-summary(fit1b)$coefficients[,4]
tablim<-paste(tab2$LL,'to',tab2$UL)
tab_out2<-cbind(tab2$Est,tablim,round(p,4))
colnames(tab_out2)<-c('Odds ratio','95% CI','p')
write.table(tab_out2,'Results_regression_v2',sep="\t")

