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
#updated by p.thompson - 10/04/2017

#load excel data 

username="pthompson" # change if neeeded to dbishop.

location <- paste("C:\\Users\\",username,"\\\Dropbox\\Pauline Frizelle study power calc\\",sep="")

main.data<-loadWorkbook(paste(location,"animation.xlsx",sep=""), create = TRUE)

sheet1.data = readWorksheet(main.data, sheet = "Sheet1", startRow = 0, endRow = 10, startCol = 0, endCol = 0)

############################################################

library(dplyr)
library(plyr)
library(car)
install.packages("tidyverse")

pauline.dat<-read_excel(paste0(getwd(),"\\animation/dataforPaul.xlsx"))

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
  sub.pauline[,i]<-recode(sub.pauline[,i],"4=2;3=1;2=0;1=0")
}

######################################################################################

name2<-c("Total TECSE subject intransitive score",
"Total TECSE subject transitive score",
"Total TECSE object score",             
"Total TECSE oblique score",            
"Total TECSE indirect object score")

for(i in name2)
{
  sub.pauline[,i]<-recode(sub.pauline[,i],"10=2;9=2;8=1;7=0;6=0;5=0;4=0;3=0;2=0;1=0")
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


pauline_long$clause<-factor(pauline_long$clause, levels=c("indobject","intransitive","object","oblique","transitive"))
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
sub1<-pauline_long[!pauline_long$resp==2,]

fit1 <- glmer(resp ~ ttt + Age_m + (1 | ID) + (1 | clause), family = binomial, data = sub1)

#model 2 output
summary(fit1)

#CI for estimates
se1<-sqrt(diag(vcov(fit1)))
tab1<-cbind(Est=fixef(fit1),LL=fixef(fit1)-1.96*se1,UL=fixef(fit1)+1.96*se1)

#Confidence interval contains zero! no effect.

#odds ratio model1

exp(tab1)

# Here no effect of test on scores.

################################################################################
#Model 2: contrast 1 and 3
################################################################################
sub2<-pauline_long[!pauline_long$resp==1,]
sub2$resp<-recode(sub2$resp,"2=1")


fit2 <- glmer(resp ~ ttt + Age_m + (1 | ID) + (1 | clause), family = binomial, data = sub2)

#model 2 output
summary(fit2)


#CI for estimates
se2<-sqrt(diag(vcov(fit2)))
tab2<-cbind(Est=fixef(fit2),LL=fixef(fit2)-1.96*se2,UL=fixef(fit2)+1.96*se2)

#Confidence interval is OK, and does not contain zero or excessively wide.

#odds ratio model2

exp(tab2)

# Odds ratio is the odds of scoring higher in TECSE are 7.26 times larger than the odds for TRC.

################################################################################
#Model 3: contrast 2 and 3
################################################################################
sub3<-pauline_long[!pauline_long$resp==0,]
sub3$resp<-recode(sub3$resp,"1=0;2=1")


fit3 <- glmer(resp ~ ttt + Age_m + (1 | ID) + (1 | clause), family = binomial, data = sub3)

#model 2 output
summary(fit3)

#CI for estimates
se3<-sqrt(diag(vcov(fit3)))
tab3<-cbind(Est=fixef(fit3),LL=fixef(fit3)-1.96*se3,UL=fixef(fit3)+1.96*se3)

#Confidence interval is OK, and does not contain zero or excessively wide.


#odds ratio model3

exp(tab3)

# Odds ratio is the odds of scoring higher in TECSE are 4.96 times larger than the odds for TRC.



