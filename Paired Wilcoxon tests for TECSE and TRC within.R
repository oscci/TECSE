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

#Load data

pauline.dat<-read_excel(paste0(location,"dataforPaul.xlsx"))

pauline.dat2<-pauline.dat[,c("ID", "Total TRC subject intransitive score", "Total TRC subject transitive score", "Total TRC object score", "Total TRC oblique score", "Total TRC indirect object score",
                             "Total TECSE subject intransitive score", "Total TECSE subject transitive score", "Total TECSE object score", "Total TECSE oblique score", "Total TECSE indirect object score")]
pauline.dat2<-as.data.frame(pauline.dat2)

#Check for normality of variables.

apply(pauline.dat2[,2:11],2,shapiro.test) # all fail
apply(pauline.dat2[,2:11],2,qqnorm)

#Rank columns of data by sum of scores in each column.

TRC_sub_rank<-pauline.dat2[,2:6][,c(order(colSums(pauline.dat2[,2:6])))]
TECSE_sub_rank<-pauline.dat2[,7:11][,c(order(colSums(pauline.dat2[,7:11])))]

TRC_test<-data.frame(statistic=1:4,p.value=1:4)

#Perform wilcoxon paired test (non-parametric equivalent of paired T-test)

#TRC data

for(i in 2:5)
{
  Test1<-wilcox.test(TRC_sub_rank[,i-1],TRC_sub_rank[,i],alternative="two.sided",paired=TRUE)
  TRC_test[i-1,1]<-Test1$statistic
  TRC_test[i-1,2]<-Test1$p.value
  TRC_test$p.value_corrected<-TRC_test$p.value<(0.05/4)
}
##############################################################

#TECSE data


TECSE_test<-data.frame(statistic=1:4,p.value=1:4)

for(i in 2:5)
{
  Test1<-wilcox.test(TECSE_sub_rank[,i-1],TECSE_sub_rank[,i],alternative="two.sided",paired=TRUE)
  TECSE_test[i-1,1]<-Test1$statistic
  TECSE_test[i-1,2]<-Test1$p.value
  TECSE_test$p.value_corrected<-TECSE_test$p.value<(0.05/4)
}


#descriptives added by DB 19/6/17
meansTRC<-colSums(TRC_sub_rank)/nrow(TRC_sub_rank)
meansTECSE<-colSums(TECSE_sub_rank)/nrow(TECSE_sub_rank)

