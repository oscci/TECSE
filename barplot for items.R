###########################################################
#
# Script to load and clean Pauline's data (animation task)
#
###########################################################



#created - p.thompson 02/06/2017

#load excel data 

username="pthompson" # change if neeeded to dbishop.

location <- paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\",sep="")

############################################################


library(plyr)

library(tidyverse)
library(readxl)
library(stringr)

pauline.dat<-read_excel(paste0("C:\\Users\\",username,"\\Dropbox\\Pauline_TECSE_TRC\\dataforPaul.xlsx"))

pauline.dat2<-pauline.dat[,c(1,25:94)]

names(pauline.dat2)<-c("ID","TRCSubjectIntrans1",      "TRCSubjectIntrans2",      "TRCSubjectIntrans3",      "TRCSubjectIntrans4",     
"TRCSubjectTrans1"   ,     "TRCSubjectTrans2",        "TRCSubjectTrans3",        "TRCSubjectTrans4",       
"TRCObject1"          ,    "TRCObject2",              "TRCObject3",              "TRCObject4",             
"TRCOblique1"          ,   "TRCOblique2",           "TRCOblique3",             "TRCOblique4",            
"TRCIndirectObject1"    ,  "TRCIndirectObject2",      "TRCIndirectObject3",      "TRCIndirectObject4",     
"TECSERCSubjectIntrans1" , "TECSERCSubjectIntrans2",  "TECSERCSubjectIntrans3",  "TECSERCSubjectIntrans4", 
"TECSERCSubjectIntrans5",  "TECSERCSubjectIntrans6",  "TECSERCSubjectIntrans7",  "TECSERCSubjectIntrans8",
"TECSERCSubjectIntrans9" , "TECSERCSubjectIntrans10", "TECSERCSubjectTrans1", "TECSERCSubjectTrans2",
"TECSERCSubjectTrans3" ,"TECSERCSubjectTrans4", "TECSERCSubjectTrans5", "TECSERCSubjectTrans6",
"TECSERCSubjectTrans7", "TECSERCSubjectTrans8", "TECSERCSubjectTrans9", "TECSERCSubjectTrans10",
"TECSERCSubjectObject1", "TECSERCSubjectObject2", "TECSERCSubjectObject3", "TECSERCSubjectObject4",
"TECSERCSubjectObject5", "TECSERCSubjectObject6", "TECSERCSubjectObject7", "TECSERCSubjectObject8",
"TECSERCSubjectObject9", "TECSERCSubjectObject10", "TECSERCSubjectOblique1", "TECSERCSubjectOblique2",
"TECSERCSubjectOblique3", "TECSERCSubjectOblique4", "TECSERCSubjectOblique5", "TECSERCSubjectOblique6",
"TECSERCSubjectOblique7", "TECSERCSubjectOblique8", "TECSERCSubjectOblique9", "TECSERCSubjectOblique10",
"TECSERCSubjectIndirectObject1", "TECSERCSubjectIndirectObject2", "TECSERCSubjectIndirectObject3", "TECSERCSubjectIndirectObject4",
"TECSERCSubjectIndirectObject5", "TECSERCSubjectIndirectObject6", "TECSERCSubjectIndirectObject7", "TECSERCSubjectIndirectObject8",
"TECSERCSubjectIndirectObject9", "TECSERCSubjectIndirectObject10")

           

pauline_long2<-gather(pauline.dat2,factor,resp,TRCSubjectIntrans1:TECSERCSubjectIndirectObject10)

pauline_long2$clause<-factor(str_extract(pauline_long2$factor,paste(c("Intrans","Trans","Object","Oblique","IndirectObject"), collapse="|") ))
pauline_long2$ttt<-factor(str_extract(pauline_long2$factor, "TRC|TECSE"))
pauline_long2$item<-factor(str_extract(pauline_long2$factor, "(\\d)+"),levels=c("1","2","3","4","5","6","7","8","9","10"))


ag_dat<-aggregate(resp ~ clause+ttt+item, pauline_long2, sum)

##########################################################################################

library(ggplot2)

levels(ag_dat$ttt)<-c("Animation","Multiple choice")


levels(ag_dat$clause)<-c("IO","SI","Obj","Obl","ST")
  
  

ag_dat$clause<-factor(ag_dat$clause,levels=c("SI", "ST", "Obj", "Obl", "IO"))

myPalette<-c("blue","red")

ggplot(data=ag_dat, aes(x=item, y=resp, fill=ttt)) +
  geom_bar(stat="identity",position="dodge")+facet_grid(. ~ clause) +
  coord_flip() + theme_bw()+ theme(legend.position = "top",legend.title = element_blank())  + ylab("Counts of correct responses")+scale_fill_manual(values=myPalette)




