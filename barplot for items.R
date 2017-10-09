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

levels(ag_dat$ttt)<-c("TECSE","TRC")


levels(ag_dat$clause)<-c("Indirect Object","Intransitive","Object","Oblique","Transitive")
  
  

ag_dat$clause<-factor(ag_dat$clause,levels=c("Intransitive", "Transitive", "Object", "Oblique", "Indirect Object"))

myPalette<-c("grey","black")


my.gen.resp<-expand.grid(clause=c("Indirect Object","Intransitive","Object","Oblique","Transitive"), ttt=c("TECSE","TRC"),item=5:10,resp=0)

ag_dat2<-rbind(ag_dat,my.gen.resp)

ggplot(data=ag_dat2, aes(x=item, y=resp, fill=ttt)) +
  geom_bar(stat="identity",position="dodge")+facet_grid(. ~ clause) +
  coord_flip() + theme_bw()+ theme(legend.position = "right",legend.title = element_blank(),strip.text.x = element_text(size = 14),axis.title = element_text(size = 14),legend.text = element_text(size=12),axis.text = element_text(size=12))  + ylab("Counts of correct responses")+scale_fill_manual(values=myPalette)




