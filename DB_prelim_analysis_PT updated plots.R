# Preliminary analysis for Pauline study comparing TECSE and TRC
# DVM Bishop, 24th April 2017
# Updated the plots and consolidated into one plot. - p.thompson 05/06/2017

# set working directory for Mac; 
#setwd("c://Users//pthompson//Dropbox/Pauline_TECSE_TRC")
#setwd("~/Dropbox/Pauline_TECSE_TRC")
#You will need to load packages XLConnect and doBy : go to Tools|Install Packages to do this  
# You only need do this once
library(doBy) #loads a package that is useful for summarising data
library(XLConnect) #package that read xls files
library(yarrr) #package for pirate plots
library(reshape2) # for converting data from wide to long form (see below)
options(scipen=999) #disable scientific notation.

# Read in raw data from xlsx file
myname<-'DataforDorothyPaul'
myfile<-paste0("c://Users//pthompson//Dropbox//Pauline_TECSE_TRC//",myname,".xlsx") #convert their ID into an excel filename - 'paste' joins things together so sticks on the filetype
myloaded<-loadWorkbook(myfile) # This is an XLConnect command for reading in an xlsx workbook into R
myrawdata<-data.frame(readWorksheet(myloaded,sheet=1))# reads in sheet 3 of xlsx file

# Compute proportion correct by test and syntax type (already done in xls but just to confirm)
myrawdata$TRC.SI<-rowSums(myrawdata[,25:28])/4
myrawdata$TRC.ST<-rowSums(myrawdata[,29:32])/4
myrawdata$TRC.O<-rowSums(myrawdata[,33:36])/4
myrawdata$TRC.Obl<-rowSums(myrawdata[,37:40])/4
myrawdata$TRC.IO<-rowSums(myrawdata[,41:44])/4
myrawdata$TEC.SI<-rowSums(myrawdata[,45:54])/10
myrawdata$TEC.ST<-rowSums(myrawdata[,55:64])/10
myrawdata$TEC.O<-rowSums(myrawdata[,65:74])/10
myrawdata$TEC.Obl<-rowSums(myrawdata[,75:84])/10
myrawdata$TEC.IO<-rowSums(myrawdata[,85:94])/10

# v clunky way of creating age bands
myrawdata$ageband<-1
myrawdata$ageband[myrawdata[,4]>43]=2
myrawdata$ageband[myrawdata[,4]>47]=3
myrawdata$ageband[myrawdata[,4]>51]=4
myrawdata$ageband[myrawdata[,4]>55]=5

# aggregate data frame for percentages correct, returning means and SDs
# NB This does not take into account that for TRC chance is .25, but for TECS it is .5

aggdatam <-aggregate(myrawdata[,95:104], by=list(myrawdata$ageband),
                     FUN=mean, na.rm=TRUE)
aggdatas<-aggregate(myrawdata[,95:104], by=list(myrawdata$ageband),
                    FUN=sd, na.rm=TRUE)

mycounts <-table(myrawdata$ageband)
colnames(aggdatam)[1]<-'Ageband'

#make pass fail scores; convert scores of 4-choice so 3->1, 4->2; convert scores of 2choice so 8->1 and 9-10 ->2
myrawdata[,106:115]<-0
colnames(myrawdata)[106:115]<-c('TRC.SI.pf','TRC.ST.pf','TRC.Obj.pf','TRC.Obl.pf','TRC.IO.pf',
                                'TEC.SI.pf','TEC.ST.pf','TEC.Obj.pf','TEC.Obl.pf','TEC.IO.pf')

for (j in 95:99){
  myrow<-which(myrawdata[,j]>.5)
  myrawdata[myrow,(j+11)]<-1
  myrow<-which(myrawdata[,j]>.75)
  myrawdata[myrow,(j+11)]<-2
  myrow<-which(myrawdata[,(j+5)]>.7)
  myrawdata[myrow,(j+16)]<-1
  myrow<-which(myrawdata[,(j+5)]>.8)
  myrawdata[myrow,(j+16)]<-2
}
###################################################################################################################

names(myrawdata)[108]<-"TRC.Obj.pf"
names(myrawdata)[113]<-"TEC.Obj.pf"

sub.raw<-myrawdata[,c("ID","ageband","TRC.SI.pf","TEC.SI.pf","TRC.ST.pf","TEC.ST.pf","TRC.Obj.pf","TEC.Obj.pf","TRC.Obl.pf","TEC.Obl.pf","TRC.IO.pf","TEC.IO.pf")]

raw_long<-tidyr::gather(sub.raw,factor,resp,TRC.SI.pf:TEC.IO.pf)

raw_long$clause<-factor(stringr::str_extract(raw_long$factor,paste(c("SI","ST","Obj","Obl","IO"), collapse="|") ))
raw_long$ttt<-factor(stringr::str_extract(raw_long$factor, "TRC|TEC"))
raw_long$resp<-factor(raw_long$resp,levels=c(2,1,0))
raw_long$ageband<-as.factor(raw_long$ageband)


library(dplyr)
raw_summary <- raw_long %>% 
  group_by(ageband,resp,clause,ttt) %>% 
  summarise(count=n())

raw_summary<-as.data.frame(raw_summary)
raw_summary$prop<-vector(mode="numeric",length=length(raw_summary[,1]))

for(i in 1:length(raw_summary[,1]))
{
raw_summary$prop[i]<-raw_summary$count[i]/sum(raw_summary$count[raw_summary$ageband==raw_summary$ageband[i]&raw_summary$clause==raw_summary$clause[i]&raw_summary$ttt==raw_summary$ttt[i]])
}

library(ggplot2)
windows(record=T)
ggplot(data=raw_summary, aes(x=ageband,y=prop,fill=resp)) +
  geom_bar(stat="identity")+facet_grid(ttt ~ clause) +
   theme(legend.position = "top") + theme_bw() + ylab("")#+ scale_fill_grey(start = 0.5, end = .8)


###################################################################################################################

# #jpeg("plots1.jpg")
# par(mfrow=c(2,1))
# temp<-table(myrawdata$ageband,myrawdata$TRC.SI.pf)
# TRC_SI_tab<-temp/rowSums(temp)
# barplot(t(TRC_SI_tab), main="TRC_Subject Intransitive",
#         xlab="Age band", col=c("pink","red","purple")) 
# temp<-table(myrawdata$ageband,myrawdata$TEC.SI.pf)
# TEC_SI_tab<-temp/rowSums(temp)
# barplot(t(TEC_SI_tab), main="TECS_Subject Intransitive",
#         xlab="Age band", col=c("pink","red","purple")) 
# #dev.off()
# 
# 
# #jpeg("plots2.jpg")
# par(mfrow=c(2,1))
# temp<-table(myrawdata$ageband,myrawdata$TRC.ST.pf)
# TRC_ST_tab<-temp/rowSums(temp)
# barplot(t(TRC_ST_tab), main="TRC_Subject Transitive",
#         xlab="Age band", col=c("pink","red","purple")) 
# temp<-table(myrawdata$ageband,myrawdata$TEC.ST.pf)
# TEC_ST_tab<-temp/rowSums(temp)
# barplot(t(TEC_ST_tab), main="TECS_Subject Transitive",
#         xlab="Age band", col=c("pink","red","purple")) 
# #dev.off()
# 
# #jpeg("plots3.jpg")
# par(mfrow=c(2,1))
# temp<-table(myrawdata$ageband,myrawdata$TRC.Obj.pf)
# TRC_O_tab<-temp/rowSums(temp)
# barplot(t(TRC_O_tab), main="TRC_Object",
#         xlab="Age band", col=c("pink","red","purple")) 
# temp<-table(myrawdata$ageband,myrawdata$TEC.Obj.pf)
# TEC_O_tab<-temp/rowSums(temp)
# barplot(t(TEC_O_tab), main="TECS_Object",
#         xlab="Age band", col=c("pink","red","purple")) 
# #dev.off()
# 
# #jpeg("plots4.jpg")
# par(mfrow=c(2,1))
# temp<-table(myrawdata$ageband,myrawdata$TRC.Obl.pf)
# TRC_Obl_tab<-temp/rowSums(temp)
# barplot(t(TRC_Obl_tab), main="TRC_Oblique",
#         xlab="Age band", col=c("pink","red","purple")) 
# temp<-table(myrawdata$ageband,myrawdata$TEC.Obl.pf)
# TEC_Obl_tab<-temp/rowSums(temp)
# barplot(t(TEC_Obl_tab), main="TECS_Oblique",
#         xlab="Age band", col=c("pink","red","purple")) 
# #dev.off()
# 
# #jpeg("plots5.jpg")
# par(mfrow=c(2,1))
# temp<-table(myrawdata$ageband,myrawdata$TRC.IO.pf)
# TRC_IO_tab<-temp/rowSums(temp)
# barplot(t(TRC_IO_tab), main="TRC_Indirect Object",
#         xlab="Age band", col=c("pink","red","purple")) 
# temp<-table(myrawdata$ageband,myrawdata$TEC.IO.pf)
# TEC_IO_tab<-temp/rowSums(temp)
# barplot(t(TEC_IO_tab), main="TECS_Indirect Object",
#         xlab="Age band", col=c("pink","red","purple")) 
# #dev.off()

