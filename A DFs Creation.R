#Data Frame creation For BMF validation paper

library(readr);library(dplyr);
#library(magrittr);

setwd("C:/Users/Niall/OneDrive/BCS/BMF validation")
#Data in Github repo
#Load BMF data
BMF <- read.table('Raw_Data/BMFData2016.csv', header=TRUE,sep=",", dec=".")
BMF<-BMF[c(1:1945),] # trim blank rows at the end
BMF$BCS.0.5<-(BMF$BCS.1.5/0.8)-1 # Convert back to French original from coarse conversion

#Load assessor data
Human<- read.table("Raw_Data/AssessorScore2016.CSV", header=TRUE,sep=",", dec=".")
Human$HumanMean<-(Human$MANUALBCS_FB+Human$MANUALBCS_JK)/2
#Variable types
BMF$Date<-as.Date(BMF$DATE,'%d/%m/%Y')
Human$Date<-as.Date(Human$DATEOFSCORING,'%d/%m/%Y')
BMF$JUMBO<-as.numeric(substring(BMF$EARTAG, 8) )
Human$JUMBO<-as.numeric(Human$JUMBO)
HumDF2<- Human[!is.na(Human$MANUALBCS_FB),] # All FB in human
HumDF3<- Human[!is.na(Human$MANUALBCS_JK),] #All JK

#Full data set 
#DF1 = FB and JK match 
#DF2= FB and BMF match
#DF3 = JK and BMF match
#DF4 JK & FB match BMF
DF1<-HumDF3[!is.na(HumDF3$MANUALBCS_FB),] #FB-JK
DF2<- inner_join(HumDF2,BMF,by=c('JUMBO','Date')) #FB- BMF
DF3<- inner_join(HumDF3,BMF,by=c('JUMBO','Date')) #JK - BMF
DF4<- inner_join(DF1,BMF,by=c('JUMBO','Date'))


cmod1 <-lm (HumanMean~BCS.0.5, data=DF4) #train model
summary(cmod1) 
DF2$ModPred<-predict(object = cmod1,newdata = DF2)
DF3$ModPred<-predict(object = cmod1,newdata = DF3)
DF4$ModPred<-predict(object = cmod1,newdata = DF4)