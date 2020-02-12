#Data Frame creation For BMF paper
#DF1 = FB and JK match 
#DF2= FB and BMF match
#DF3 = JK and BMF match
#DF4 JK & FB match BMF
#.libPaths("D:\\programs\\R_packages") ; 

library(checkpoint)
checkpoint("2018-04-27")
library(readr);library(magrittr);library(dplyr);
library(dplyr);library(caret)
library(broom);

#setwd("\\\\mkax341/Projects/DAIRYPROD/6657/BCS/Ingenera_BMF//Analysis")
setwd("C:/Users/olearyn2/OneDrive - Lincoln University/BCS")

#Load BMF, trim and recreate french score
BMF <- read.table('Raw_Data/BMFData2016.CSV', 
                      header=TRUE,sep=",", dec=".")
BMF<-BMF[c(1:1945),] # blank rows at the end gone
BMF$BCS.0.5<-(BMF$BCS.1.5/0.8)-1 # French original

#Load assessor data
Human<- read.table("Raw_Data/AssessorScore2016.CSV", 
                   header=TRUE,sep=",", dec=".")
Human$HumanMean<-(Human$MANUALBCS_FB+Human$MANUALBCS_JK)/2
#Variable types
BMF$Date<-as.Date(BMF$DATE,'%d/%m/%Y')
Human$Date<-as.Date(Human$DATEOFSCORING,'%d/%m/%Y')
BMF$JUMBO<-as.numeric(substring(BMF$EARTAG, 8) )
Human$JUMBO<-as.numeric(Human$JUMBO)
#Lists for subsetting
FBList<-is.na(Human$MANUALBCS_FB) # FB records
HumDF2<- Human[!FBList,] # All FB 
JKList<-is.na(Human$MANUALBCS_JK) # JK records 
HumDF3<- Human[!JKList,] #All JK
List4<-is.na(HumDF3$MANUALBCS_FB) # FB match Jk

#Full data set (0 before train/test)
DF10<-HumDF3[!List4,] # no bmf data 
DF20<- inner_join(HumDF2,BMF,by=c('JUMBO','Date'))
DF30<- inner_join(HumDF3,BMF,by=c('JUMBO','Date'))
DF40<- inner_join(DF10,BMF,by=c('JUMBO','Date'))

#4 diverse groups 
Bal<-Human[,c(2,4,5,8,11,14)]
Bal<-Human %>% filter(DATEOFSCORING=="12/04/2016") 
# 102 - missing 5 cows, 5 cows without a human mean
balna<-is.na(Bal$HumanMean)
Bal<-Bal[!balna,]

#need to stratify first by BCS - then lactation
#so split first by lacation - a & b
BalLact<-Bal[order(Bal$HumanMean),]
a<-BalLact[c(0:(nrow(BalLact)-(nrow(BalLact)/2))), ] 
b<-BalLact[(nrow(BalLact)/2):nrow(BalLact),]
#summary(a$HumanMean)
#summary(b$HumanMean) 
#summary(a$LACTATION) #Low BCS (2.75)
#summary(b$LACTATION) #High BCS (3)

#Human mean split
a<-a[order(a$LACTATION),] 
b<-b[order(b$LACTATION),]

c<-a[c(0:24),] #low partity, low bcs
d<-a[c(25:48),] # High parity, Low bcs

e<-b[c(0:25),] #Low parity , High bcs
f<-b[c(26:49),] #high parity, high bcs

#summary(c$HumanMean)
#summary(d$HumanMean)
#summary(e$HumanMean)
#summary(f$HumanMean)

#summary(c$LACTATION)
#summary(d$LACTATION)
#summary(e$LACTATION)
#summary(f$LACTATION)

#cdef is then createDatapartition

#lists of cows
c.list<-data.frame(unique(c$JUMBO))
d.list<-data.frame(unique(d$JUMBO))
e.list<-data.frame(unique(e$JUMBO))
f.list<-data.frame(unique(f$JUMBO))

#random draw - 0.49 gets 50% of records (repeated measures)
set.seed(1)
#true/ false
CinTrain<-createDataPartition(y=c.list$unique.c.JUMBO.,p=0.49,list=F)
DinTrain<-createDataPartition(y=d.list$unique.d.JUMBO.,p=0.49,list=F)
EinTrain<-createDataPartition(y=e.list$unique.e.JUMBO.,p=0.49,list=F)
FinTrain<-createDataPartition(y=f.list$unique.f.JUMBO.,p=0.49,list=F)

#select cows based on True or false
cTrainList<-c.list[CinTrain,]
dTrainList<-d.list[DinTrain,]
eTrainList<-e.list[EinTrain,]
fTrainList<-f.list[FinTrain,]

trainBMFlist<-c(cTrainList,dTrainList,eTrainList,fTrainList)
#export this this =
TodayDate<-as.character(Sys.Date())

#setwd("\\\\mkax341/Projects/DAIRYPROD/6657/BCS/Ingenera_BMF//Analysis//TrainList")
#write.csv(TodayDate, x=trainBMFlist)
#setwd("\\\\mkax341/Projects/DAIRYPROD/6657/BCS/Ingenera_BMF//Analysis")

Bal2<-Human %>% filter(DATEOFSCORING=="12/04/2016") 
Baltrain<-Human%>% filter (JUMBO %in% trainBMFlist) %>% filter(DATEOFSCORING=="12/04/2016") 
Baltest<-Human%>% filter (!JUMBO %in% trainBMFlist) %>% filter(DATEOFSCORING=="12/04/2016") 
#summary(Baltrain$LACTATION)
#summary(Baltest$LACTATION) #0.15 difference, median out by 1, 
#summary(Baltrain$HumanMean)
#summary(Baltest$HumanMean) #broadly similar

trainBMFdata<-DF40%>% filter (JUMBO %in% trainBMFlist)

#train model
#names(trainBMFdata)
cmod1 <-lm (HumanMean~BCS.0.5, data=trainBMFdata)
#summary(cmod1) 
#tidy(cmod1)
#glance(cmod1)

#tree - 
#rm(fit)
#fit<- rpart(HumanMean~BCS.0.5,trainBMFdata,method="anova" )
#trainBMFdata$treePred<-predict(fit,trainBMFdata)
#modtreetrain<-lm(trainBMFdata$HumanMean~trainBMFdata$treePred)
#summary(modtreetrain) # overfitted
#complex<-printcp(fit)

#summary(fit)
tiff("FileName.tiff", height = 12, width = 17, units = 'cm', compression = "lzw",res = 300)

#rpart.plot(fit)

#Test set 
DF1<-DF10 %>% filter (!JUMBO %in% trainBMFlist)
DF2<-DF20 %>% filter (!JUMBO %in% trainBMFlist)
DF3<-DF30 %>% filter (!JUMBO %in% trainBMFlist)
DF4<-DF40 %>% filter (!JUMBO %in% trainBMFlist)
#pred
trainBMFdata$ModPred<-predict(object = cmod1,newdata = trainBMFdata)
DF2$ModPred<-predict(object = cmod1,newdata = DF2)
DF3$ModPred<-predict(object = cmod1,newdata = DF3)
DF4$ModPred<-predict(object = cmod1,newdata = DF4)

vmod<-lm(DF4$HumanMean~DF4$ModPred)
#summary(vmod)

#plot(DF4$ModPred,DF4$HumanMean,xlim = c(2.2,3.4))

# not a great slope - regressing scores to mean
# DF2$rtreePred<-predict(fit,newdata = DF2)
# DF3$rtreePred<-predict(fit,newdata = DF3)
# DF4$rtreePred<-predict(fit,newdata = DF4)
# modtreetest<-lm(DF4$HumanMean~DF4$rtreePred)
#summary(modtreetest)

