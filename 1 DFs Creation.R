#Data Frame creation For BMF paper
#DF1 = FB and JK match 
#DF2= FB and BMF match
#DF3 = JK and BMF match
#DF4 JK & FB match BMF

#library(checkpoint)
#checkpoint("2018-04-27")
library(readr);library(magrittr);library(dplyr);library(dplyr);library(caret)
library(broom);

#setwd("\\\\mkax341/Projects/DAIRYPROD/6657/BCS/Ingenera_BMF//Analysis")
setwd("C:/Users/olearyn2/OneDrive - Lincoln University/BCS/BMF validation")

#Load BMF, trim and recreate french score
BMF <- read.table('Raw_Data/BMFData2016.CSV', 
                      header=TRUE,sep=",", dec=".")
BMF<-BMF[c(1:1945),] # blank rows at the end gone
BMF$BCS.0.5<-(BMF$BCS.1.5/0.8)-1 # Convert back to French original from coarse conversion

#Load assessor data
Human<- read.table("Raw_Data/AssessorScore2016.CSV", 
                   header=TRUE,sep=",", dec=".")
Human$HumanMean<-(Human$MANUALBCS_FB+Human$MANUALBCS_JK)/2
#Variable types
BMF$Date<-as.Date(BMF$DATE,'%d/%m/%Y')
Human$Date<-as.Date(Human$DATEOFSCORING,'%d/%m/%Y')
BMF$JUMBO<-as.numeric(substring(BMF$EARTAG, 8) )
Human$JUMBO<-as.numeric(Human$JUMBO)
HumDF2<- Human[!is.na(Human$MANUALBCS_FB),] # All FB in human
HumDF3<- Human[!is.na(Human$MANUALBCS_JK),] #All JK

#Full data set 
DF1<-HumDF3[!is.na(HumDF3$MANUALBCS_FB),] 
DF2<- inner_join(HumDF2,BMF,by=c('JUMBO','Date')) #FB- BMF
DF3<- inner_join(HumDF3,BMF,by=c('JUMBO','Date')) #JK - BMF
DF4<- inner_join(DF1,BMF,by=c('JUMBO','Date'))

#For train and test of conversion, stratification implemented.  
#Step 1 = Create 4 diverse groups 
#Bal = Balanced sets creation
Bal<-Human %>% filter(DATEOFSCORING=="12/04/2016")  #date select
Bal<-Bal[!is.na(Bal$HumanMean),]

#Stratify by BCS - then lactation
BalLact<-Bal[order(Bal$HumanMean),]
a<-BalLact[c(0:(nrow(BalLact)-(nrow(BalLact)/2))), ] # 1/2 split
b<-BalLact[(nrow(BalLact)/2):nrow(BalLact),]
#summary(a$LACTATION) #Low BCS (2.75)
#summary(b$LACTATION) #High BCS (3)

a<-a[order(a$LACTATION),] 
b<-b[order(b$LACTATION),]

a1<-a[c(0:24),] #low partity, low bcs
a2<-a[c(25:48),] # High parity, Low bcs

b1<-b[c(0:25),] #Low parity , High bcs
b2<-b[c(26:49),] #high parity, high bcs



#random draw - 0.49 gets 50% of records (repeated measures)
set.seed(1)
#true/ false
a1inTrain<-createDataPartition(y=unique(a1$JUMBO),p=0.49,list=F)
a2inTrain<-createDataPartition(y=unique(a2$JUMBO),p=0.49,list=F)
b1inTrain<-createDataPartition(y=unique(b1$JUMBO),p=0.49,list=F)
b2inTrain<-createDataPartition(y=unique(b2$JUMBO),p=0.49,list=F)

#select cows based on True or false
a1TrainList<-unique(a1$JUMBO)[a1inTrain]
a2TrainList<-unique(a2$JUMBO)[a2inTrain]
b1TrainList<-unique(b1$JUMBO)[b1inTrain]
b2TrainList<-unique(b2$JUMBO)[b2inTrain]

trainBMFlist<-c(a1TrainList,a2TrainList,b1TrainList,b2TrainList) #stratified training sample
#export this this =
TodayDate<-as.character(Sys.Date())

#setwd("\\\\mkax341/Projects/DAIRYPROD/6657/BCS/Ingenera_BMF//Analysis//TrainList")
#write.csv(TodayDate, x=trainBMFlist)
#setwd("\\\\mkax341/Projects/DAIRYPROD/6657/BCS/Ingenera_BMF//Analysis")

trainBMFdata<-DF4%>% filter (JUMBO %in% trainBMFlist)

#train model
#names(trainBMFdata)
cmod1 <-lm (HumanMean~BCS.0.5, data=trainBMFdata)
summary(cmod1) 
tidy(cmod1)
glance(cmod1)


#Test set 
#DF1te<-DF1 %>% filter (!JUMBO %in% trainBMFlist)
DF2te<-DF2 %>% filter (!JUMBO %in% trainBMFlist)
DF3te<-DF3 %>% filter (!JUMBO %in% trainBMFlist)
DF4te<-DF4 %>% filter (!JUMBO %in% trainBMFlist)
#pred
trainBMFdata$ModPred<-predict(object = cmod1,newdata = trainBMFdata)
DF2te$ModPred<-predict(object = cmod1,newdata = DF2te)
DF3te$ModPred<-predict(object = cmod1,newdata = DF3te)
DF4te$ModPred<-predict(object = cmod1,newdata = DF4te)

vmod<-lm(DF4te$HumanMean~DF4te$ModPred)
#summary(vmod)

plot(DF4te$ModPred,DF4te$HumanMean,xlim = c(2.2,3.4))



