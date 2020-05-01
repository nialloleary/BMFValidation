#Table 1 
table(DF2$Date)
table(DF3$Date)
table(BMF$Date)

#Table 2 #April 12th results for start of results text
Apr<- DF4 %>% filter(DATEOFSCORING=="12/04/2016") 
Table2<-rbind(summary(Apr$DIM.x),summary(Apr$LACTATION.x), summary(Apr$MANUALBCS_FB),summary(Apr$MANUALBCS_JK), summary(predict(object = cmod1,newdata = Apr)), summary(Apr$HumanMean))

Table2<-Table2[,c(4,3,1,6,2,5)]
write.csv(Table2,file='Table2.csv')

#Table 3 Concordance

library(cccrm) #
#need data in long format for repeated measures
#FB - JK
DF1A<-DF1
DF1B<-DF1
DF1A$Method<-"FB"
DF1B$Method<-"JK"
DF1A$result<-DF1A$MANUALBCS_FB
DF1B$result<-DF1B$MANUALBCS_JK
DF10C<-rbind(DF1A,DF1B)
C1<-ccclon(dataset= DF10C,"result","JUMBO",
           rtime="Date", # repeated measures
           rmet="Method")  
C1 


#BMF - FB
LBA<-DF2
LBB<-DF2
LBA$Method<-"FB"
LBB$Method<-"BMFl"
LBA$result<-LBA$MANUALBCS_FB
LBB$result<-LBB$ModPred
LBC<-rbind(LBA,LBB)
L1<-ccclon(dataset= LBC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L1 # 

#BMF- JK
LJA<-DF3
LJB<-DF3
LJA$Method<-"JK"
LJB$Method<-"BMFl"
LJA$result<-LJA$MANUALBCS_JK
LJB$result<-LJB$ModPred
LJC<-rbind(LJA,LJB)
L2<-ccclon(dataset= LJC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L2 # 

#BMF - mean 
LMA<-DF4
LMB<-DF4
LMA$Method<-"Mean"
LMB$Method<-"BMFl"
LMA$result<-LMA$HumanMean
LMB$result<-LMB$ModPred #????????
LMC<-rbind(LMA,LMB)
L3<-ccclon(dataset= LMC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L3 # 
#Pearsons R for comparison with Mullin's et al.
cor.test(Apr$MANUALBCS_FB,Apr$MANUALBCS_JK)
cor.test(Apr$ModPred,Apr$MANUALBCS_FB)
cor.test(Apr$ModPred,Apr$MANUALBCS_JK)
cor.test(Apr$HumanMean,Apr$ModPred)
