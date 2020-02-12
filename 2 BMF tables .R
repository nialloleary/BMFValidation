#Table 1 training part
table(DF1$Date)
table(DF2$Date)
table(DF3$Date)
table(BMF$Date)

#Table 2

#April 12th results for start of results text
Apr<- DF4 %>% filter(DATEOFSCORING=="12/04/2016") 
Table2<-rbind(summary(Apr$DIM.x),summary(Apr$LACTATION.x), summary(Apr$MANUALBCS_FB),summary(Apr$MANUALBCS_JK), summary(predict(object = cmod1,newdata = Apr)), summary(Apr$HumanMean))

Table2<-Table2[,c(4,3,1,6,2,5)]
write.csv(Table2,file='Table2.csv')



rmse <- function(error)
{
  sqrt(mean(error^2))
}
rmse(trainBMFdata$HumanMean-trainBMFdata$ModPred)
rmse(DF4$HumanMean-DF4$rtreePred)
rmse(DF4$HumanMean-DF4$ModPred)
#Table 3 #Systematic bias  - removed
#column 1
mean(DF10$MANUALBCS_JK-DF10$MANUALBCS_FB)
mean(DF2$rtreePred-DF2$MANUALBCS_FB) 
mean(DF2$ModPred-DF2$MANUALBCS_FB) 
mean(DF4$HumanMean-DF4$MANUALBCS_FB)

#column 2
#opposite
#blank
mean(DF3$rtreePred-DF3$MANUALBCS_JK)
mean(DF3$ModPred-DF3$MANUALBCS_JK)
mean(DF4$HumanMean-DF4$MANUALBCS_JK)

#Col3 
#3
mean(DF4$ModPred-DF4$rtreePred)
mean(DF4$ModPred-DF4$HumanMean)

#col4 bmf linear
mean(DF3$MANUALBCS_JK-DF3$ModPred)
mean(DF4$HumanMean-DF4$ModPred)

#table 4 - see  sheet 5 - repeatability

#Table 5 & 6
table(DF10$Date) # times FB & JK 
table(HumDF2$Date) # FB 
table(HumDF3$Date) # JK
table(BMF$Date) 
table(DF2$Date) # FB & BMF
table(DF3$Date) # JK & BMF

#Table 5 row 1 , column 1
DF10$agree<-DF10$MANUALBCS_FB==DF10$MANUALBCS_JK
table(DF10$agree) 
#table(DF10$Date)
226/426
table(DF10$agree,DF10$Date)

#Table 5 row 2 , column 1
DF4$diff<-sqrt((DF4$rtreePred-DF4$HumanMean)^2)
DF4$d125<-DF4$diff>0.125
table(DF4$Date)
table(DF4$d125)
87/216
table(DF4$d125,DF4$Date) # greater than

#Table 5 row 3 , column 1
DF4$diffm<-sqrt((DF4$ModPred-DF4$HumanMean)^2)
DF4$dm125<-DF4$diffm>0.125
table(DF4$Date)
table(DF4$dm125)
100/216
table(DF4$dm125,DF4$Date) # greater than

#Table 5 row 1 , column 2
DF10$diff<-sqrt((DF10$MANUALBCS_JK - DF10$MANUALBCS_FB)^2)

DF10$d375<-DF10$diff>0.375
table(DF10$Date)
table(DF10$d375)
12/426
table(DF10$Date,DF10$d375)
#Table 5 row 2
DF4$difft<-sqrt((DF4$rtreePred-DF4$HumanMean)^2)
DF4$dt375<-DF4$difft>0.375
table(DF4$Date)
table(DF4$dt375)
3/216
table(DF4$Date,DF4$dt375) # greater than

#Table 5 row 3
DF4$diffm<-sqrt((DF4$ModPred-DF4$HumanMean)^2)
DF4$dm375<-DF4$diffm>0.375
table(DF4$Date)
table(DF4$dm375)
2/216
table(DF4$Date,DF4$dm375) # greater than

#Table 6 Concordance

library(cccrm) #
#need data in long format for repeated measures
#FB - JK
DF10A<-DF10
DF10B<-DF10
DF10A$Method<-"FB"
DF10B$Method<-"JK"
DF10A$result<-DF10A$MANUALBCS_FB
DF10B$result<-DF10B$MANUALBCS_JK
DF10C<-rbind(DF10A,DF10B)
C1<-ccclon(dataset= DF10C,"result","JUMBO",
           rtime="Date", # repeated measures
           rmet="Method")  
C1 # 0.68 or 0.668?


#Tree FB (Tree Buckley TB)
TBA<-DF2
TBB<-DF2
TBA$Method<-"FB"
TBB$Method<-"BMFt"
TBA$result<-TBA$MANUALBCS_FB
TBB$result<-TBB$rtreePred
TBC<-rbind(TBA,TBB)
C2<-ccclon(dataset= TBC,"result","JUMBO",
           rtime="Date",rmet="Method")  
C2 # 0.53 or 0.46 / 0.55

#Tree JK - TJ
TJA<-DF3
TJB<-DF3
TJA$Method<-"JK"
TJB$Method<-"BMFt"
TJA$result<-TJA$MANUALBCS_JK
TJB$result<-TJB$rtreePred
TJC<-rbind(TJA,TJB)
C3<-ccclon(dataset= TJC,"result","JUMBO",
           rtime="Date",rmet="Method")  
C3 # 0.53 / 0.55

#Tree - mean
DF4A<-DF4
DF4B<-DF4
DF4A$Method<-"Mean"
DF4B$Method<-"BMFt"

DF4A$result<-DF4A$HumanMean
DF4B$result<-DF4B$rtreePred
DF4C<-rbind(DF4A,DF4B)
C4<-ccclon(dataset= DF4C,"result","JUMBO",
           rtime="Date",rmet="Method")  
C4 # # 0.67 - tree 0.63(seed 6) 0.68 see 1

#Linear 

#Linear FB
LBA<-DF2
LBB<-DF2
LBA$Method<-"FB"
LBB$Method<-"BMFl"
LBA$result<-LBA$MANUALBCS_FB
LBB$result<-LBB$ModPred
LBC<-rbind(LBA,LBB)
L1<-ccclon(dataset= LBC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L1 # 0.53

#Tree JK
LJA<-DF3
LJB<-DF3
LJA$Method<-"JK"
LJB$Method<-"BMFl"
LJA$result<-LJA$MANUALBCS_JK
LJB$result<-LJB$ModPred
LJC<-rbind(LJA,LJB)
L2<-ccclon(dataset= LJC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L2 # 0.53

#Linear model - mean 
LMA<-DF4
LMB<-DF4
LMA$Method<-"Mean"
LMB$Method<-"BMFl"
LMA$result<-LMA$HumanMean
LMB$result<-LMB$ModPred0 #????????
LMC<-rbind(LMA,LMB)
L3<-ccclon(dataset= LMC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L3 # 0.53 / 0.69 / 0.71 / 0.65(6) 0.705( seed 1)
#Slightly better for old equation 
