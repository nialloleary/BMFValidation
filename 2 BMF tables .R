#Table 1 
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


#Table 3 #Systematic bias  - removed
#column 1
mean(DF1$MANUALBCS_JK-DF1$MANUALBCS_FB)
mean(DF2te$ModPred-DF2te$MANUALBCS_FB) 
mean(DF4te$HumanMean-DF4te$MANUALBCS_FB)

#column 2
mean(DF3te$ModPred-DF3te$MANUALBCS_JK)
mean(DF4$HumanMean-DF4$MANUALBCS_JK)

#Col3 
#3
mean(DF4$ModPred-DF4$HumanMean)

#col4 bmf linear
mean(DF3te$MANUALBCS_JK-DF3te$ModPred)
mean(DF4te$HumanMean-DF4te$ModPred)

#table 4 - see  sheet 5 - repeatability


#Table 5 Disagreement
DF1$agree<-DF1$MANUALBCS_FB==DF1$MANUALBCS_JK
table(DF1$agree) 
#table(DF10$Date)
226/426
table(DF1$agree,DF1$Date) # agreement best on first day together


#Table 5 row 3 , column 1
DF4te$diffm<-sqrt((DF4te$ModPred-DF4te$HumanMean)^2)
DF4te$dm125<-DF4te$diffm>0.125
table(DF4te$Date)
table(DF4te$dm125)
100/216
table(DF4te$dm125,DF4te$Date) # greater than ( same as above?)

#Table 5 row 1 , column 2
DF1$diff<-sqrt((DF1$MANUALBCS_JK - DF1$MANUALBCS_FB)^2)

DF1$d375<-DF1$diff>0.375
table(DF1$Date)
table(DF1$d375)
12/426
table(DF1$Date,DF1$d375)

#Table 5 row 3
DF4te$diffm<-sqrt((DF4te$ModPred-DF4te$HumanMean)^2)
DF4te$dm375<-DF4te$diffm>0.375
table(DF4te$Date)
table(DF4te$dm375)
2/216 # 2nd different one
table(DF4te$Date,DF4te$dm375) # greater than

#Table 6 Concordance

library(cccrm) #
#need data in long format for repeated measures
#FB - JK
DF1A<-DF1
DF1B<-DF1
DF1A$Method<-"FB"
DF1B$Method<-"JK"
DF1A$result<-DF1A$MANUALBCS_FB
DF1B$result<-DF1B$MANUALBCS_JK
DF10C<-rbind(DF10A,DF10B)
C1<-ccclon(dataset= DF10C,"result","JUMBO",
           rtime="Date", # repeated measures
           rmet="Method")  
C1 # 0.68 or 0.668?


#Linear 

#Linear FB
LBA<-DF2te
LBB<-DF2te
LBA$Method<-"FB"
LBB$Method<-"BMFl"
LBA$result<-LBA$MANUALBCS_FB
LBB$result<-LBB$ModPred
LBC<-rbind(LBA,LBB)
L1<-ccclon(dataset= LBC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L1 # 0.53 ->0.56

LJA<-DF3te
LJB<-DF3te
LJA$Method<-"JK"
LJB$Method<-"BMFl"
LJA$result<-LJA$MANUALBCS_JK
LJB$result<-LJB$ModPred
LJC<-rbind(LJA,LJB)
L2<-ccclon(dataset= LJC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L2 # 0.53

#Linear model - mean 
LMA<-DF4te
LMB<-DF4te
LMA$Method<-"Mean"
LMB$Method<-"BMFl"
LMA$result<-LMA$HumanMean
LMB$result<-LMB$ModPred #????????
LMC<-rbind(LMA,LMB)
L3<-ccclon(dataset= LMC,"result","JUMBO",
           rtime="Date",rmet="Method")  
L3 # 0.53 / 0.69 / 0.71 / 0.65(6) 0.705( seed 1)
#Slightly better for old equation 
