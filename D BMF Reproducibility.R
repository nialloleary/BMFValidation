
#Reproducibility using calculations as described in McAlinden et al 2015
DFrep1<- DF4 %>% select(JUMBO,DATE,MANUALBCS_FB,MANUALBCS_JK,HumanMean,ModPred)

# split by date
DFa<- DFrep1 %>% filter (DATE=='12/04/2016' ) # 97
DFb<- DFrep1 %>% filter (DATE=='10/05/2016' ) # 92
DFc<- DFrep1 %>% filter (DATE=='07/06/2016' ) # 91

DFrep2<- inner_join(DFa, DFb, by="JUMBO")

DFr<- inner_join(DFrep2, DFc, by="JUMBO") # 83 cows measured each period
df  <- data.frame(BCS = c(DFr$ModPred,DFr$HumanMean),
                  Method=rep(c('BMF','HumanMean'),each=83))

#BMF apr - may   X- Y          
DFr$BMFm<-(DFr$ModPred.x+DFr$ModPred.y)/2 #mean
DFr$BMFv2<-((DFr$ModPred.x-DFr$BMFm)^2+
              (DFr$ModPred.y-DFr$BMFm)^2)/82 #variance
sum(DFr$BMFv2)
mean(DFr$BMFv2)
sqrt(mean(DFr$BMFv2)) #SW reproducability
(1.96*sqrt(2))*sqrt(mean(DFr$BMFv2)) # repeatability limit

#FB apr - may 
DFr$MANUALBCS_FBm<-(DFr$MANUALBCS_FB.x+DFr$MANUALBCS_FB.y)/2
DFr$MANUALBCS_FBv2<-((DFr$MANUALBCS_FB.x-DFr$MANUALBCS_FBm)^2+(DFr$MANUALBCS_FB.y-DFr$MANUALBCS_FBm)^2)/82
sum(DFr$MANUALBCS_FBv2)
mean(DFr$MANUALBCS_FBv2)
sqrt(mean(DFr$MANUALBCS_FBv2)) # repeatability
(1.96*sqrt(2))*sqrt(mean(DFr$MANUALBCS_FBv2)) #Limit
# JK Apr - May
DFr$MANUALBCS_JKm<-(DFr$MANUALBCS_JK.x+DFr$MANUALBCS_JK.y)/2
DFr$MANUALBCS_JKv2<-((DFr$MANUALBCS_JK.x-DFr$MANUALBCS_JKm)^2+(DFr$MANUALBCS_JK.y-DFr$MANUALBCS_JKm)^2)/82
sum(DFr$MANUALBCS_JKv2)
mean(DFr$MANUALBCS_JKv2)
sqrt(mean(DFr$MANUALBCS_JKv2)) # repeatability
(1.96*sqrt(2))*sqrt(mean(DFr$MANUALBCS_JKv2)) # limit

#Human Mean Apr - May
DFr$HumanMeanm<-(DFr$HumanMean.x+DFr$HumanMean.y)/2
DFr$HumanMeanv2<-((DFr$HumanMean.x-DFr$HumanMeanm)^2+(DFr$HumanMean.y-DFr$HumanMeanm)^2)/82
sum(DFr$HumanMeanv2)
mean(DFr$HumanMeanv2)
sqrt(mean(DFr$HumanMeanv2)) # repeatability
(1.96*sqrt(2))*sqrt(mean(DFr$HumanMeanv2)) # limit

#BMF may - June            
DFr$BMFm<-(DFr$ModPred.y+DFr$ModPred)/2
DFr$BMFv2<-((DFr$ModPred.y-DFr$BMFm)^2+(DFr$ModPred-DFr$BMFm)^2)/82
sum(DFr$BMFv2)
mean(DFr$BMFv2)
sqrt(mean(DFr$BMFv2))
(1.96*sqrt(2))*sqrt(mean(DFr$BMFv2)) # repeatability limit

#FB may - June
DFr$MANUALBCS_FBm<-(DFr$MANUALBCS_FB+DFr$MANUALBCS_FB.y)/2
DFr$MANUALBCS_FBv2<-((DFr$MANUALBCS_FB-DFr$MANUALBCS_FBm)^2+(DFr$MANUALBCS_FB.y-DFr$MANUALBCS_FBm)^2)/82
sum(DFr$MANUALBCS_FBv2)
mean(DFr$MANUALBCS_FBv2)
sqrt(mean(DFr$MANUALBCS_FBv2))
(1.96*sqrt(2))*sqrt(mean(DFr$MANUALBCS_FBv2))

#JK may - June
DFr$MANUALBCS_JKm<-(DFr$MANUALBCS_JK+DFr$MANUALBCS_JK.y)/2
DFr$MANUALBCS_JKv2<-((DFr$MANUALBCS_JK-DFr$MANUALBCS_JKm)^2+(DFr$MANUALBCS_JK.y-DFr$MANUALBCS_JKm)^2)/82
sum(DFr$MANUALBCS_JKv2)
mean(DFr$MANUALBCS_JKv2)
sqrt(mean(DFr$MANUALBCS_JKv2))
(1.96*sqrt(2))*sqrt(mean(DFr$HumanMeanv2)) # better than Apr - May

#Human mean may - June
DFr$HumanMeanm<-(DFr$HumanMean+DFr$HumanMean.y)/2
DFr$HumanMeanv2<-((DFr$HumanMean-DFr$HumanMeanm)^2+(DFr$HumanMean.y-DFr$HumanMeanm)^2)/82
sum(DFr$HumanMeanv2)
mean(DFr$HumanMeanv2)
sqrt(mean(DFr$HumanMeanv2))
(1.96*sqrt(2))*sqrt(mean(DFr$HumanMeanv2)) # better than Apr - May
