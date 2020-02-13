#Each of the 3 measures
#Write function that calculates the next score with the same cow & days difference 
#correlation of error

# Need to compare same cows and same dates only to be a fair comparison?
#DF4  starting df - all matching records
table(DF4$Date) # why is it reporting 0 scores

DFrep1<- DF4 %>% select(JUMBO,DATE,MANUALBCS_FB,MANUALBCS_JK,HumanMean)
#Old equation
DFrep1$BMF<-(DF4$BCS.0.5*0.3991)+1.63434
# split by date
DFa<- DFrep1 %>% filter (DATE=='12/04/2016' ) # 97
DFb<- DFrep1 %>% filter (DATE=='10/05/2016' ) # 92
DFc<- DFrep1 %>% filter (DATE=='07/06/2016' ) # 91
DFd<- DFrep1 %>% filter (DATE=='19/07/2016' ) # 66
head(DFrep1)

DFrep2<- inner_join(DFa, DFb, by="JUMBO")
DFr<- inner_join(DFrep2, DFc, by="JUMBO") # 83

shapiro.test(DFr$MANUALBCS_FB.x) #normality test
shapiro.test(DFr$MANUALBCS_JK)
hist(DFr$MANUALBCS_FB.x)
shapiro.test(DFr$BMF.x) #- not normalizePath()
hist(DFr$BMF.x)
shapiro.test(DFr$BMF.y) #- defo not normalizePath()
hist(DFr$BMF.y)
shapiro.test(DFr$BMF) #- normal
hist(DFr$BMF)

#BMF apr - may            
DFr$BMFm<-(DFr$BMF.x+DFr$BMF.y)/2
DFr$BMFv2<-((DFr$BMF.x-DFr$BMFm)^2+(DFr$BMF.y-DFr$BMFm)^2)/82
sum(DFr$BMFv2)
mean(DFr$BMFv2)
sqrt(mean(DFr$BMFv2)) # repeatability
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
DFr$BMFm<-(DFr$BMF+DFr$BMF.y)/2
DFr$BMFv2<-((DFr$BMF-DFr$BMFm)^2+(DFr$BMF.y-DFr$BMFm)^2)/82
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

#JK may - June
DFr$HumanMeanm<-(DFr$HumanMean+DFr$HumanMean.y)/2
DFr$HumanMeanv2<-((DFr$HumanMean-DFr$HumanMeanm)^2+(DFr$HumanMean.y-DFr$HumanMeanm)^2)/82
sum(DFr$HumanMeanv2)
mean(DFr$HumanMeanv2)
sqrt(mean(DFr$HumanMeanv2))
(1.96*sqrt(2))*sqrt(mean(DFr$HumanMeanv2)) # better than Apr - May

?anova
anova()
modr<-lm()

#Then join by cow
#How many cows measured 5 times


#Within subject variation as measured by the methods
#Variance accross whole period 
#variance between 1 & 2nd, 2nd & 3rd, 3rd & 4th.
#April - august 5 with about 80 cows 
#Units - BMF different scale -use converted?
#Need data frame [,1=Cow,2=Date1,3BMF1,4FB1,5JK1,6=Date2,7BMF2,8FB2,9JK2,
# ]

#Then anova 


head(BMF)
#sort by cow and then date
BMF %>% arrange(desc(EARTAG),desc(DATE))
length(unique(BMF$EARTAG))

nextScore<- vector("numeric", nrow(BMF)-length(unique(BMF$EARTAG)))

nextScore #1,834 of 1945
som <- 0
somr<-0
somr
for(j in 1:(nrow(BMF)-1)) {
    somr <- c(somr,  (BMF[j,1]==BMF[j+1,1]))
} 

head(somr,37)
BMF<-cbind(BMF, somr)
matchr<- 0
for(j in 1:(nrow(BMF)+1)) {
  matchr <-  c(matchr,BMF[j-1,8])
} 
BMF<-cbind(BMF,matchr) # error matching

head(matchr)  

  nextscore[j] <- BMF[j+1,8]
  if(gap[j]>200){ 
    ######  
    #How to find the threshold so that the time between 2 steps is an 
    #inactive period and not the "normal" time?
    ######
    som <- som + gap[j]
  }
}
gap_mean <- som/j
gap_mean
gap_mean/10 #- 44 seconds