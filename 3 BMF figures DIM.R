library(ggplot2); library(gtable);library(gridExtra);

#Figure 2
#trim last few scores 
BMFt<-BMF %>% filter  (!JUMBO %in% trainBMFlist)
BMFt2<-BMFt %>% filter (!DIM>290 )
DF2b<-DF2%>% filter (!DIM.x>290)
DF3b<-DF3%>% filter (!DIM.x>290)

#FB
F2B <- ggplot(DF2b,aes(x=DIM.x,y=MANUALBCS_FB)) +
  ylim(2.2,3.7) + xlim(35,300)+
  geom_jitter(shape=1)+
    geom_smooth()+
  ylab("FB scores")+ 
  xlab("Days in milk")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
F2B

#JK plot
F2C<-ggplot(DF3b,aes(x=DIM.x,y=MANUALBCS_JK))+ylim(2.2,3.7)+
  geom_jitter(shape=1)+ xlim(35,300)+
 geom_smooth()+
  ylab("JK scores")+
  xlab("Days in milk")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
F2C

#Human Mean 
F2D<-ggplot(DF40,aes(x=DIM.x,y=HumanMean))+
  ylim(2.2,3.7)+ xlim(35,300)+
  geom_jitter(shape=1)+
    geom_smooth()+
  ylab("Mean JK & FB") +
  xlab("Days in milk") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
F2D  

#decision tree
BMFt2$rtreePred<-predict(fit,BMFt2)
F2A<-ggplot(BMFt2, aes(x=DIM, y=rtreePred))+
  ylim(2.2,3.7)+ xlim(35,300)+
  geom_jitter(shape=1)+ 
  geom_smooth()+
  ylab('BMF decision tree scores') +
  xlab("Days in milk")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
F2A 

# Decision tree


#BMF linear
BMFt2$modPred<-predict(cmod1,BMFt2)
F2mod<-ggplot(BMFt2, aes(x=DIM, y=modPred))+
  ylim(2.2,3.7)+xlim(35,300)+
   geom_jitter(shape=1)+  
  geom_smooth()+
  ylab('BMF linear conversion scores')+
  xlab("Days in milk")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
F2mod

#Faceted plot put them in a row -
grid.arrange(F2B,F2C,F2D,F2A,F2mod, nrow=2)

#ggsave(filename = 'Figure 3', device ='tiff', dpi = 300)

?geom_smooth
?lm

