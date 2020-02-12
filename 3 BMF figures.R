library(ggplot2); library(gtable);library(gridExtra)

#Figure 2
BMFt$rtreePred<-predict(fit,BMFt)
F2A<-ggplot(BMFt, aes(x=Date, y=rtreePred))+ylim(2.2,3.7)+
  #  scale_x_date(limits = c(as.Date(2016-03-1),as.Date(2016-12-13)))+
  geom_jitter(shape=1, position=position_jitter(2,0.03))+ geom_smooth()+
  ylab('BMF decision tree scores')+
  theme_bw()

#F2A
BMFt$modPred<-predict(cmod1,BMFt)
F2mod<-ggplot(BMFt, aes(x=Date, y=modPred))+ylim(2.2,3.7)+
  #  scale_x_date(limits = c(as.Date(2016-03-1),as.Date(2016-12-13)))+
  geom_jitter(shape=1, position=position_jitter(2,0.03))+ geom_smooth()+
  ylab('BMF linear conversion scores')+
  
  theme_bw()

#F2mod


F2B<-ggplot(DF2,aes(x=Date,y=MANUALBCS_FB)) +ylim(2.2,3.7)+
  # xlim(2016-03-15,2016-12-13) +
  geom_jitter(shape=1, position=position_jitter(2,0.03))+geom_smooth()+
  ylab("FB scores")+
    theme_bw()
F2B

F2C<-ggplot(DF3,aes(x=Date,y=MANUALBCS_JK))+ylim(2.2,3.7)+
  # xlim(2016-03-15,2016-12-13) +
  geom_jitter(shape=1, position=position_jitter(2,0.03))+geom_smooth()+
  ylab("JK scores")+
  theme_bw()
F2C

F2D<-ggplot(DF40,aes(x=Date,y=HumanMean))+ylim(2.2,3.7)+
  # xlim(2016-03-15,2016-12-13) +
  geom_jitter(shape=1, position=position_jitter(2,0.03))+geom_smooth()+
  ylab("JK scores")
#F2D

#Faceted plot put them in a row -
grid.arrange(F2B,F2C,F2A,F2mod, nrow=1 )



