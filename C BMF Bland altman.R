#Repeated measures Bland altman function
#https://stat.ethz.ch/pipermail/r-help/2008-July/166921.html
### Function Code

'Bland.Altman' <- function(x,y,alpha=.05,rep.meas=FALSE,
                           subject,...){
  #* Construct a Bland Altman Plot
  #* 1. Set a few constants 
  #* 2. Calculate mean difference
  #* 3. Calculate difference standard deviation 
  #* 4. Calculate upper and lower confidence limits  
  #* 5. Make Plot
  
#*** 1. Set a few constants
  z <- qnorm(1-alpha/2)  ## value of z corresponding to alpha
  d <- x-y               ## pair-wise differences
  m <- (x+y)/2           ## pair-wise means
  
#*** 2. Calculate mean difference
  d.mn <- mean(d,na.rm=TRUE)
  
#*** 3. Calculate difference standard deviation
  if(rep.meas==FALSE){ d.sd=sqrt(var(d,na.rm=TRUE)) }
  else{
    
    #*** 3a. Ensure subject is a factor variable
    if(!is.factor(subject)) subject <- as.factor(subject)
    
    #*** 3b. Extract model information
    n <- length(levels(subject))      # Number of subjects
    model <- aov(d~subject)           # One way analysis of variance
    MSB <- anova(model)[[3]][1]       # Degrees of Freedom
    MSW <- anova(model)[[3]][2]       # Sums of Squares
    
    #*** 3c. Calculate number of complete pairs for each subject
    pairs <- NULL
    for(i in 1:length(levels(as.factor(subject)))){
      pairs[i] <- sum(is.na(d[subject==levels(subject)[i]])==FALSE)
    }
    Sig.dl <- (MSB-MSW)/((sum(pairs)^2-sum(pairs^2))/
                           ((n-1)*sum(pairs)))
    d.sd <- sqrt(Sig.dl+MSW)
  }
  
#*** 4. Calculate lower and upper confidence limits
  ucl <- d.mn+z*d.sd # d.mn = mean, z = constant @ 0.05 alpha - z=1.96
  lcl <- d.mn-z*d.sd
  
#*** 5. Make Plot 
  plot(jitter(m), jitter(d),abline(h=c(d.mn,ucl,lcl)),  ...)
  values <- round(cbind(lcl,d.mn,ucl),4)
  colnames(values) <- c("LCL","Mean","UCL")
  if(rep.meas==FALSE) Output <- list(limits=values,Var=d.sd^2)
  else Output <- list(limits=values,Var=Sig.dl)
  return(Output)
#  return(lcl)
}
#####

mean(DF4$HumanMean-DF4$ModPred)
sd(DF4$HumanMean-DF4$ModPred)*2

par(mfrow=c(2,2))
hist(DF1$HumanMean,xlab = 'Body Condition Score 1-5',
     main = 'A. Frequency of Human Mean BCS scores',
     xlim = c(2.25,3.75))
Bland.Altman(DF1$MANUALBCS_FB, DF1$MANUALBCS_JK,rep.meas = T,
             subject = DF1$JUMBO,ylab="FB minus JK", 
             xlab="Human mean score", ylim=c(-0.7,0.5),xlim=c(2.2,3.5),
             main='C. Human assessor aggreement')

hist(DF4$ModPred,xlab = 'Body Condition Score 1-5',main = 'B. Frequency of BMF BCS scores',
     xlim = c(2.25,3.75))

Bland.Altman(DF4$HumanMean, DF4$ModPred,rep.meas = T,
             subject = DF4$JUMBO,ylab="Human mean minus BMF", 
             xlab="Mean of Human Mean and BMF scores", ylim=c(-0.7,0.5),xlim=c(2.2,3.5),
             main='D. BMF and Human Mean')
