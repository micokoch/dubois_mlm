library(haven)
Nurses <- read_sav("Y:/CHSC 594 Applied MLM/Nurses.sav")
View(Nurses)
nurse3level = lmer(stress ~ age  + gender  + experien + 
           )                  expcon  + wardtype  + hospsize   + (1 | hospital) + (1 | hospital:wardid), data=Nurses)
summary(nurse3level)
nurse3level2 = lmer(stress ~ age  + gender  + experien + 
                      expcon  + wardtype  + hospsize   + (1 | hospital) + (1 | wardid), data=Nurses)
summary(nurse3level2)
nurse3level3 = lmer(stress ~ age  + gender  + experien + 
                      expcon  + wardtype  + hospsize   + (1 | hospital) + (1 | ward), data=Nurses)
summary(nurse3level3)
step(nurse3level)
ICC(nurse3level)
ffRanefLMER.fnc(model=nurse3level,ran.effects=list(slopes=c("age", "gender", "experien"),
                                                   by.vars="wardid", corr=c(0,0,0,0,0)), 
                if.warn.not.add=FALSE, alpha=0.05)
#stop and add any random slope effects for L1 predictors
#at L2 based on results of above before looking at need for doing so at L3 for same predictors
ffRanefLMER.fnc(model=nurse3level,ran.effects=list(slopes=c("age", "gender", "experien"),
                                                   by.vars="hospital", corr=c(0,0,0,0,0)), 
                if.warn.not.add=FALSE, alpha=0.05)
#now after adding any further needed random slopes look at need for L2 predictors to have random slopes at L3
ffRanefLMER.fnc(model=nurse3level,ran.effects=list(slopes=c("expcon", "wardtype"),
                                                   by.vars=c("hospital"), corr=c(1,1)),
                if.warn.not.add=FALSE, alpha=0.05)


library(redres)
launch_redres(nurse3level)
nurse2level = lmer(stress ~ age  + gender  + experien + 
                expcon  + wardtype  + hospsize   +  + (1 | hospital), data=Nurses)
launch_redres(nurse2level)

data(tobvote)

library(LukeMLM)
library(lme4)
library(xtable)
library(ggplot2)

## Three-level Models from Luke text

  data(tobvote)
  # levels(tobvote$votedpro)
  # table(tobvote$votedpro)
  # tobvote$votedpro_bin <- as.numeric(tobvote$votedpro)-1
  gmod2 <- glmer(votedpro ~ party + pactotal + acres + (1|cmid) + (party|state), data=tobvote, family=binomial)
  summary(gmod2)

  
  # Fixes for later: put into ggplot and don't use hand-coded values
  x <- seq(0,113,.5)
  
  ydem_a0 <- plogis(-1.79 + x*.025)
  yrep_a0 <- plogis(-1.79 + 2.63 + x*.025)
  
  ydem_a10 <- plogis(-1.79 + x*.025 + 10*.006)
  yrep_a10 <- plogis(-1.79 + 2.63 + x*.025 + 10*.006)
  
  ydem_a100 <- plogis(-1.79 + x*.025 + 100*.006)
  yrep_a100 <- plogis(-1.79 + 2.63 + x*.025 + 100*.006)
  
  plot(x,ydem_a0,type="l",col="black",lty=1,lwd=2,
       xlim=c(0,125),ylim=c(0,1.1),
       ylab="Predicted protobacco voting probabilities",xlab="PAC contributions ($K)")
  
  points(x,ydem_a10,type="l",lty=2,col="black",lwd=2)
  points(x,ydem_a100,type="l",lty=3,col="black",lwd=2)
  
  points(x,yrep_a0,type="l",lty=1,col="gray40",lwd=1.5)
  points(x,yrep_a10,type="l",lty=2,col="gray40",lwd=1.5)
  points(x,yrep_a100,type="l",lty=3,col="gray40",lwd=1.5)
  
  legend(c(58,105),c(-.02,.40), 
         c("Republican - 100K Acres",
           "Republican - 10K Acres",
           "Republican - 0K Acres",
           "Democrat - 100K Acres",
           "Democrat - 10K Acres",
           "Democrat - 0K Acres"),
         col=c("gray40","gray40","gray40","black","black","black"),
         lty=c(3,2,1),lwd=c(1.5,1.5,1.5,2,2,2),cex=1.1,
         box.lty=0)
  
  