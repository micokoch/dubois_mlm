library(tidyverse)
library(readxl)
# hdp <- read_excel("C:/Users/dldub/Downloads/hdp.xlsx")
hdp <- read.csv("hdp.csv")
View(hdp)
library(lme4)
library(lmerTest)
uncond0=lm(tumorsize ~ 1, data=hdp)
summary(uncond0)
uncond1=lmer(tumorsize ~ (1|DID), data=hdp)
summary(uncond1)
af=allFit(uncond1)
summary(af)
install.packages("reghelper")
library(reghelper)
ICC(uncond1)
comp01=anova(uncond1,uncond0)
comp01
l1pred=lmer(tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + RBC + (1|DID), data=hdp)
summary(l1pred)
ICC(l1pred)
install.packages("lmmlasso")
library(lmmlasso)
install.packages("glmmLasso")
library(glmmLasso)
library(dplyr)
# hdp=mutate(DID$hdp=factor(DID$hdp))
hdp <- hdp %>% mutate(DID = factor(DID))
mutate(hdp,DID=factor(DID))
lastest=glmmLasso(fix=tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + RBC ,  rnd=list(DID=~1), 
                  lambda=50, data=hdp)
summary(lastest)
install.packages("LMERConvenienceFunctions")
library(LMERConvenienceFunctions)
ffRanefLMER.fnc(model=l1pred,ran.effects=list(slopes=c("Sex", "Age", "FamilyHx", "LengthofStay"),
                                              by.vars="DID", corr=c(1,1,1,1)), 
                if.warn.not.add=FALSE, alpha=0.05)
l1pred2=lmer(tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + (1+LengthofStay|DID), data=hdp)
test=allFit(l1pred2)
summary(test)
l1pred2=lmer(tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + (1+LengthofStay|DID), data=hdp)
summary(l1pred2)
l2pred=lmer(tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + Experience + School + 
              Lawsuits + (1+LengthofStayc|DID), data=hdp)
l2predrsnol2=lmer(tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + 
              (1+LengthofStay|DID),REML=FALSE, data=hdp, control =lmerControl(optimizer = "bobyqa"))
summary(l2pred)
step(l2pred)
l2predrsnol2=lmer(tumorsize ~ Sex + Age+ FamilyHx + LengthofStay + (1+LengthofStay|DID),
                  REML=FALSE, data=hdp, control =
lmerControl(optimizer = "bobyqa"))
anova(l2predrs,l2predrsnol2)
library(redres)
shapiro.test(hdp$raw)
launch_redres(l2pred)
library(influence.ME)
infl <- influence(l2predrsnol2, obs = TRUE)
library(misty)
hdp$LengthofStaycgm <- center(hdp$LengthofStay, type = "CGM")
hdp$LengthofStaycwc <- center(hdp$LengthofStay, type = "CWC", cluster=hdp$DID)


## code for centering length of stay
hdp$LengthofStayc <- scale(hdp$LengthofStay, center=TRUE, scale=FALSE)


## Class #3
library(r2mlm)
l1pred2 <- lmer(tumorsize ~ (1|DID), hdp)
r2test <- r2mlm(l1pred2)

# Garth Rauscher code
library(misty)

hdpsub <- select(hdp, tumorsize, Age, LengthofStay, DID)
hdpsub$LengthofStaycgm <- center(hdpsub$LengthofStay,type="CGM")
hdpsub$LengthofStaycwc <- center(hdpsub$LengthofStay,type="CWC", cluster=hdpsub$DID)
hdpsub$Agecgm <- center(hdpsub$Age,type="CGM")
hdpsub$Agecwc <- center(hdpsub$Age,type="CWC", cluster=hdpsub$DID)

l1rsqcl2r=lmer(tumorsize ~ Agecwc+ LengthofStaycwc + (1+LengthofStaycwc|DID), 
               REML=FALSE, data=hdpsub, control=lmerControl(optimizer="bobyqa"))
summary(l1rsqcl2r)
r2mlm(l1rsqcl2r,bargraph = T)

# Jordan Barone code
library(haven)
safety <- read_sav("Safety_1.sav")

ggplot(safety, aes(x = crowded, y = binunsafe)) + 
  stat_sum(aes(size = ..n.., group = 1)) + 
  scale_size_area(max_size=10)

binary1 <- glmer(binunsafe~age10c+ sex + crowded + (1+age10c|street),family=binomial,data=safety)

# get confints and lod-odds, odds ratios!
se <- sqrt(diag(vcov(binary1)))
binary1ci <- cbind(Est = fixef(binary1), LL = fixef(binary1) - 1.96 * se, 
                   UL = fixef(binary1) + 1.96 * se)
binary1cior <- exp(binary1ci) 
summary(binary1cior)
binary1cior

# New David DuBois code
library(performance)
check_overdispersion(binary1)

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

overdisp_fun(binary1)
