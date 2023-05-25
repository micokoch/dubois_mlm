# lme4 vignette
# https://cran.r-project.org/web/packages/lme4/vignettes/lmer.pdf

library(lme4)

# Throughout our discussion of lme4, we will work with a data set on the average reaction time
# per day for subjects in a sleep deprivation study (Belenky et al. 2003). On day 0 the subjects
# had their normal amount of sleep. Starting that night they were restricted to 3 hours of sleep
# per night. The response variable, Reaction, represents average reaction times in milliseconds
# (ms) on a series of tests given each Day to each Subject (Figure 1)

str(sleepstudy)
summary(sleepstudy)

library(ggplot2)

lemur <- sleepstudy %>% 
  group_by(Subject)
lemur_lm <- lm(Reaction ~ Days, lemur)
summary(lemur_lm)

lemur_mlm <- lmer(Reaction ~ (1 + Days | Subject), lemur)
summary(lemur_mlm)

lemur_plot <- ggplot(lemur, 
                aes(x=Days, y=Reaction, group=Subject)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  facet_wrap( ~ Subject, ncol=9)
lemur_plot

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(fm1)

fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
summary(fm2)
