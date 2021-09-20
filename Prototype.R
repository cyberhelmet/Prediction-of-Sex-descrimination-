library(Sleuth3)
library(MASS)
library(car)

bank <- read.csv("bank.csv", row.names=1)
head(bank)

#get an overview of the data
pairs(~Bsal+Senior+Age+Educ+Exper, data=bank, col=bank$Sex)

#simple linear fit of non-discriminatory terms to check out need for transformations
fit <- lm(Bsal ~ Senior + Age + Educ + Exper, data=bank)
summary(fit)
#this model explains 32% of the variation in the base salary

opar <- par(mfrow=c(2,2))
plot(fit)
par(opar)
#these don't look too bad. Hint of heteroscedacity?

boxcox(fit)
#suggests a log transformation. Although 1 (the do nothing option) is *just* outside the interval

fit2 <- lm(log(Bsal) ~ Senior + Age + Educ + Exper, data=bank)
summary(fit2)

opar <- par(mfrow=c(2,2))
plot(fit2)
par(opar)
#the diagnostics aren't much changed either
#would be happy if students went either way with this, i.e. log transform or no transform

avPlots(fit2, col=bank$Sex)
par(mfrow=c(2,2))
termplot(fit2, partial.resid = TRUE, smooth=panel.smooth)
#there is some hint that relationship between age and salary and exper and salary is non linear

fit3 <- lm(log(Bsal) ~ (Senior + Age + Educ + Exper)^2 + I(Senior^2) + I(Age^2) + I(Educ^2) + I(Exper^2), data=bank)
summary(fit3)
#this model is overfit but r^2 has improved quite a bit, so it is picking up some useful variation


opar <- par(mfrow=c(2,2))
plot(fit3)
par(opar)
#diagnostics are good

summary(influence.measures(fit3))
plot(rstudent(fit3))
par(mfrow=c(1,1))
plot(cooks.distance(fit3))
plot(hatvalues(fit3))
#there don't seem to be any overly influencial points to worry about

#time to cut back the model to just terms that seem to matter
fit4 <- stepAIC(fit3)

#drop (fit3)
#would be fine to drop1 here instead and to base decisions of p-values instead of AIC
#some students may opt for a bottom up approach instead

summary(fit4)
#note that AIC prefers to keep in some terms that aren't significant according to the t-tests

fit5 <- lm(log(Bsal) ~ Senior + Age + Educ + Age:Educ + Age:Exper, data=bank)
summary(fit5)
par(mfrow=c(2,2))
plot(fit5)

termplot(fit5, partial.resid=TRUE, smooth=panel.smooth)

#would be reasonable to drop the interaction term with senior if you wanted a model where all terms look significant
#also fine to stick with what AIC suggests

#now we're happy with out model for the non-discriminatory variables let's add sex into the model
fit5s <- lm(log(Bsal) ~ Sex + Senior + Age + Educ + Exper + Age:Educ + Age:Exper, data=bank)


summary(fit5s)
par(mfrow=c(2,2))
plot(fit5)
#R squared jumps up a lot
exp(confint(fit5s))
#so starting salaries for males were 1.08 to 1.18 times higher than females
#once other confounding variables were taken into account

avPlots(fit5s, "Sex")





#other thoughts

#if we don't log transform what's the additive effect of sex?
fit7 <- lm(Bsal ~ (Senior + Age + Educ + Exper)^2 + I(Senior^2) + I(Age^2) + I(Educ^2) + I(Exper^2), data=bank)
summary(fit7)
fit8 <- stepAIC(fit7)
summary(fit8)
fit8s <- lm(Bsal ~ Senior + Age + Educ + Exper + Senior:Educ + Age:Educ + Sex, data=bank)
summary(fit8s)
confint(fit8s) # starting salary $386 to $888 higher for males

#what is we start with higher order polynomial terms
fit.crazy <- lm(log(Bsal) ~ (Senior + Age + Educ + Exper)^3 + I(Senior ^2) + I(Age^2) + I(Educ^2) + I(Exper^2)+ I(Senior ^3) + I(Age^3) + I(Educ^3) + I(Exper^3), data=bank)
summary(fit.crazy)
fit.reduced <- stepAIC(fit.crazy)

fit.crazy <- lm(log(Bsal) ~ Senior + Age + Educ + Exper + I(Senior^2) + I(Age^2) +
                  I(Exper^2) + I(Senior^3) + I(Age^3) + Senior:Age + Senior:Educ +
                  Senior:Exper + Age:Educ + Age:Exper + Educ:Exper + Senior:Age:Educ +
                  Senior:Educ:Exper + Age:Educ:Exper + Sex, data=bank)
exp(confint(fit.crazy))
#so with a model that errs on the side of being overly complex,
#starting salaried for males were 1.05 to 1.16 times higher than females


#could go looking for interaction effects with sex
fit.int <- lm(log(Bsal) ~ (Sex + Senior + Age + Educ + Exper)^2, data=bank)
fit.int.reduced <- stepAIC(fit.int)
summary(fit.int.reduced)
#but it gets harder to answer the origional question of interest


par(mfrow=c(1,1))
interaction.plot(bank$Educ, bank$Sex, bank$Bsal)