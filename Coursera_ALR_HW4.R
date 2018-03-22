##################################################
# Coursera - Applied Logistic Regression
# Homework 4
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
hypo = read.csv("C:\\Users\\emguffy\\Documents\\R\\HYPONATREMIA.CSV")
head(hypo)

### a)
### Perform a logistic regression analysis using nas135 as dependent variable and
### female as the only independent variable.  Interpret the coefficients of the
### model.
summary(m1 <- glm(data=hypo,formula=nas135~female,family=binomial(link="logit")))
# The coefficient is positive, suggesting that females will have higher odds of
# having hyponatremia than will males.


### b)
### Fit a model with runtime as the only independent variable.  Interpret the
### coefficient for runtime.
summary(m2 <- glm(data=hypo,formula=nas135~runtime,family=binomial(link="logit")))
# The coefficient suggests that for every additional minute of runtime, the log odds
# of having hyponatremia will increase by the value of the coefficient (.0155).


### c)
### Calculate the odds ratio for the varaible runtime and interpret it.
runtime_odds <- function(runtime = 0){
  prob_hypo <- as.vector(exp(m2$coef[1]+m2$coef[2]*runtime)/(1 + exp(m2$coef[1]+m2$coef[2]*runtime)))
  prob_no_hypo <- as.vector(1 / (1 + exp(m2$coef[1]+m2$coef[2]*runtime)))
  return(prob_hypo/prob_no_hypo)
}
rt <- 205
runtime_odds_ratio <- runtime_odds(rt)/runtime_odds(rt-1)
runtime_odds_ratio
# Note that regardless of what rt value we select, the runtime_odd_ratio is the same.
# The odds ratio is exp(.0155), which is the coefficient of the runtime variable in
# our model m2.
round(log(runtime_odds_ratio),.001) == round(m2$coef[2],.001)


### d)
### Interpret the coefficient for the constant in the model with runtime as the only
### independent variable.  Does it make sense?  If not, what can you do to obtain a
### coefficient for the constant which is easily interpreted?
m2$coef[1]
# It is easier to interpret the coefficient by taking its exponentail.
exp(m2$coef[1])/(1 + exp(m2$coef[1]))
# This tells us that in the presence of an instantaneous marathon (runtime=0), the
# subject would have a probability of having hyponatremia of .0037.  Obviously, it
# is not possible to run a marathon in such a time.  As such, we could present the
# probability based on the average time taken to run the marathon.???  
# This would require recoding the runtime variable so that it represents the 
# difference from the average runtime rather than the raw number of minutes.


### e)
### Calculate the Odds Ratio of hyponatremia of a runner who takes 2 hours more
### than another runner, and the corresponding 95% Confidence Interval.
runtime_odds(320)/runtime_odds(200)
# We can also find this by the following:
exp(120*m2$coef[2])

# To find the 95% confidence interval, we must first find the variance/standard
# deviation for the second coefficient.
# The standard error of the runtime coefficient:
rt_se <- summary(m2)$coefficients[2,2]
# or
vcov(m2)[2,2]**.5

runtime_beta_ci <- as.vector(c(m2$coef[2]-1.96*rt_se,m2$coef[2]+1.96*rt_se))
# CI for the log odds:
odds_ci <- exp(120*runtime_beta_ci)
names(odds_ci) <- c("lower bound","higher bound")
odds_ci
# From the solutions: The odds of hyponatremia of a runner who takes 2 hours more 
# than another runner is about 6 times larger. This ratio can be as low as 3 times
# and as high as 13 times.


### f)
### Fit a model with female and runtime as independent variables.  Interpret both
### coefficients.
summary(m3 <- glm(data=hypo,formula=nas135~runtime+female,family=binomial(link="logit")))
# The odds of having hyponatremia are exp(.96) times higher if female than if male.
# The odds of having hyponatremia are exp(.014) times higher for each additional
# minute of runtime

# For example, the probability of a female with runtime of 250 is:
pf250 <- exp(m3$coef[1]+m3$coef[2]*250+m3$coef[3]*1)/(1+exp(m3$coef[1]+m3$coef[2]*250+m3$coef[3]*1))
pf250
# The odds are:
of250 <- pf250/(1-pf250)
of250

# The probability of a male with a runtime of 250 is:
pm250 <- exp(m3$coef[1]+m3$coef[2]*250+m3$coef[3]*0)/(1+exp(m3$coef[1]+m3$coef[2]*250+m3$coef[3]*0))
pm250
# The odds are:
om250 <- pm250/(1-pm250)
om250

# The odds ratio is:
of250/om250
exp(m3$coef[3])
round(of250/om250,.001) == round(exp(m3$coef[3]),.001)


### g)
### Compare the coefficients for female in the model with female as the only
### independent variable with that in the model that contains female and runtime.
### What is the percentage change in the coefficient of female?
summary(m1)
summary(m3)
m1$coef[2]
m3$coef[3]
# Percentage change:
(m3$coef[3]-m1$coef[2])/m1$coef[2]


### h)
### Calculate the Odds Ratio of hyponatremia for a female compared to a male who 
### completes the marathon in the same time.
summary(m3)
or_fm <- exp(m3$coef[3])
or_fm


### i) 
### What type of association do you expect between the variables female and runtime?
### Answer this question before looking at the data, only on the basis of the
### observed change in the coefficient for female when runtime is entered into the
### model.  Then make a box-plot of runtime by female.
# Females will tend to have longer runtimes than males, potentially acting as a
# confounding variable (one confounding the other, or vice-versa).
p3 <- ggplot(data=hypo,aes(x=factor(female),y=runtime)) + geom_boxplot()
p3


### j)
### Assess whether there is an interaction between female and runtime.
hypo$f_rt <- hypo$female*hypo$runtime
summary(m3_b <- glm(data=hypo,formula=nas135~runtime+female+f_rt,family=binomial(link="logit")))
# The coefficient is not significant, so we conclude that there is no interaction.


### k)
### Add to the model that contains female and runtime a dichotomous variable wgain 
### which takes the value of 0 if wtidff <= 0 and the value of 1 if wtidff > 0.
### Test for interaction between female and wgain.
hypo$wgain <- ifelse(hypo$wtdiff>0,1,0)
summary(m4 <- glm(data=hypo,formula=nas135~runtime+female+wgain,family=binomial(link="logit")))
summary(hypo$wtdiff)
hypo$f_wgain <- hypo$wgain*hypo$female
summary(m4_b <- glm(data=hypo,formula=nas135~runtime+female+wgain+f_wgain,family=binomial(link="logit")))
# Since the interaction term appears significant, we conclude that there is
# interaction.


### l)
### On the basis of the model with the interaction term, calculate the Odds Rations
### of hyponatremia for males who gain weight as compared to those who don't.
### Repeat this exercise for a female.  Interpret your findings.
bp4 <- ggplot(data=hypo,aes(x=factor(female),y=wgain)) + geom_boxplot()
m_wgain_or <- exp(m4_b$coef[4])
m_wgain_or

f_wgain_or <- exp(m4_b$coef[4]+m4_b$coef[5])
f_wgain_or

# For males, those who gain weight have a log odds that is roughly 11 times as high
# as the odd for males who don't gain weight.  For females, gaining weight has less
# of an impact on odds, with those gaining weight having a multiple of only 3.3
# over females that don't gain weight.


### m)
### Compare using the Likelihood Ratio test the model with female and runtime with
### a model with female, runtime, wgain, urinat3p, and bmi.  (Hint: the 2 models
### must be fitted on teh same set of observations.  Be aware of missing values in 
### some of these variables).  How many degrees of freedom does the test statistic
### have?
hypo_trimmed <- hypo[complete.cases(hypo),]
summary(m5 <- glm(data=hypo_trimmed,formula=nas135~runtime+female+wgain+urinat3p+bmi,family=binomial(link="logit")))
summary(m3_t <- glm(data=hypo_trimmed,formula=nas135~runtime+female,family=binomial(link="logit")))

L <- function(model){
  prod(predict(model,type='response')**model$y)*
    prod((1-predict(model,type='response'))**(1-model$y))
} 
log(L(m5))


D <- -2*(logLik(m3_t) - logLik(m5))
D
qchisq(.95,3)
D > qchisq(.95,3)
# Therefore we reject the null hypothesis that the additional variables do not add
# to the fit of the model.  At least one of the variables is significant.
