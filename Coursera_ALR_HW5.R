##################################################
# Coursera - Applied Logistic Regression
# Homework 5
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
hypo = read.csv("C:\\Users\\emguffy\\Documents\\R\\HYPONATREMIA.CSV")
head(hypo)

### a)
### Assess the association between hyponatremia (dichotomous variable nas135) and
### sex (variable female) by making a 2 by 2 table. Calculate the odds ratio of
### hyponatremia of a female compared to a male. Compute the 95% confidence interval
### for this odds ratio. Interpret the findings.
hypoTable <- table(hypo$nas135,hypo$female,dnn=c("Hyponatremia","Gender"))
dimnames(hypoTable)[[1]] <- c("No","Yes")
dimnames(hypoTable)[[2]] <- c("Male","Female")
hypoTable
(hypoMFOdds <- (hypoTable[2,2]*hypoTable[1,1])/(hypoTable[1,2]*hypoTable[2,1]))
# or...
# (hypoMFOdds <- (37/129)/(25/297))
summary(m1 <- glm(data=hypo,formula=nas135~female,family=binomial))
f_se <- summary(m1)$coefficients[2,2]
female_oddsRatio_ci <- as.vector(c(exp(m1$coef[2]-1.96*f_se),exp(m1$coef[2]),exp(m1$coef[2]+1.96*f_se)))
names(female_oddsRatio_ci) <- c("lower bound","mean","higher bound")
female_oddsRatio_ci


### b) 
### Perform a logistic regression analysis with Stata using nas135 as dependent
### variable and female as the only independent variable. Use the Likelihood Ratio
### test to assess the significance of the model. Is the model with female a better
### model than the naïve model? If you are using Stata, you can use their built-in
### statistical functions to obtain p-values (type help functions).
summary(m1 <- glm(data=hypo,formula=nas135~female,family=binomial))
summary(m0 <- glm(data=hypo,formula=nas135~1,family=binomial))
G <- as.vector(-2 * (logLik(m0) - logLik(m1)))
G
c <- qchisq(.95,df=1)
(G < c)
# Since G is greater than the critical value, we reject the null hypothesis that
# the variable female is not informative.


### c)
### What is the naïve model? What is the probability of hyponatremia that this model
### predicts?
# m0 is the naive model.  It is the model based on the raw mean of hyponatremia.
summary(m0)
# The naive model predicts an unconditional probability of hyponatremia of:
prob_hypo <- as.vector(exp(m0$coef[1])/(1+exp(m0$coef[1])))
prob_hypo
# We can also calculate this by simply taking the mean of nas135:
mean(hypo$nas135)


### d)
### Run a logistic regression analyses with no independent variables. Transform the
### coefficient obtained from this model into a probability.
# Hmmmm, see above.  What was he expecting us to do in the prior step????


### e)
### Using the model with female as independent variable, compute the estimated
### probability of hyponatremia per males and females. Write down the equation for
### the logit.
f_hypo_logit <- function(gender){
  as.vector(m1$coef[1]+m1$coef[2]*gender)
}
f_hypo_logit(0)
f_hypo_logit(1)

p_hypo_female <- exp(f_hypo_logit(1))/(1 + exp(f_hypo_logit(1)))
p_hypo_female
p_hypo_male <- exp(f_hypo_logit(0))/(1 + exp(f_hypo_logit(0)))
p_hypo_male


### f)
### Use the Wald test to assess the significance of the coefficient for female.
W <- summary(m1)$coefficients[2,1]/summary(m1)$coefficients[2,2]
W
c <- qnorm(.95)
c
(W < c)
# Since W is greater than the critical value, we reject the null hypothesis that
# the true value of beta_female is 0.


### g)
### Fit a model with runtime as the only independent variable. Assess the
### significance of the model.
summary(m2 <- glm(data=hypo,formula=nas135~runtime,family=binomial))
# The variable runtime is significant on the basis of the Wald test.
# The runtime model also satisfies the likelihood ratio test.
G <- as.vector(-2 * (logLik(m0) - logLik(m2)))
G
c <- qchisq(.95,df=1)
(G < c)
# Since G is greater than the critical value, we reject the null hypothesis that
# the variable runtime is not informative.

### h)
### Calculate the probability of hyponatremia of a runner who takes 4 hours (240
### minutes) to complete the marathon.
predict(m2,newdata=data.frame(runtime=240),type="response")

### i)
### Fit a model with female and runtime as independent variables. Assess the
### significance of the model. Which null hypothesis is tested?
summary(m3 <- glm(data=hypo,formula=nas135~female+runtime,family=binomial))
G <- as.vector(-2 * (logLik(m0) - logLik(m3)))
G
(c <- qchisq(.95,df=2))
(G < c)
# The hypothesis being tested is that both runtime and female (considered together)
# are not significant.  This is seemingly false, as the G statistic, presumably
# distributed as a chi-square random variable with 2 degrees of freedom, is
# considerably larger than the critical value (c) of 5.99.  We therefore reject
# the null hypothesis.