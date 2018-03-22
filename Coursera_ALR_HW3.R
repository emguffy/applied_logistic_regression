##################################################
# Coursera - Applied Logistic Regression
# Homework 3
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
myopia = read.csv("C:\\Users\\emguffy\\Documents\\R\\MYOPIA.CSV")
head(myopia)

summary(m1 <- glm(data=myopia,MYOPIC~SPHEQ,family=binomial(link="logit")))

### a)
## Using the results from Week Two, Exercise 1, compute the 95 percent confidence
## intervals for the slope coefficient SPHEQ.  Write a sentence interpreting this
## confidence.

SPHEQ_CI <- c(summary(m1)$coefficients[2,1]-1.96*summary(m1)$coefficients[2,2],
               summary(m1)$coefficients[2,1]+1.96*summary(m1)$coefficients[2,2])
names(SPHEQ_CI) <- c("lower bound","upper bound")
SPHEQ_CI

# If allowed to repeadely sample the population of data, the estimate for the 
# slope coefficient of SPHEQ will be within the CI 95% of the time.

## Use Stat to obrain the estimated covariance matrix.  then compute the logit
## and estimated logistic probablity for a subject with SPHEQ = 2.  Then evaluate
## the endpoints of the 95 percent confidence intervals for the logit and estimated
## logistic probablity.  Write a sentence interpreting the estimated probability
## and its confidence interval.
(m1_vcov <- vcov(m1))

m1CI <- function(SPHEQ){
  # First we find the variance of the logit using the variance-covariance table
  var_logit <- m1_vcov[1,1] + (SPHEQ**2)*m1_vcov[2,2] + 2*SPHEQ*m1_vcov[1,2]
  # Then we supplement this with the estimated value of the logit itself
  logit <- m1$coef[1] + m1$coef[2]*SPHEQ
  # Next, we find the confidence interval for the logit
  logit_CI <- c(logit-1.96*sqrt(var_logit),logit+1.96*sqrt(var_logit))
  names(logit_CI) <- c("lower bound","upper bound")  
  # Finally, we find the CI for the probabilities using the logit CI
  SPHEQ_CI <- c(exp(logit_CI[1])/(1+exp(logit_CI[1])),exp(logit_CI[2])/(1+exp(logit_CI[2])) )
  names(SPHEQ_CI) <- c("lower bound","upper bound")
  
  CI <- as.data.frame(rbind(logit_CI,SPHEQ_CI))
  colnames(CI) <- c("lower bound","upper bound")
  rownames(CI) <- c("logit CI","SPHEQ CI")
  return(CI)
}
m1CI(2)

# When an individual has a spherical value of 2, we are 95% confident that the
# true probability of having myopia is between .01% and .19%.


#### Exercise 2
# rm(list=ls())
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV")
head(icu)

summary(m2 <- glm(data=icu,STA~AGE,family=binomial(link="logit")))

### a)
## Using the results from Week Two, Exercise 2, compute the 95 percent confidence
## intervals for the slope and constant term.  Write a sentence interpreting the
## confidence interval for the slope.
# CI for constant term
m2_constant_CI <- c(m2$coef[1] - 1.96*summary(m2)$coefficients[1,2],
                    m2$coef[1] + 1.96*summary(m2)$coefficients[1,2])
names(m2_constant_CI) <- c("lower_bound","upper_bound")


m2_slope_CI <- c(m2$coef[2] - 1.96*summary(m2)$coefficients[2,2],
                 m2$coef[2] + 1.96*summary(m2)$coefficients[2,2])
names(m2_slope_CI) <- c("lower_bound","upper_bound")
# For each additional year of age, the log odds ratio increases between.0068 and
# .0482, with 95% confidence.

m2_vcov <- vcov(m2)

### b)
## Obtain the estimated covariance matrix for the model fit from Week One, 
## Exercise 2, Part (d). Then compute the logit and estimated logistic probability
## for a 60-year old subject. Then compute a 95 percent confidence intervals for the
## logit and estimated logistic probability. Write a sentence or two interpreting the
## estimated probability and its confidence interval.
logit_hat <- predict(m2,newdata=data.frame(AGE=60),type="link")
odds <- exp(logit_hat)/(1 + exp(logit_hat))

predict(m2,newdata=data.frame(AGE=60),type="response")


vcov(m2)


m2CI <- function(AGE){
  # First we find the variance of the logit using the variance-covariance table
  var_logit <- m2_vcov[1,1] + (AGE**2)*m2_vcov[2,2] + 2*AGE*m2_vcov[1,2]
  # Then we supplement this with the estimated value of the logit itself
  logit <- m2$coef[1] + m2$coef[2]*AGE
  # Next, we find the confidence interval for the logit
  logit_CI <- c(logit-1.96*sqrt(var_logit),logit+1.96*sqrt(var_logit))
  names(logit_CI) <- c("lower bound","upper bound")  
  # Finally, we find the CI for the probabilities using the logit CI
  AGE_CI <- c(exp(logit_CI[1])/(1+exp(logit_CI[1])),exp(logit_CI[2])/(1+exp(logit_CI[2])) )
  names(AGE_CI) <- c("lower bound","upper bound")
  
  CI <- as.data.frame(rbind(logit_CI,AGE_CI))
  colnames(CI) <- c("lower bound","upper bound")
  rownames(CI) <- c("logit CI","AGE CI")
  return(CI)
}
m2CI(60)

# We predict that an aged 60 subject has a 19.7% chance of being in vital status, 
# where 95% of repeated samples are estimated to be within 14.6% and 26.0%.





#### Exercise 3
# rm(list=ls())
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV",stringsAsFactors=FALSE)

head(icu)
str(icu)
### a)
## The variable RACE is coded at three levels.  Prepare a table showing the coding
## of the two design variables necessary for including this variable in a logistic
## regression model.
head(icu$RACE)
icu$RACE.f <- as.factor(icu$RACE)
icu$RACE.d1 <- ifelse(icu$RACE==2,1,0) 
icu$RACE.d2 <- ifelse(icu$RACE==3,1,0) 
race.table <- as.data.frame(rbind(c(0,0),c(1,0),c(0,1)))
colnames(race.table) <- c("RACE.d1","RACE.d2")
rownames(race.table) <- c("white = 1","black = 2","other = 3")
race.table

### b)
## Write down the equation for the logistic regression model of STA on AGE, CAN, 
## CPR, INF, and RACE.  Write down the equation for the logit transformation of this
## logistic regression model.  How many parameters does this model contain?
# Prob(STA==1) = exp(b_0 + b_1*AGE + b_2*CAN + b_3*CPR + b_4*INF + b_5*RACE.d1 + b_6*RACE.d2)/
#       (1 + exp(b_0 + b_1*AGE + b_2*CAN + b_3*CPR + b_4*INF + b_5*RACE.d1 + b_6*RACE.d2))
# Logit Transformation:
# log(Prob(STA==1)/(1 - Prob(STA==1))) = 
#           b_0 + b_1*AGE + b_2*CAN + b_3*CPR + b_4*INF + b_5*RACE.d1 + b_6*RACE.d2
# Number of parameters = 7 (if we include b_0)


### c)
## Write down an expression for the likelihood and log-likelihood for the logistic
## regression model in part (b).  How many likelihood equations are there?  Write
## down an expression for a typical likelihood equation for his problem.
# Likelihood = 
#   prod(
#   (exp(b_0+b_1*AGE+b_2*CAN+b_3*CPR+b_4*INF+b_5*RACE.d1+b_6*RACE.d2)/
#   (1+exp(b_0+b_1*AGE+b_2*CAN+b_3*CPR+b_4*INF+b_5*RACE.d1+b_6*RACE.d2)))**STA *
#   (1)/ (1+exp(b_0+b_1*AGE+b_2*CAN+b_3*CPR+b_4*INF+b_5*RACE.d1+b_6*RACE.d2)))**(1-STA)
#   )

# Log-likelihood = 
#   sum(
#   STA*(LOGIT - log(1 - exp(LOGIT))) + (1 - STA)*(1 - log(exp(LOGIT)))
#   )
#   Where the LOGIT is equal to:
#     b_0+b_1*AGE+b_2*CAN+b_3*CPR+b_4*INF+b_5*RACE.d1+b_6*RACE.d2

# When maximizing the log-likelihood or likelihood, we will be using a system of
# p+1 (7) likelihood equations.


### d)
## Using a logistic regression package, obtain the maximum likelihood estimates of
## the parameters of the logistic regression model in part (b).  Using these
## estimates, write down the equation for the fitted values, that is, the estimated
## logistic probabilities.
summary(m3 <- glm(data=icu,STA~AGE+CAN+CPR+INF+RACE.f,family=binomial(link="logit")))

LLRatio <- as.vector(logLik(m3) - logLik(glm(data=icu,STA~1,family=binomial(link="logit"))))
LLCritical <- as.vector(pchisq(LLRatio,7))

predict_m3 <- function(rn){
  logit <- m3$coef[1] +
      m3$coef[2]*icu$AGE[rn] +
      m3$coef[3]*icu$CAN[rn] +
      m3$coef[4]*icu$CPR[rn] +
      m3$coef[5]*icu$INF[rn] +
      m3$coef[6]*ifelse(icu$RACE[rn]==2,1,0) +
      m3$coef[7]*ifelse(icu$RACE[rn]==3,1,0)  
  prob <- exp(logit)/(1 + exp(logit))
  return(prob)
}
rowNum <- c(2,3)
predict_m3(rowNum)
predict(m3,type="response")[rowNum]

### e)
## Using the results of the output from the logistic regression package used in part
## (d), assess the significance of the slope coefficients for the variables in the
## model using the likeihood ratio test.  What assumptions are needed for the
## p-values computed for this test to be valid?  What is the value of the deviance
## for the fitted model?

# In order to conduct a likelihood ratio test for each variable, we need to compute
# the likelihood of the data/model with and without the each respective variable.
# The theory of the likelihood ratio test suggests that this ratio should be
# distributed as a chi-square variable with one degree of freedom.

# First, we formulate vector of variables:
varList <- names(m3$model)[-1]
varList

# Now, for each variable in the varList, we can draft a model with and without it.
# To do so, we create a formula object.
logLikelihoodRatios <- NULL
for (j in 1:length(varList)){
  m3LL <- as.formula(paste("STA~",paste(varList[-j], collapse= "+")))
  m3.reduced <- glm(data=icu,formula=m3LL,family=binomial(link="logit"))
  LLRatio <- as.vector(-2*(logLik(m3.reduced)-logLik(m3)))
  logLikelihoodRatios[j] <- LLRatio
}
criticalValue <- qchisq(.95,df=1)
LLRatioTest <- logLikelihoodRatios > criticalValue
names(LLRatioTest) <- varList


### f) 
## Use the Wald statistics to obtain an approximation to the significance of the
## individual slope coefficeints for the the variables in the model.  Fit a reduced
## model that eliminates those variables with non-significant Wald statistics.  Assess
## the joint (conditional) significance of the variables excluded from the model.
## Present the results of fitting the reduced model in a table.
summary(m3)
W <- m3$coef/(diag(summary(m3)$cov.unscaled)**.5)
# Investigating the summary.glm() method, they test the abs of "tValue"
# varKeep <- 2 * pnorm(-abs(z)) < .05
varKeep <- pnorm(abs(W)) > .95

m3.reduced.formula <- as.formula(paste("STA~",paste(names(varKeep)[varKeep][-1], collapse= "+")))
m3.reduced <- glm(data=icu,formula=m3.reduced.formula,family=binomial(link="logit"))
summary(m3.reduced)
LLRatio <- as.vector(-2*(logLik(m3.reduced)-logLik(m3)))
criticalValue <- qchisq(.95,df=1)
LLRatioTest <- LLRatio > criticalValue
LLRatioTest
# Removing the variables does NOT have a proportionally damaging impact on the model.
# Therefore, remove the variables (a simpler model is better)

### g)
## Using the results form (f), compute 95 percent confidence intervals for the 
## coefficients in the model.  Write a sentance interpreting the confidence
## intervals for the non-constant covariates.
str(m3.reduced)
str(summary(m3.reduced))
m3.reduced.ci <- rbind(
  m3.reduced$coef - 1.96*summary(m3.reduced)$coefficients[,2],
  m3.reduced$coef,
  m3.reduced$coef + 1.96*summary(m3.reduced)$coefficients[,2]
)
rownames(m3.reduced.ci) <- c("lower bound","coefficient","upper bound")
colnames(m3.reduced.ci) <- names(m3.reduced$coef)
m3.reduced.ci
# The INF variable is concerning since the confidence interval contains 0.
# Otherwise, the variables are good.

