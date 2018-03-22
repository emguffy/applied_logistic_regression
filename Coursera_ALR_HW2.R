##################################################
# Coursera - Applied Logistic Regression
# Homework 2
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
myopia = read.csv("C:\\Users\\emguffy\\Documents\\R\\MYOPIA.CSV")
head(myopia)

summary(m1 <- glm(data=myopia,MYOPIC~SPHEQ,family=binomial(link="logit")))

### Assess the significance of the slope coefficient for SPHEQ using the
### likelihood ratio test and Wald test;
## a) Likelihood ratio test:
# Likelihood of m1:
m1_L <- prod(
    (exp(m1$coef[1] + m1$coef[2]*m1$data$SPHEQ)/(1 + exp(m1$coef[1] + m1$coef[2]*m1$data$SPHEQ)))**m1$data$MYOPIC *
    (1/(1 + exp(m1$coef[1] + m1$coef[2]*m1$data$SPHEQ)))**(1-m1$data$MYOPIC)
  )
m1_L
# exp(logLik(m1))


# Likelihood of the naive model (where we use the mean of MYOPIC as the estimate):
mNaive_L <- prod(
    (mean(m1$data$MYOPIC)**(m1$data$MYOPIC))*((1-mean(m1$data$MYOPIC))**(1-m1$data$MYOPIC))
  )
mNaive_L

# Likelihood Ratio:
G1 <- -2*log(mNaive_L/m1_L)
G1
# or
# -2*(log(mNaive_L) - log(m1_L))

# This is well above the critical value at 1% of the Chi-square distribution
# at 1 d.f. of 6.625.  Therefore, we reject the null hypothesis that the 
# coefficient is truely 0.

## b) Wald test:
(W1 <- unname(m1$coef[2]/summary(m1)$coef[2,2]))
# Based on the value of W (assumed to have a standard normal distribution),
# we reject the null hypothesis that the true coefficient is 0.


### What assumptions are needed for the p-values computed for each of these
### tests to be valid?
# A: G is assumed to be distributed as a chi-square variable with 1 d.f.; 
# W is assumed to be distributed as a standard normal distribution.
# Are the results of these tests consistent with one another?

### What is the value of the deviance for the fitted model?
# The glm procedure calculates and saves the deviace as part of the object,
# but we can calculate the deviance on our own.
# The likelihood of the saturated model is 1, since a model with as many
# parameters as observations will perfectly describe the data, the 
# likelihood of which will be:
m1_Saturated_L <- prod(
  ((m1$data$MYOPIC)**(m1$data$MYOPIC)) * ((1 - (m1$data$MYOPIC))**(1 - m1$data$MYOPIC))   
  )
m1_Saturated_L

(m1_deviance <- -2*(log(m1_L)-log(m1_Saturated_L)))
# Checking to see if m1_deviance (our manual calculation) matches the 
# glm.fit method object:
m1_deviance == m1$deviance

  
#### Exercise 2
# rm(list=ls())
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV")
head(icu)

summary(m2 <- glm(data=icu,STA~AGE,family=binomial(link="logit")))

### Assess the significance of the slope coefficient for AGE using the
### likelihood ratio test and Wald test.
### likelihood ratio test and Wald test;
## a) Likelihood ratio test:
# Likelihood of m2:
(m2_L <- prod(
  (exp(m2$coef[1] + m2$coef[2]*m2$data$AGE)/(1 + exp(m2$coef[1] + m2$coef[2]*m2$data$AGE)))**m2$data$STA *
    (1/(1 + exp(m2$coef[1] + m2$coef[2]*m2$data$AGE)))**(1-m2$data$STA)
))
# exp(logLik(m2))


# Likelihood of the naive model (where we use the mean of STA as the estimate):
(mNaive_L <- prod(
  (mean(m2$data$STA)**(m2$data$STA))*((1-mean(m2$data$STA))**(1-m2$data$STA))
))

# Likelihood Ratio:
(G2 <- -2*log(mNaive_L/m2_L))
# or
# -2*(log(mNaive_L) - log(m2_L))

# This is above the critical value at 1% of the Chi-square distribution
# at 1 d.f. of 6.625.  Therefore, we reject the null hypothesis that the 
# coefficient is truely 0.

## b) Wald test:
(W2 <- unname(m2$coef[2]/summary(m2)$coef[2,2]))
# Based on the value of W (assumed to have a standard normal distribution),
# we reject the null hypothesis that the true coefficient is 0.

### What assumptions are needed for the p-values computed for each of these
### tests to be valid?
# A: G is assumed to be distributed as a chi-square variable with 1 d.f.; 
# W is assumed to be distributed as a standard normal distribution.
# Are the results of these tests consistent with one another?


### Are the results of these tests consistent with one another?
# A: Yes, for the most part.  But not as strong as with the myopic data.


### What is the value of the deviance of the fitted model?
# The glm procedure calculates and saves the deviace as part of the object,
# but we can calculate the deviance on our own.
# The likelihood of the saturated model is 1, since a model with as many
# parameters as observations will perfectly describe the data, the 
# likelihood of which will be:
mSaturated_L <- prod(
  ((m2$data$STA)**(m2$data$STA)) * ((1 - (m2$data$STA))**(1 - m2$data$STA))   
)
mSaturated_L

(m2_deviance <- -2*(log(m2_L)-log(mSaturated_L)))
# Checking to see if m1_deviance (our manual calculation) matches the 
# glm.fit method object:
m2_deviance == m2$deviance
