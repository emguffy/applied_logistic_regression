##################################################
# Coursera - Applied Logistic Regression
# Homework 4 - Updated Version
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV")
head(icu)

### a)
### Consider the ICU data and use as the outcome variable vital status (STA) and
### race (RACE) as a covariate. Prepare a table showing the coding of the two design
### variables for RACE using the value RACE = 1, white, as the reference group.
race_matrix <- matrix(c(0,1,0,0,0,1),nrow=3,dimnames=list(c("white","black","other"),c("RACE_dummy_1","RACE_dummy_2")))


### b)
### Show that the estimated log-odds ratios obtained from the cross-classification 
### of STA by RACE, using RACE = 1 as the reference group, are identical to
### estimated slope coefficients for the two design variables from the logistic
### regression of STA on RACE.
summary(m1 <- glm(data=icu,formula=STA~factor(RACE),family=binomial))
head(icu)
(sta_odds_white <- sum(ifelse(icu$RACE==1&icu$STA==1,1,0))/sum(ifelse(icu$RACE==1&icu$STA==0,1,0)))
(sta_odds_black <- sum(ifelse(icu$RACE==2&icu$STA==1,1,0))/sum(ifelse(icu$RACE==2&icu$STA==0,1,0)))
(sta_odds_other <- sum(ifelse(icu$RACE==3&icu$STA==1,1,0))/sum(ifelse(icu$RACE==3&icu$STA==0,1,0)))

as.vector(exp(m1$coef[2]))
(bw_or <- sta_odds_black/sta_odds_white)

as.vector(exp(m1$coef[3]))
(ow_or <- sta_odds_other/sta_odds_white)


### c)
### Verify that the estimated standard errors of the estimated slope coefficients
### for the two design variables for RACE are identical to the square root of the
### sum of the inverse of the cell frequencies from the cross-classification of STA
### by RACE used to calculate the odds ratio.
# Standard error of the black odds ratio:
(beta_b_se <- summary(m1)$coefficients[2,2])
sqrt(
  1/sum(ifelse(icu$RACE==2&icu$STA==1,1,0)) + 
  1/sum(ifelse(icu$RACE==2&icu$STA==0,1,0)) + 
  1/sum(ifelse(icu$RACE==1&icu$STA==1,1,0)) + 
  1/sum(ifelse(icu$RACE==1&icu$STA==0,1,0))
)

# Standard error of the other odds ratio:
(beta_o_se <- summary(m1)$coefficients[3,2])
sqrt(
  1/sum(ifelse(icu$RACE==3&icu$STA==1,1,0)) + 
  1/sum(ifelse(icu$RACE==3&icu$STA==0,1,0)) + 
  1/sum(ifelse(icu$RACE==1&icu$STA==1,1,0)) + 
  1/sum(ifelse(icu$RACE==1&icu$STA==0,1,0))
)


### d)
### Use either set of computations to compute the 95% Confidence Interval for the
### odds ratios.
bw_or_ci <- c(
  exp(log(bw_or)-1.96*beta_b_se),  
  bw_or,
  exp(log(bw_or)+1.96*beta_b_se)
  )
names(bw_or_ci) <- c("lower bound","mean","upper bound")
bw_or_ci

ow_or_ci <- c(
  exp(log(ow_or)-1.96*beta_o_se),  
  ow_or,
  exp(log(ow_or)+1.96*beta_o_se)
)
names(ow_or_ci) <- c("lower bound","mean","upper bound")
ow_or_ci



#### Exercise 2
### a)
### Create design variables for RACE using the method typically employed in ANOVA.
race_matrix <- matrix(c(-1,1,0,-1,0,1),nrow=3,dimnames=list(c("white","black","other"),c("RACE_dummy_1","RACE_dummy_2")))
race_matrix

icu$RACE_factor.b <- ifelse(icu$RACE == 1,-1,ifelse(icu$RACE==2,1,0))
icu$RACE_factor.o <- ifelse(icu$RACE == 1,-1,ifelse(icu$RACE==3,1,0))
head(icu)


### b)
### Perform the logistic regression of STA on RACE.
summary(m2 <- glm(data=icu,formula=STA~RACE_factor.b+RACE_factor.o,family=binomial))




### c)
### Show by calculation that the estimated logit differences of RACE = 2 versus RACE
### = 1 and RACE = 3 versus RACE = 1 are equivalent to the values of the log-odds
### ratio obtained in exercise 1.
logit_b <- m2$coef[1]+m2$coef[2]
logit_w <- m2$coef[1] + m2$coef[2]*-1 + m2$coef[3]*-1
# The logit difference is 2*beta_w + beta_o
(logit_diff_bw <- 2*m2$coef[2] + m2$coef[3])
(logit_diff_bw <- logit_b - logit_w)
m1$coef[2]


logit_o <- m2$coef[1]+m2$coef[3]
logit_w <- m2$coef[1] + m2$coef[2]*-1 + m2$coef[3]*-1
# The logit difference is 2*beta_0 + beta_w
(logit_diff_ow <- 2*m2$coef[3] + m2$coef[2])
(logit_diff_ow <- logit_o - logit_w)
m1$coef[3]


### d)
### Use the results of the logistic regression to obtain the 95% Confidence Interval
### for the odds ratios and verify that they are the same limits as obtained in
### exercise 1. Note that the estimated covariance matrix for the estimated
### coefficients is needed to obtain the estimated variances of the logit
### differences.
vcov(m2)
# The standard error is the square root of the variance of the logit difference,
# where the logit difference is 2*beta_b + beta_o and the variance of the logit
# difference is 4*var(beta_b) + var(beta_o) + 2*cov(beta_b,beta_o):

(logit_bw_se <- sqrt(4*vcov(m2)[2,2] + vcov(m2)[3,3] + 4*vcov(m2)[3,2]))
bw_or_ci_2 <- c(
  exp(logit_diff_bw-1.96*logit_bw_se),  
  exp(logit_diff_bw),
  exp(logit_diff_bw+1.96*logit_bw_se)
)
names(bw_or_ci_2) <- c("lower bound","mean","upper bound")
bw_or_ci_2
bw_or_ci

# The standard error is the square root of the variance of the logit difference,
# where the logit difference is 2*beta_o - beta_b:
logit_ow_se <- sqrt(4*vcov(m2)[3,3] + vcov(m2)[2,2] + 4*vcov(m2)[3,2])
ow_or_ci_2 <- c(
  exp(logit_diff_ow-1.96*logit_ow_se),  
  exp(logit_diff_ow),
  exp(logit_diff_ow+1.96*logit_ow_se)
)
names(ow_or_ci_2) <- c("lower bound","mean","upper bound")
ow_or_ci_2
ow_or_ci
