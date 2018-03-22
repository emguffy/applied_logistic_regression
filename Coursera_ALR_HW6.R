##################################################
# Coursera - Applied Logistic Regression
# Homework 6
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV")
head(icu)


### a)
### Demonstrate that the value of the log-odds ratio obtained from the cross-
### classification of STA by CPR is identical to the estimated slope coefficient 
### from the logistic regression of STA on CPR. Verify that the estimated standard 
### error of the estimated slope coefficient for CPR obtained from the logistic
### regression package is identical to the square root of the sum of the inverse of
###  the cell frequencies from the cross-classification of STA by CPR. Use either
### set of computations to obtain 95% CI for the odds ratio. What aspect concerning
### the coding of the variable CPR makes the calculations for the two methods
### equivalent?

# Estimated slope coefficients:
summary(m1 <- glm(data=icu,formula=STA~CPR,family=binomial))

STA_CPR_t <- matrix(
  c(
    sum(ifelse(icu$CPR==1&icu$STA==1,1,0)),
    sum(ifelse(icu$CPR==1&icu$STA==0,1,0)),
    sum(ifelse(icu$CPR==0&icu$STA==1,1,0)),
    sum(ifelse(icu$CPR==0&icu$STA==0,1,0))
  ),
  nrow=2,
  dimnames=list(STA=c(1,0),CPR=c(1,0))
)
STA_CPR_t

STA_CPR_or <- (STA_CPR_t[1,1]*STA_CPR_t[2,2])/(STA_CPR_t[2,1]*STA_CPR_t[1,2])
STA_CPR_or

# The log of the odds ratio is equivalient to the coefficient from logistic
# regression. 
log(STA_CPR_or)
summary(m1)$coefficients[2,1]

# The standard error of the slope coefficient, calculated as the square root of the 
# reciprocal frequencies:
STA_CPR_se <- sqrt(1/STA_CPR_t[1,1]+1/STA_CPR_t[1,2]+1/STA_CPR_t[2,1]+1/STA_CPR_t[2,2])
STA_CPR_se
# ... as calculated by the logistic regression procedure:
summary(m1)$coefficients[2,2]


STA_CPR_ci <- c(
  exp(log(STA_CPR_or)-1.96*STA_CPR_se),
  STA_CPR_or,
  exp(log(STA_CPR_or)+1.96*STA_CPR_se)  
)
names(STA_CPR_ci) <- c("lower bound","mean","upper bound")
STA_CPR_ci
# Because CPR is dichotomous, the calculations become a matter of algebra.


### b)
### For purposes of illustration, use a data transformation statement to recode, for
### this problem only, the variable CPR as follows: 4 = no and 2 = yes. Perform the
### logistic regression of STA on CPR (recoded). Demonstrate how the calculation of
### the logit difference of CPR = yes versus CPR = no is equivalent to the value of
### the log-odds ratio obtained in exercise 1-a. Use the results from the logistic
### regression to obtain the 95% CI for the odds ratio and verify that they are the
### same limits as obtained in Exercise 1-a.
icu$CPR_r[icu$CPR==0] <- 4
icu$CPR_r[icu$CPR==1] <- 2
summary(m2 <- glm(data=icu,formula=STA~CPR_r,family=binomial))
CPR_y_logit <- m2$coef[1] + 2*m2$coef[2]
CPR_n_logit <- m2$coef[1] + 4*m2$coef[2]
CPR_logit_diff <- as.vector(CPR_y_logit - CPR_n_logit)
# Note that the logit difference is equivalent to -2 * m2$coef[2]
CPR_logit_diff
-2*m2$coef[2]

# These two are equivalent.  This demonstrates that no matter how the variables
# are coded, the odds ratio is unchanged.
CPR_logit_diff
m1$coef[2]

# This is the standard error of the CPR coefficient for model 2.
summary(m2)$coefficients[2,2]

# Since we need the standard error of the logit difference and the logit difference
# is -2*beta_cpr (of model 2), the standard error we're looking for is 
CPR_l_diff_se <- 2 * summary(m2)$coefficients[2,2]
CPR_l_diff_se
STA_CPR_ci_m2 <- c(
  exp(CPR_logit_diff-1.96*CPR_l_diff_se),
  exp(CPR_logit_diff),
  exp(CPR_logit_diff+1.96*CPR_l_diff_se)  
)
names(STA_CPR_ci_m2) <- c("lower bound","mean","upper bound")
STA_CPR_ci_m2
STA_CPR_ci


### c)
### Consider the ICU data and use as the outcome variable vital status (STA) and 
### race (RACE) as a covariate. Prepare a table showing the coding of the two design
### variables for RACE using the value RACE = 1, white, as the reference group. Show
### that the estimated log-odds ratios obtained from the cross-classification of STA
### by RACE, using RACE = 1 as the reference group, are identical to estimated slope
### coefficients for the two design variables from the logistic regression of STA on
### RACE. Verify that the estimated standard errors of the estimated slope
### coefficients for the two design variables for RACE are identical to the square
### root of the sum of the inverse of the cell frequencies from the cross-
### classification of STA by RACE used to calculate the odds ratio. Use either set
### of computations to compute the 95% CI for the odds ratios.

icu$RACE.f <- as.factor(icu$RACE)
icu$RACE.d1 <- ifelse(icu$RACE==2,1,0) 
icu$RACE.d2 <- ifelse(icu$RACE==3,1,0) 
race.table <- as.data.frame(rbind(c(0,0),c(1,0),c(0,1)))
colnames(race.table) <- c("RACE.d1","RACE.d2")
rownames(race.table) <- c("white = 1","black = 2","other = 3")
race.table

summary(m3 <- glm(data=icu,formula=STA~RACE.d1+RACE.d2,family=binomial))
(sta_odds_white <- sum(ifelse(icu$RACE==1&icu$STA==1,1,0))/sum(ifelse(icu$RACE==1&icu$STA==0,1,0)))
(sta_odds_black <- sum(ifelse(icu$RACE==2&icu$STA==1,1,0))/sum(ifelse(icu$RACE==2&icu$STA==0,1,0)))
(sta_odds_other <- sum(ifelse(icu$RACE==3&icu$STA==1,1,0))/sum(ifelse(icu$RACE==3&icu$STA==0,1,0)))

race_freq_table <- matrix(
  c(
    sum(ifelse(icu$RACE==1&icu$STA==1,1,0)),
    sum(ifelse(icu$RACE==1&icu$STA==0,1,0)),
    sum(ifelse(icu$RACE==2&icu$STA==1,1,0)),
    sum(ifelse(icu$RACE==2&icu$STA==0,1,0)),
    sum(ifelse(icu$RACE==3&icu$STA==1,1,0)),
    sum(ifelse(icu$RACE==3&icu$STA==0,1,0)) 
  ),
  nrow=2,
  dimnames=list(c("1","0"),c("White","Black","Other"))
)


(bw_or <- sta_odds_black/sta_odds_white)
exp(m3$coef[2])

(ow_or <- sta_odds_other/sta_odds_white)
exp(m3$coef[3])


# The standard errors:
(bw_se <- summary(m3)$coefficients[2,2])
sqrt(
    1/race_freq_table[1,1]+
    1/race_freq_table[2,1]+
    1/race_freq_table[1,2]+
    1/race_freq_table[2,2]
)

(ow_se <- summary(m3)$coefficients[3,2])
sqrt(
  1/race_freq_table[1,1]+
    1/race_freq_table[2,1]+
    1/race_freq_table[1,3]+
    1/race_freq_table[2,3]
)

# Confidence intervalse:
bw_or_ci <- c(
  exp(log(bw_or)-1.96*bw_se),
  bw_or,
  exp(log(bw_or)+1.96*bw_se)  
)
names(bw_or_ci) <- c("lower bound","mean","upper bound")
bw_or_ci

ow_or_ci <- c(
  exp(log(ow_or)-1.96*ow_se),
  ow_or,
  exp(log(ow_or)+1.96*ow_se)  
)
names(ow_or_ci) <- c("lower bound","mean","upper bound")
ow_or_ci


### d)
### Create design variables for RACE using the method typically employed in ANOVA.
### Perform the logistic regression of STA on RACE. Show by calculation that the
### estimated logit differences of RACE = 2 versus RACE = 1 and RACE = 3 versus 
### RACE = 1 are equivalent to the values of the log-odds ratio obtained in problem 1
### (c). Use the results of the logistic regression to obtain the 95% CI for the odds
### ratios and verify that they are the same limits as obtained in Exercise 1(c).
### Note that the estimated covariance matrix for the estimated coefficients is
### needed to obtain the estimated variances of the logit differences.
race_matrix <- matrix(c(-1,1,0,-1,0,1),nrow=3,dimnames=list(c("white","black","other"),c("RACE_dummy_1","RACE_dummy_2")))
race_matrix
icu$RACE.b.anova <- ifelse(icu$RACE==1,-1,ifelse(icu$RACE==2,1,0))
icu$RACE.o.anova <- ifelse(icu$RACE==1,-1,ifelse(icu$RACE==3,1,0))

summary(m4 <- glm(data=icu,formula=STA~RACE.b.anova+RACE.o.anova,family=binomial))
# black-white odds ratio
# bw odds ratio is: 2*beta_black + beta_other
bw_logit_diff <- (m4$coef[1] + m4$coef[2]) - (m4$coef[1] - m4$coef[2] - m4$coef[3])
bw_logit_diff

bw_log_odds <- log((sum(ifelse(icu$RACE.b.anova==1&icu$STA==1,1,0)) / 
                    sum(ifelse(icu$RACE.b.anova==1&icu$STA==0,1,0))) /
                   (sum(ifelse(icu$RACE.b.anova==-1&icu$STA==1,1,0)) / 
                    sum(ifelse(icu$RACE.b.anova==-1&icu$STA==0,1,0))))
bw_log_odds
exp(bw_log_odds)

# other-white odds ratio
# ow odds ratio is: 2*beta_other + beta_black
ow_logit_diff <- (m4$coef[1] + m4$coef[3]) - (m4$coef[1] - m4$coef[2] - m4$coef[3])
ow_logit_diff

ow_log_odds <- log((sum(ifelse(icu$RACE.o.anova==1&icu$STA==1,1,0)) / 
                      sum(ifelse(icu$RACE.o.anova==1&icu$STA==0,1,0))) /
                     (sum(ifelse(icu$RACE.o.anova==-1&icu$STA==1,1,0)) / 
                        sum(ifelse(icu$RACE.o.anova==-1&icu$STA==0,1,0))))
ow_log_odds
exp(ow_log_odds)

# Variance of the bw_log_or -> var(2*beta_black + beta_other)
# = 4*var(beta_black) + var(beta_other) + 2*2*cov(beta_black,beta_other)
bw_log_or_se <- sqrt(4 * vcov(m4)[2,2] + vcov(m4)[3,3] + 4 * vcov(m4)[3,2])
bw_log_or_se

bw_log_or_ci <- c(
  exp(bw_logit_diff-1.96*bw_log_or_se),
  exp(bw_logit_diff),
  exp(bw_logit_diff+1.96*bw_log_or_se)  
)
names(bw_log_or_ci) <- c("lower bound","mean","upper bound")
bw_log_or_ci
bw_or_ci
#

ow_log_or_se <- sqrt(4 * vcov(m4)[3,3] + vcov(m4)[2,2] + 4 * vcov(m4)[3,2])
ow_log_or_se

ow_log_or_ci <- c(
  exp(ow_logit_diff-1.96*ow_log_or_se),
  exp(ow_logit_diff),
  exp(ow_logit_diff+1.96*ow_log_or_se)  
)
names(ow_log_or_ci) <- c("lower bound","mean","upper bound")
ow_log_or_ci
ow_or_ci


### e)
### Consider the logistic regression of STA on CRN and AGE. Consider CRN to be the
### risk factor and show that AGE is a confounder of the association of CRN with STA.
### Addition of the interaction of AGE by CRN presents an interesting modeling
### dilemma. Examine the main effects only and interaction models graphically. Using
### the graphical results and any significance tests you feel are needed, select the
### best model (main effects or interaction) and justify your choice. Estimate
### relevant odds ratios. Repeat this analysis of confounding and interaction for a
### model that includes CPR as the risk factor and AGE as the potential confounding
### variable.
# CRN analysis
summary(m5 <- glm(data=icu,formula=STA~CRN+AGE,family=binomial))
summary(m5.reduced <- glm(data=icu,formula=STA~CRN,family=binomial))
m5$coef[2]
m5.reduced$coef[2]

# Since the change is greater than 15%, we conclude that there is confounding.
m5$coef[2]/m5.reduced$coef[2] - 1

summary(m5.interaction <- glm(data=icu,formula=STA~CRN+AGE+CRN*AGE,family=binomial))
ll_test <- as.vector(logLik(m5.interaction) - logLik(m5))
ifelse(ll_test > qchisq(.95,1),"REJECT NULL","DO NOT REJECT NULL")

# We should go with the non-interaction model: m5
summary(m5)

# CPR analysis
summary(m6 <- glm(data=icu,formula=STA~CPR+AGE,family=binomial))
summary(m6.reduced <- glm(data=icu,formula=STA~CPR,family=binomial))
m6$coef[2]
m6.reduced$coef[2]

# Since the change is less than 15%, we conclude that there is not confounding.
m6$coef[2]/m6.reduced$coef[2] - 1

summary(m6.interaction <- glm(data=icu,formula=STA~CPR+AGE+CPR*AGE,family=binomial))
ll_test <- as.vector(logLik(m6.interaction) - logLik(m6))
ll_test
ifelse(ll_test > qchisq(.95,1),"REJECT NULL","DO NOT REJECT NULL")

# We should go with the non-interaction model: m6
summary(m6)


### f)
### Consider an analysis for confounding and interaction for the model with STA as
### the outcome, CAN as the risk factor, and TYP as the potential confounding
### variable. Perform this analysis using logistic regression modeling and
### Mantel-Haenszel analysis. Compare the results of the two approaches.

# Logistic analysis:
summary(m7 <- glm(data=icu,formula=STA~CAN+TYP,family=binomial))
summary(m7.reduced <- glm(data=icu,formula=STA~CAN,family=binomial))
m7$coef[2]
m7.reduced$coef[2]

# Since the change is greater than 15%, we conclude that there is confounding.
m7$coef[2]/m7.reduced$coef[2] - 1

summary(m7.interaction <- glm(data=icu,formula=STA~CAN+TYP+CAN*TYP,family=binomial))
ll_test <- as.vector(logLik(m7.interaction) - logLik(m7))
ifelse(ll_test > qchisq(.95,1),"REJECT NULL","DO NOT REJECT NULL")

icu$TYP


MH <- (
  sum(ifelse(icu$TYP==1 & icu$STA==1&icu$CAN==1,1,0))*sum(ifelse(icu$TYP==1 & icu$STA==0&icu$CAN==0,1,0)) / 
    sum(ifelse(icu$TYP==1,1,0)) + 
  sum(ifelse(icu$TYP==0 & icu$STA==1&icu$CAN==1,1,0))*sum(ifelse(icu$TYP==0 & icu$STA==0&icu$CAN==0,1,0)) /
    sum(ifelse(icu$TYP==0,1,0))
  ) / (
  sum(ifelse(icu$TYP==1 & icu$STA==1&icu$CAN==0,1,0))*sum(ifelse(icu$TYP==1 & icu$STA==0&icu$CAN==1,1,0)) /
    sum(ifelse(icu$TYP==1,1,0)) + 
  sum(ifelse(icu$TYP==0 & icu$STA==1&icu$CAN==0,1,0))*sum(ifelse(icu$TYP==0 & icu$STA==0&icu$CAN==1,1,0)) / 
    sum(ifelse(icu$TYP==0,1,0))
  )
MH