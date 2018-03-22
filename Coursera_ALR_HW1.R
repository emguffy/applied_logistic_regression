##################################################
# Coursera - Applied Logistic Regression
# Homework 1
##################################################

# rm(list=ls())
require(ggplot2)

# Exercise 1
myopia = read.csv("C:\\Users\\emguffy\\Documents\\R\\MYOPIA.CSV")
head(myopia)

# A
# Equation for the logistic regression model of SPHEQ on MYOPIC:
# MYOPIC = exp(b_0 + b_1*SPHEQ)/(1 + exp(b_0 + b_1*SPHEQ))

# Transformation of the logistic equation:
# log(MYOPIC/(1-MYOPIC)) = b_0 + b_1*SPHEQ
# logit = b_0 + b_1*SPHEQ

# MYOPIC is binary (takes values of either 1 or 0).  

# B
(myopia_plot <- ggplot(myopia, aes(x=SPHEQ,y=MYOPIC)) + geom_point())
  
# C
# This corresponds to the xi (squiggly-looking e) function within the lectures.
# Where the response equals 1 (y=1), the first part of the equation is active;
# Where the response equals 0 (y=0), the second part is active;
L <- function(b_0,b_1,y,x){
  prod(
    (exp(b_0 + b_1*x)/(1 + exp(b_0 + b_1*x)))**y *(1/(1 + exp(b_0 + b_1*x)))**(1-y)  
  )
}
logL <- function(b_0,b_1,y,x){
  sum(
    y*((b_0 + b_1*x) - log(1 + exp(b_0 + b_1*x))) + (1-y)*(-log(1 + exp(b_0 + b_1*x)))  
  )
}

# D

# D
summary(m1 <- glm(data=myopia,MYOPIC~SPHEQ,family="binomial"))

(m1_L <- L(m1$coef[1],m1$coef[2],myopia$MYOPIC,myopia$SPHEQ))
(m1_logL <- logL(m1$coef[1],m1$coef[2],myopia$MYOPIC,myopia$SPHEQ))

# We verify that our likelihood function from (C) is accurate by using the
# logLik() function as part of the stats package.
exp(logLik(m1)) 
m1_L

logLik(m1)
m1_logL

# Fitted model:
# # MYOPIC = exp(0.05397 - 3.83310*SPHEQ)/(1 + exp(0.05397 - 3.83310*SPHEQ))
m1_function <- function(SPHEQ){
  exp(m1$coef[1] + m1$coef[2]*SPHEQ)/(1 + exp(m1$coef[1] + m1$coef[2]*SPHEQ))
}

# We just tack on to the ggplot object from (B)
(myopia_plot <- myopia_plot  + stat_function(fun=m1_function,geom="line") + 
   scale_x_continuous(limits=c(-3,5)))



# Exercise 2
# rm(list=ls())
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV")
head(icu)

summary(icu$AGE)
summary(icu$STA)

# A
# Equation for the logistic regression model of STA on AGE:
# STA = exp(b_0 + b_1*AGE)/(1 + exp(b_0 + b_1*AGE))

# Transformation of the logistic equation:
# log(STA/(1-STA)) = b_0 + b_1*AGE
# logit = b_0 + b_1*AGE

# STA is binary (takes values of either 1 or 0).


# B
(icu_plot <- ggplot(icu, aes(x=AGE,y=STA)) + geom_point())

# C
# Using functions from Exercise 1
L <- function(b_0,b_1,y,x){
  prod(
    (exp(b_0 + b_1*x)/(1 + exp(b_0 + b_1*x)))**y *(1/(1 + exp(b_0 + b_1*x)))**(1-y)  
  )
}
logL <- function(b_0,b_1,y,x){
  sum(
    y*((b_0 + b_1*x) - log(1 + exp(b_0 + b_1*x))) + (1-y)*(-log(1 + exp(b_0 + b_1*x)))  
  )
}

# D
summary(m2 <- glm(data=icu,STA~AGE,family="binomial"))

(m2_L <- L(m2$coef[1],m2$coef[2],icu$STA,icu$AGE))
(m2_logL <- logL(m2$coef[1],m2$coef[2],icu$STA,icu$AGE))

# We verify that our likelihood function from (C) is accurate by using the
# logLik() function as part of the stats package.
exp(logLik(m2)) 
m2_L

logLik(m2)
m2_logL

# Fitted model:
# # MYOPIC = exp(-3.058513 + 0.02754261*AGE)/(1 + exp(-3.058513 + 0.02754261*AGE))
m2_function <- function(AGE){
  exp(m2$coef[1] + m2$coef[2]*AGE)/(1 + exp(m2$coef[1] + m2$coef[2]*AGE))
}

# We just tack on to the ggplot object from (B)
(icu_plot <- icu_plot + stat_function(fun=m2_function,geom="line") +
   scale_x_continuous(limits=c(0,200)))
