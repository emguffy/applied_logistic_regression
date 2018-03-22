##################################################
# Coursera - Applied Logistic Regression
# Homework 8
##################################################

# rm(list=ls())
require(ggplot2)

#### Exercise 1
icu = read.csv("C:\\Users\\emguffy\\Documents\\R\\ICU.CSV")
head(icu)

### Consider a multivariable model for the ICU study with the variables: LOC2, TYP,
### AGE, CAN, SYS, and AGExSYS. Assess the fit of the model.
summary(m1 <- glm(data=icu,formula=STA~factor(LOC)+TYP+AGE+CAN+SYS+AGE*SYS,family=binomial))
icu$LOC2 <- ifelse(icu$LOC>=1,1,0)
icu$obs <- 1
icu$iAGESYS<- icu$AGE * icu$SYS

head(icu)

summary(m2 <- glm(data=icu,formula=STA~LOC2+TYP+AGE+CAN+SYS+iAGESYS,family=binomial))
icu$p_m2 <- predict(m2,newdata=icu,type='response')

icu_1 <- with(icu,aggregate(x=icu,by=list(LOC2,TYP,AGE,CAN,SYS,iAGESYS),FUN=sum,na.rm=TRUE))
head(icu_1,10)

icu_1$p_resid <- (icu_1$STA - icu_1$p_m2)/(sqrt(icu_1$p_m2*(1-icu_1$p_m2/icu_1$obs)))
X <- list(X_square=sum(icu_1$p_resid**2))
X <- c(X,df=dim(icu_1)[1] - ((length(m2$coef) - 1) + 1))
# Theoretically, X_square is distributed as a chi-square variable with (J - (p+1))
# degrees of freedom.
X

# The chi-square statistic is not valid here, however, since the number of
# covariate patterns approaches n, the number of observations in the dataset.
# Therefore, we turn our attention to the Hosmer-Lemeshow test.
model <- m2
g <- 10
rm(model,g)
hosmerlem = function(model, g=10) {
  y <- model$y
  yhat <- model$fitted.values
  cutyhat <- 
    cut(yhat, breaks=quantile(yhat, probs=seq(0, 1, 1/g)), include.lowest=TRUE)
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}
hosmerlem(m2)



### Finding the area under the ROC:
## Sensitivity - Among the true 1's, what proportion did we predict as such.
# model <- m2
# threshhold <- .3
# rm(model,threshhold)
sensitivity <- function(model,threshhold) {
  y <- model$y
  yhat <- model$fitted.values
  sum(ifelse(y==1 & yhat >= threshhold,1,0))/sum(y)  
}
sensitivity(m2,.01)
# At lower threshholds, we are likely to predict a positive result. As we pick
# higher threshholds, the proportion that we predict as 1's decreases, as does
# the proportion of the actual 1's (the sensitivity). 


## Specificity - Among the true 0's, what proportion did we predict as such.
# model <- m2
# threshhold <- .3
# rm(model,threshhold)
specificity <- function(model,threshhold) {
  y <- model$y
  yhat <- model$fitted.values
  sum(ifelse(y==0 & yhat < threshhold,1,0))/(length(y) - sum(y))  
}
specificity(m2,.9)
specificity(m2,.1)


# model <- m2
# rm(d1,model,i,plot1)
plotSensificity <- function(model){
  # i <- 2
  d1 <- data.frame(threshhold=seq(0,1.00,.005),sensitivity=0,specificity=0)
  for(i in 1:(dim(d1)[1])) {
      d1$sensitivity[i] <- sensitivity(model,d1[i,1])
      d1$specificity[i] <- specificity(model,d1[i,1])      
  }
  print(d1)
  plot1 <- ggplot() + 
    geom_point(data=d1,aes(x=threshhold,y=sensitivity)) + 
    geom_point(data=d1,aes(x=threshhold,y=specificity)) + 
    xlab("threshhold") +
    ylab("Sensitivity/Specificity") +
    theme(legend.position="none")
  plot1
}
plotSensificity(m2)


# model <- m2
# rm(model,mergedData,auroc)
auroc <- function(model){
  mergedData <- merge(data.frame(y1=model$fitted.values[model$y==1]),data.frame(y0=model$fitted.values[model$y==0]))
  auroc <- sum(ifelse(mergedData$y1>mergedData$y0,1,0))/dim(mergedData)[1]
}


# model <- m2
# rm(model,mergedData,auroc)
rocCurve <- function(model) {
  ssdata <- data.frame(threshhold=seq(0,1.00,.005),sensitivity=0,specificity=0)
  for(i in 1:(dim(ssdata)[1])) {
    ssdata$sensitivity[i] <- sensitivity(model,ssdata[i,1])
    ssdata$specificity[i] <- specificity(model,ssdata[i,1])      
  }
  ssdata$recip_specificity <- 1 - ssdata$specificity
  
  plot2 <- ggplot(data=ssdata,aes(x=recip_specificity,y=sensitivity)) + 
    geom_point() + 
    geom_line() +
    xlab("1 - Specificity") +
    ylab("Sensitivity") +
    theme(legend.position="none") + 
    annotate(
      "text",
      x=.75,
      y=.2,
      label=paste("area under curve = ",auroc(model),sep="")
      )
  plot2
}
rocCurve(m2)


#### Exercise 2
# Removes non-functions from the global symbol table.
# rm(list = setdiff(ls(), lsf.str()))
hypo = read.csv("C:\\Users\\emguffy\\Documents\\R\\HYPONATREMIA.CSV")
hypo <- hypo[,c("nas135","female","urinat3p","wtdiff","bmi","runtime")]
hypo <- hypo[complete.cases(hypo),]
head(hypo)

### a)
### Fit a regression model with female, urinat3p, runtime, wtdiff and bmi. Evaluate
### the fit of the model using both Hosmer-Lemeshow test (using deciles of risk) and
### the Pearson Chi-square statistic. Assess whether the results of the two tests are
### consistent.

summary(m3 <- glm(data=hypo,formula=nas135~female+urinat3p+runtime+wtdiff+bmi,family=binomial))
hypo$predict_m3 <- predict(m3,newdata=hypo,type='response')

hypo$obs <- 1
hypo <- with(hypo,aggregate(x=hypo,by=list(female,urinat3p,runtime,wtdiff,bmi,predict_m3),FUN=sum,na.rm=TRUE))
hypo$man_pears_r <- (hypo$nas135 - hypo$predict_m3)/sqrt(hypo$obs*hypo$predict_m3*(1-hypo$predict_m3))
head(hypo)
chi_square <- sum(hypo$man_pears_r**2)
chi_square

# Theoretically, X_square is distributed as a chi-square variable with (J - (p+1))
# degrees of freedom.
df <- dim(hypo)[1] - ((length(m3$coef) - 1) + 1)
df

# model <- m5
# g <- 10
# rm(y,yhat,g,cutyhat,obs,expect,chisq,P)
hosmerlem = function(model, g=10) {
  cutyhat <- cut(model$fitted.values, breaks=unique(quantile(unique(model$fitted.values), probs=seq(0, 1, 1/g))), include.lowest=TRUE)
  obs = xtabs(cbind(1 - model$y, model$y) ~ cutyhat)
  expect = xtabs(cbind(1 - model$fitted.values, model$fitted.values) ~ cutyhat)
  chisq = sum((obs - expect)^2/expect)
  P = 1 - pchisq(chisq, g - 2)
  return(list(chisq=chisq,p.value=P))
}
hosmerlem(m3)


sum(residuals(m3,type='pearson')**2)

# No, these tests are not consistent.  The Pearson residuals test does not handle 
# data where the number of covariate patterns is substantial.

### b)
### On the basis of the logistic model with runtime, bmi, bmi2 and wtdiff as
### covariates, estimate the sensitivity and specificity of classifying subjects as
### having or not having hyponatremia using the cut-off values for the probability
### of hyponatremia of 0.5.
summary(m4 <- glm(data=hypo,formula=nas135~runtime+wtdiff+bmi+bmi**2,family=binomial) )
sensitivity(m4,.5)
specificity(m4,.5)

### c)
### Repeat the previous exercise using different cut-off values for the probability
### of hyponatremia. Draw by hand the ROC curve using these values of sensitivity and
### specificity.
rocCurve(m4)


### d)
### Use Stata (or your preferred statistical modeling software) to obtain the ROC
### curve. What is the discriminatory power of the model?
rocCurve(m4)


### e)
### Suppose someone had fraudulently access to your PC and altered data of the
### dependent variable in such a way that the coefficients of the model would remain
### the same. However, the predicted probabilities of the outcome would be largely
### affected. What would happen to goodness-of-fit statistics?
# Goodness-of-fit statistics will show a poorer fit, but the discrimination (ROC curve)
# will look the same.


### f)
### Fit a model with female and urinat3p as covariates. Assess the overall fit of the
### model and its discriminatory power by computing the Pearson Chi square goodness
### of fit statistic and the area under the ROC curve.
summary(m5 <- glm(data=hypo,formula=nas135~female+urinat3p,family=binomial))
m5_x2 <- data.frame(
    obs=as.vector(table(factor(m5$fitted.values))),
    yhat=as.numeric(levels(factor(m5$fitted.values))),
    y=table(factor(m5$fitted.values),m5$y)[,2]
    )
m5_x2
m5_x2$pearson_r <- (m5_x2$y - m5_x2$yhat*m5_x2$obs)/(m5_x2$obs*m5_x2$yhat*(1-m5_x2$yhat))**.5
m5_x2$pearson_r
x2 <- list(X_square=sum(m5_x2$pearson_r**2))
x2 <- c(x2,df=dim(m5_x2)[1] - ((length(m5$coef) - 1) + 1))
x2
hosmerlem(m5) # Will not work due to the finite number of covariate patterns (<10).


### g)
### Estimate the predicted probability of the outcome.
m5_x2$predicted <- m5_x2$yhat * m5_x2$obs
m5_x2

