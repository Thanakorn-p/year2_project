#################################################################
#     R-code to generate data for 2022 Project.  This code MUST be
#     placed at the start of your own R-script.  You must edit
#     the argument to set.seed( ) function.
#############################SET SEED##############
rm(list = ls(all.names = TRUE))
###################################################

#     Read in CMI data and load R-funcions
#
#     NB: The files CMI_read.r, the CMI data files and R-function file
#         
##############################SET.SEED.#############
set.seed(6505586)          # Change 123456 to be the project coordinator ID !!!
####################################################
############## Do not modify this part below!!!! ########
library(MASS)
library('Rcmdr')
source("CMI_read.r")
YEAR <- sample(Year, 1); 
DTH <- Dth[Age > 36, Year == YEAR]
EXP <- Exp[Age > 36, Year == YEAR]
AGE <- Age[Age > 36]
Obs <- log(DTH/EXP)
Future.Life <- read.table("Future_Life_6_c.csv", header=TRUE, sep=",")
pool<-c("Factor1","Factor2","Factor3","Factor4","Factor5");pool
X1<-sample(pool,1)
pool<-pool[-which(pool==X1)]
X2<-sample(pool,1)
X3<-"Factor6"
x1<-Future.Life[,which(names(Future.Life)==X1)]
x2<-Future.Life[,which(names(Future.Life)==X2)]
x3<-Future.Life[,which(names(Future.Life)==X3)]
attach(Future.Life)

###################Functions
#
#     Sign test
#
#     Argument: Z = residuals Z.x
#
Sign <- function(Z){
  n <- length(Z)
  Greater <- sum(Z >= 0)
  Less <- sum(Z < 0)
  if(Greater == Less) return("Number + is equal to number -")
  if(Greater > Less){
    Sig.Prob <- 2 * (1 - pbinom(Greater - 1, n, 0.5))
    return(list(N.plus = Greater, N.minus = Less, Sig.Prob = Sig.Prob))
  }
  if(Greater < Less){
    Sig.Prob <- 2 * pbinom(Greater, n, 0.5)
    return(list(N.plus = Greater, N.minus = Less, Sig.Prob = Sig.Prob))
  }
}
#
#     ========================================================
#
#     Change of sign test
#
#     Argument: Z = residuals Z.x
#
Change.Sign <- function(Z){
  Change <- 0
  N1 <- length(Z) - 1
  for(i in 1:N1) {
    if(Z[i] * Z[i+1] < 0) Change <- Change + 1
  }
  Sig.Pr <- pbinom(Change, N1, 0.5)
  return(list(N = N1+1, Change = Change, Sig.Pr = Sig.Pr))
}
#
#     =====================================================
#
#     Runs test
#
#     Arguments: Z = residuals Z.x
#
#     This function calls the subsiduary function Runs( )
#
Runs.test <- function(Z){
  Code <- Z; Code[ Z <= 0] = -1; Code[ Z > 0] = 1
  n1 <- sum(Code == 1); n2 <- sum(Code == -1)
  g <- 0; if(Code[1] > 0) g <- 1
  for(i in 1:(length(Code)-1)) if(Code[i] < Code[i+1]) g <- g + 1
  Sig.Pr <- Runs(n1, n2, g)
  return(list(n1 = n1, n2 = n2, g = g, Sig.Prob = Sig.Pr))
}
#
#     Subsiduary function to calculate significance probability
#
Runs <- function(n1, n2, g){
  Sig.Pr <- 0
  Denom <- choose(n1+n2, n1)
  for(i in 1:g){
    Num <- choose(n1-1,i-1)*choose(n2+1,i)
    Sig.Pr <- Sig.Pr + Num/Denom
  }
  Sig.Pr
}
#
#     ================================================
#
#
############## Do not modify the above part !!!! ########
#####################################################
############start your code below ################
#####################################################/
#Note : 1. Use Obs as the response values for mortality rate in logarithmic scale
#       2. In scenario 2, use x1, x2, and/or x3 as your explanatory variables,
#                      use Benefit as your response variable
#       

# Scenario 1
#install.packages("MASS")
#install.packages("Rcmdr")
#install.packages("tidyverse")

## Q1
SLR <- lm(Obs ~ AGE)
#summary(SLR)
SLR$coef
#lm(y ~ x)


## Q2
Z.x <- rstandard(SLR)
qqnorm(Z.x, main = "Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
A <- -3:3 # The line is drawn from -3 to 3 
lines(A, A, col = "red")


## Q3
pre_age = 50
NewData <- data.frame(AGE=pre_age)
Predict <- predict(SLR, NewData, interval="prediction", level=.93)
Predict
exp(Predict[1])*100#point estimate of the prediction
exp(Predict[2])*100
exp(Predict[3])*100
#Predict[2] and Predict[3]: These typically represent 
#the lower and upper bounds of the prediction interval.

## Q4
Sign(Z.x)
#Runs.test(Z.x)

## Q5
AGE2 = AGE^5
AGE1 = AGE^3
MLR <- lm(Obs ~ AGE1+AGE2)
summary(MLR)
MLR$coef



## Q6
summary(SLR)
summary(MLR)

#Q7
summary(MLR)

## Q8
plot(AGE, Obs,ylab = "log(mortality)")
lines(AGE, SLR$coef[1]+SLR$coef[2]*AGE, col = "blue")
lines(AGE, MLR$coef[1]+MLR$coef[2]*AGE1+MLR$coef[3]*AGE2, col = "green" )
SLR$coef
MLR$coef



## Q9
Sign(Z.x)
Runs.test(Z.x)
Z.m <- rstandard(MLR)
Sign(Z.m)
Runs.test(Z.m)
summary(SLR)
summary(MLR)

#---------------------------------------------------------------------------------------

# Scenario 2

## Q1
MLR2 <- lm(Benefit ~ x1+x2, data=Future.Life)
summary(MLR2)
predc_x1 = 3500
predc_x2 = 5000
NewData2 <- data.frame(x1=predc_x1, x2=predc_x2)
model_Predict <- predict(MLR2, NewData2, interval="prediction")
model_Predict


## Q2
vif(MLR2)
cor(Future.Life[c(X1,X2)], use = "complete.obs")


## Q3
### a
SLR2 <- lm(Benefit ~ x3, data=Future.Life)
summary(SLR2)
SLR2$coef

### b
plot(x3,Benefit, xlab = "x3", ylab = "Benefit")
lines(x3,SLR2$coef[1]+SLR2$coef[2]*x3, col = "red")
summary(SLR2)
plot(SLR2)

### Outliers
T.SLR2 <- rstudent(SLR2)## Calculate Studentized residuals
# Plot the predicted values (y_hat) against the Studentized residuals.
plot(SLR2$coef[1]+SLR2$coef[2]*x3, T.SLR2, xlab = "y_hat", ylab = "Studentized Residual")
# Identify outliers as those observations where the Studentized residuals
# are greater than 2 or less than -2. 
OUTL.T <- T.SLR2[T.SLR2 > 2 | T.SLR2 < -2]# Count and report the number of outliers detected.
length(OUTL.T)
OUTL.T
### Leverages
hatval <- hatvalues(SLR2)# Calculate the leverage values 
plot(Benefit,hatval)
length(hatval)
HAT <- hatval[hatval > (2*(1+1)/length(hatval))] #2(k+1)/n = 0.014545
length(HAT)
HAT
### Exclude the leverage
library(tidyverse)
# Exclude the outlier and leverage observation from the 'Future.Life' dataset. 
Future.Life_1 <- Future.Life %>% slice(-38,-74,-138,-152,-161,-172,-229,-232,-241,-242,-249)
x3_A <- Future.Life_1[,which(names(Future.Life_1)==X3)]
model_A <- lm(Benefit ~ x3_A, data=Future.Life_1)
plot(model_A)
plot(x3_A,Future.Life_1$Benefit, xlab = "x3_A", ylab = "Benefit")
lines(x3_A,model_A$coef[1]+model_A$coef[2]*x3_A, col = "red")
summary(model_A)
model_A$coef

