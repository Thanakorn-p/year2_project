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
set.seed(6505769)          # Change 123456 to be the project coordinator ID !!!
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

################Q1
SLR <- lm(Obs ~ AGE)
SLR$coef

#Q2
Z.x <- rstandard(SLR)
qqnorm(Z.x, main = "Q-Q Plot of the Standardized residuals")
lines(Z.x, Z.x, col = "red")

#Q3
NewData <- data.frame(AGE=50)
Predict <- predict(SLR, NewData, interval="prediction", level=.93)
Predict
exp(Predict[1])
exp(Predict[2])
exp(Predict[3])

#Q4
Sign(Z.x)

#Q5
AGE3 = AGE ^ 3
AGE5 = AGE ^ 5
MLR <- lm(Obs~AGE3+AGE5)
MLR$coef

#Q6 Q7
summary(SLR)
summary(MLR)

# Q8
plot(AGE, Obs,xlab = "Age",ylab = "log(mortality)", main = "Scatter plot between logarithmic mortality rate and age")
lines(AGE, SLR$fit,col = "blue")
lines(AGE, MLR$fit, col = "green" )
SLR$coef
MLR$coef

#Q9
summary(SLR)
summary(MLR)
#---------------------------------------------------------------------------------------

# Scenario 2

## Q1
MLR2 <- lm(Benefit ~ x1+x2, data=Future.Life)
summary(MLR2)
predict_x1 = 3500
predict_x2 = 5000
NewData2 <- data.frame(x1=predict_x1, x2=predict_x2)
Predict_val <- predict(MLR2, NewData2, interval="prediction")
Predict_val

#Q2
vif(MLR2,)
cor(Future.Life[,c(X1,X2)], use = "complete.obs")

#Q3
#a
SLR2 <- lm(Benefit ~ x3, data=Future.Life)
summary(SLR2)
SLR2$coef
#b
plot(x3,Benefit, xlab = "x3", ylab = "Benefit")
lines(x3,SLR2$coef[1]+SLR2$coef[2]*x3, col = "red")
summary(SLR2)
#c
#outlier
r_t <- rstudent(SLR2)
outlier_indices <- which(abs(r_t) > 2)
length(outlier_indices)
# Leverages
hatval <- hatvalues(SLR2)
average_leverage <- 2*(length(coef(SLR2)))/nrow(Future.Life)
high_leverage_indices <- which(hatval > average_leverage)
length(high_leverage_indices)
#Newmodel
indices_to_remove <- unique(c(outlier_indices, high_leverage_indices))
cleaned_data <- Future.Life[-indices_to_remove, ]
x3_cleaned <- x3[-indices_to_remove]
cleaned_data$x3 <- x3_cleaned
SLR2_cleaned <- lm(Benefit ~ x3, data=cleaned_data)
summary(SLR2_cleaned)
