#In preparing for class on Wed Jun 13, I think the core reading in K&S Chapter 7 is 7.1-7.9.
#The concepts in logic regression will be covered in the Rossi chapter for next week.

#In K&S Chapter 7, please complete Activities 3, 5, 6, 7, 8, 9, 11, 12, 14.
#Logistic regression uses function "glm" and the argument "family=binomial".
#Submit using the file name "KS-Ch7.R".

# Activity 3
setwd("/Users/SMC/学/ECE 608/08 - Jun 20 - Multiple regression (KS logistic regression)")
table7.1 <- read.csv("Book1.csv")
attach(table7.1)
library(ggplot2)

fit <- lm(SuccesfulLaunch ~ AmbientTemp)

plot(SuccesfulLaunch ~ AmbientTemp )+
  abline(fit$coefficients, col="red")

predict.60 <- predict(fit, data.frame(AmbientTemp = 60)) #  0.3380952
predict.85 <- predict(fit, data.frame(AmbientTemp = 85)) #  1.272619 
# Activity 5
pi = function (x, b0, b1) {exp(b0+b1*x)/(1+exp(b0+b1*x)) }
# b0 = -10 and -5 and b1 = 0.5, 1, and 1.5
plot(pi(1:85, -10, 0.5), type = 'l', xlab="temp F", ylab="pi" , main= " b0 = -10, b1 = 0.5, 1, 1.5") + 
  lines(pi(1:85, -10, 1),col="green") +
  lines(pi(1:85, -10, 1.5), col="red")

plot(pi(1:85, -5, 0.5), type = 'l', xlab="temp F", ylab="pi", main= " b0 = -5, b1 = 0.5, 1, 1.5") + 
  lines(pi(1:85, -5, 1),col="green") +
  lines(pi(1:85, -5, 1.5), col="red")

#solution
par(mfrow=c(2,3))
Beta0 <- c(-10,-5)
Beta1 <- c(.5,1,1.5)
X <- seq(0,30,by=.1)
for (i in 1:2) {
  for (j in 1:3) {
    pi <- exp(Beta0[i] + Beta1[j]*X)/(1+exp(Beta0[i] + Beta1[j]*X))
    plot(X,pi,type="l",ylim=c(0,1),ylab = expression(pi))
    title(main=bquote(paste(beta[0]==.(Beta0[i]),"
                            ",beta[1]==.(Beta1[j]))),cex=.7)
  }
  }

# b0 = 10 and 5 and b1 = -0.5, -1, and -1.5.
plot(pi(1:85, 10, -0.5), type = 'l', xlab="temp F", ylab="pi" , main= " b0 = 10,  b1 = -0.5, -1, -1.5") + 
  lines(pi(1:85, 10, -1),col="green") +
  lines(pi(1:85, 10, -1.5), col="red")

plot(pi(1:85, 5, -0.5), type = 'l', xlab="temp F", ylab="pi", main= " b0 = 5,  b1 = -0.5, -1, -1.5") + 
  lines(pi(1:85, 5, -1),col="green") +
  lines(pi(1:85, 5, -1.5), col="red")


#ans: 
#a. the magnitude of b0 shapes the steepness of the curve, and 
#the sign of b0 inverts/complements the curve; b1 defines how long the transition is
#b. For all of these graphs, what value of p appears to have the steepest slope?
#pi = 0.5 
# Activity 6
fit2 <- glm(SuccesfulLaunch ~ AmbientTemp,family = binomial)
plot(SuccesfulLaunch ~ AmbientTemp) +
  abline(fit2, col="green") +
  lines(pi(1:85, -15.0429,  0.2322), col="red")
#MLE   b0=-15.0429      b1= 0.2322  
#LMS   b0=-1.90476      b1= 0.03738  
# The maximum likelihood estimate gives an s-shaped lot, result in better cutoff between 0 and 1

# Activity 7
p31=pi(31, -15.0429,  0.2322) # 0.0003916697
predict(fit2, data.frame(AmbientTemp = 31), type="response")
p50=pi(50, -15.0429,  0.2322) # 0.031282931.2613
p75=pi(75, -15.0429,  0.2322) # 0.9146749

# Activity 8
o60=pi(60, -15.0429,  0.2322)/(1-pi(60, -15.0429,  0.2322)) # 0.3292625
o70=pi(70, -15.0429,  0.2322)/(1-pi(70, -15.0429,  0.2322)) # 3.357176
# redo
p60 <- predict(fit2, data.frame(AmbientTemp = 60), type = "response")
a60 = p60/(1-p60)

p70 <- predict(fit2, data.frame(AmbientTemp = 70), type = "response")
a70 = p70/(1-p70)

# Activity 9
exp(0.2322*10)
# Activity 11
fit2 <- glm(SuccesfulLaunch ~ AmbientTemp,family = "binomial")
plot(SuccesfulLaunch ~ AmbientTemp, main="LMS in blue, MLE in RED",  xlim=c(0, 125))
  lines(pi(0:125, -1.90476190, 0.03738095), col="blue")  #LSE
  lines(pi(0:125, -15.0429,  0.2322), col="red", lty = 2) #MLE
  legend("topleft", # places a legend at the appropriate place 
         c("LSE","MLE"), # puts text in the legend
         lty=c(1,2), # gives the legend appropriate symbols (lines)
         lwd=c(1,1),
         col=c("blue","red"), 
         cex=0.5) 
  
  
# Activity 12
exp(0.2322*29) # 840.3345

exp(0.2322-1.96*0.1082)^29
exp(0.2322+1.96*0.1082)^29
# 95% confidence interval is (1.792681,393914)
# the odds of success at 60°F could be just slightly larger than the odds at 31°F
# or 393914 times as large as the odds at 31°F

# Activity 14
summary(fit2)
"Null deviance: 28.267  on 22  degrees of freedom
Residual deviance: 20.315  on 21  degrees of freedom"

# G-statistic = 28.267-20.315=7.952
"Coefficients:
            Estimate Std. Error z value Pr(>|z|)  
(Intercept) -15.0429     7.3786  -2.039   0.0415 *
AmbientTemp   0.2322     0.1082   2.145   0.0320 *"

# The p value of b1 is small, less than 0.05 so reject null model, and conclude 
# Ha, b1 not equal to 0, ambient temperature is related to the odd of a successful launch

# null model
# fit3 <- glm(SuccesfulLaunch ~ 1,family = binomial("logit")) #P 0.0681

                              