# Activities 9, 10, 15, 17, 20, 21, 22, 27, 34.
# Poisson log-linear regression uses function "glm", argument "family=poisson", and an "offset" function.

#A9
hist(rbinom(100000, 1138, 326/100000*25))
P=sum(dbinom(67:1138, 1138, 326/100000*25))
#or
P= 1- pbinom(66,1138,326/100000*25)
#or
P = sum(rbinom(100000, 1138, 326/100000*25)>67)/100000
# Probability to observe at least 67 cases of cancer is 0.9984989
Pvalue=sum(dbinom(68:1138, 1138, 326/100000*25))
# P value is 0.9978003


#redo 
n.sims <- 10000
n.people <- 1138
n.years <- 12.5
rate <- 326/100000
n.person.years <- n.people * n.years

cases <- rep(0, n.sims)
for (sim in 1:n.sims) {
  x = runif(n.person.years, 0, 1)
  cases[sim] <- sum(x < rate)
}
summary(cases)
hist(cases)

pvalue <- sum(cases >= 67) / n.sims

#A10
hist(rbinom(10000, 1138, 326/100000*12.5))
P12.5=sum(dbinom(67:1138, 1138, 326/100000*12.5))
# Probability to observe at least 67 cases of cancer is 0.002111118

Pvalue12.5=sum(dbinom(68:1138, 1138, 326/100000*12.5))
# or
#1-pbinom(67,1138,326/100000*12.5)

# P value is 0.001375957

#A15
setwd("/Users/SMC/学/ECE 608/09 - Jun 27 - Logistic and Poisson regression")
table <- read.csv("C8 CancerCluster.csv")
poissonreg.bga = glm(Cases ~ MedianAge + offset(log(Person.years)),data=table[1:4,], family = poisson)
#b0 = -8.66852; b1 = 0.04922  
#A17
poissonreg.ctr = glm(Cases ~ MedianAge + offset(log(Person.years)),data=table[5:8,], family = poisson)
#b0 = -9.07615; b1 = 0.07142

#The b1 of model for CTR data is greater than that of BGA data, the CTR model shows the logarithm of 
#cancer rates growing faster with age

#A20
poissonreg.all = glm(Cases ~ MedianAge + offset(log(Person.years)),data=table, family = poisson)
#b0 = -9.0760; b1 = 0.0713  
# The model make sense, as the coefficients with all data matchs closely to CTR, and BGA model. 
# It mathces more to the CTR which is expected due to the CTR data has more cases thatn BGA data. 

#A21
table1 <- cbind(Loc = c(0 ,0 ,0 ,0 ,1 ,1 ,1 ,1), table) # add dummy loc
# table[1:4,"location_dummy"] = 0
poissonreg.loc = glm(Cases ~ Loc  + offset(log(Person.years)),data=table1, family = poisson)
# Peopl in CTR location has 2.9(e^1.067) times cancer rate than people in BGA location

"Coefficients:
(Intercept)          Loc  
-6.235        1.067  "

#A22
poissonreg.loc.age = glm(Cases ~ Loc + MedianAge + offset(log(Person.years)),data=table1, family = poisson)
#e^b1=1.074, the cancer rate increases by 0.074 with each year of age increment.
#e^b2=2.475, the cancer rate for people in location CTR is 2.475 times to people in BGA.

"Coefficients:
(Intercept)          Loc    MedianAge  
-9.97305      0.90637      0.07127  "

#A27
poissonreg.loc.age.inter = glm(Cases ~ Loc + MedianAge + (Loc*MedianAge)+offset(log(Person.years)),data=table1, family = poisson)
"  (Intercept)            Loc      MedianAge  Loc:MedianAge  
     -8.66852       -0.40764        0.04922        0.02220  "
#A34
#a
##Wald’s tests
library("aod")
wald.test(b = coef(poissonreg.loc.age.inter), Sigma = vcov(poissonreg.loc.age.inter), Terms = 4)
##Chi-squared test:
#X2 = 6.4, df = 1, P(> X2) = 0.012 < 0.05, so we reject the null hypothesis, there is a significant relationship with
#interaction term.

##deviance statistic
"Null deviance: 11539.630  on 7  degrees of freedom
 Residual deviance:    40.453  on 4  degrees of freedom"
#40.453 indicating a good fitting with parameters as compared with
#model only include the intercept(grand mean)

##deviance residuals
resid(poissonreg.loc.age.inter)
'-2.4052   3.3458   0.7277  -1.9765  -2.3245   1.6425   2.8430  -1.6895 '
#The deviance of training sample 1, 5, 7 is greater than 2, which may indicating a poor-fitting model 

#b
poissonreg.loc.age.square = glm(Cases ~ Loc + MedianAge + (MedianAge*MedianAge)+offset(log(Person.years)),data=table1, family = poisson)
anova(poissonreg.loc.age.square,poissonreg.loc.age)
#The model with MedianAge^2 is no different than the model without it(G is zero), as shown in the LRT/ANOVA summary
#Thus age^2 should not be incorporated into the model.

