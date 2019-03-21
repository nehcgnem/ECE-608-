#In K&S Chapter 4, please complete Activities 5, 13, 15, 16, 17, 18.
setwd("/Users/SMC/学/ECE 608/10 - Jul 04 - Design of Experiments")

#Activity 5
table <- read.csv("C4 Popcorn.csv")
group1 <- table[table$Brand == "Fastco" & table$Time =='105 s',]
group2 <- table[table$Brand == "Fastco" & table$Time =='135 s',]
group3 <- table[table$Brand == "Pop Secret" & table$Time =='105 s',]
group4 <- table[table$Brand == "Pop Secret" & table$Time =='135 s',]

## Group 1 Summary Statistics
summary(group1)
"     Brand    Microwave    Time     X.Unpopped       PopRate     
Fastco    :8   Lounge:4   105 s:8   Min.   :11.50   Min.   :73.80  
Pop Secret:0   Room  :4   135 s:0   1st Qu.:15.46   1st Qu.:77.03  
                                    Median :18.60   Median :81.40  
                                    Mean   :18.87   Mean   :81.13  
                                    3rd Qu.:22.98   3rd Qu.:84.54  
                                    Max.   :26.20   Max.   :88.50  "
sd(group1$PopRate)
"5.398159"
sd(group1$X.Unpopped)
"5.398454"

## Group 2 Summary Statistics
summary(group2)
"      Brand    Microwave    Time     X.Unpopped       PopRate     
Fastco    :8   Lounge:4   105 s:0   Min.   : 7.40   Min.   :71.50  
Pop Secret:0   Room  :4   135 s:8   1st Qu.:14.26   1st Qu.:79.38  
                                    Median :17.65   Median :82.35  
                                    Mean   :17.62   Mean   :82.38  
                                    3rd Qu.:20.62   3rd Qu.:85.74  
                                    Max.   :28.50   Max.   :92.60  "
sd(group2$PopRate)
"7.226651"
sd(group2$X.Unpopped)
"7.226796"

## Group 3 Summary Statistics
summary(group3)
"       Brand    Microwave    Time     X.Unpopped       PopRate     
Fastco    :0   Lounge:4   105 s:8   Min.   :11.03   Min.   :65.98  
Pop Secret:8   Room  :4   135 s:0   1st Qu.:21.52   1st Qu.:72.02  
                                    Median :26.46   Median :73.54  
                                    Mean   :24.56   Mean   :75.44  
                                    3rd Qu.:27.98   3rd Qu.:78.48  
                                    Max.   :34.02   Max.   :88.97  "
sd(group3$PopRate)
"7.063566"
sd(group3$X.Unpopped)
"7.063806"


## Group 4 Summary Statistics
summary(group4)
"      Brand    Microwave    Time     X.Unpopped        PopRate     
Fastco    :0   Lounge:4   105 s:0   Min.   : 5.720   Min.   :78.71  
Pop Secret:8   Room  :4   135 s:8   1st Qu.: 9.227   1st Qu.:82.10  
                                    Median :13.085   Median :86.91  
                                    Mean   :13.576   Mean   :86.42  
                                    3rd Qu.:17.900   3rd Qu.:90.77  
                                    Max.   :21.290   Max.   :94.28  "
sd(group4$PopRate)
"5.866157"
sd(group4$X.Unpopped)
"5.867082"

## Fastco PopRate between 105s and 135s: -1.25%
## Pop Secret PopRate between 105s and 135s: -10.98%

#Activity 13
##MSBrand,MSTime, MSBrandTime
summary(aov(formula = PopRate ~ Time * Brand, data = table))

"           Df Sum Sq Mean Sq F value Pr(>F)  
Brand        1    5.4    5.42   0.131 0.7204  
Time         1  299.0  298.98   7.219 0.0120 *
Time:Brand   1  189.5  189.50   4.575 0.0413 *
Residuals   28 1159.7   41.42     "
##From the P value we can reject h02, h03, but failed to reject h01 which is no difference in the mean response(PopRate) between
#the two brands; the mean PopRate for the two Times are different; there is an interaction between Brand and Time

#Activity 15
library(ggplot2)
ggplot (table,aes (x = Time, y = PopRate, fill = Brand))  + facet_grid (. ~ Brand) + geom_point()
##a.From the plot there is no outlier, it is hard to tell wheter a data point is outlier with a such small sample size.
sd(group2$PopRate)/sd(group1$PopRate)
##b.The spread of each group is roughly the same,some shifts could result from small sample size.
# sd(group2$PopRate)/sd(group1$PopRate) = 1.338725 <2, thus no strong evidence against equal variance assumption.
attach(table)
m1 <- aov(PopRate ~ factor(Time) * Brand)
qqnorm(resid(m1))
qqline(m1$residuals)
hist(resid(m1))
##c.Yes, the residuals roughly follow a normal distribution with slight right-skewness as seen on the hist plot above
#and a pretty straight normal probability plot.
#Activity 16
ggplot (table,aes (x = Time, y = PopRate)) + facet_grid (Brand ~ Microwave) + geom_point() 
ggplot (table,aes (x = Time, y = PopRate)) + facet_grid (Microwave ~ Brand) + geom_point() 
##a.The pop rate is more affected by Time among all the groups.
#There isn't much significant difference between Lounge and Room microwaves.
#Pop Secret is more affect by Time as compared with Fastco.
##b. No, the spread within each group is quite different, mostly affect by the Time factor.
#as can be seen on the graph printed above, there is a visible jump in PopRate between Group2 and Group6,
#and between Group4 and Group8 where the explanatory variable Time has changed. Similarly, the same trend 
#can be observed for Fastco Brand as well, between Group1 and Group5 and between Group3 and Group7.

#Activity 17
group21 <- table[table$Brand == "Fastco" & table$Time =='105 s' & table$Microwave =='Lounge',]
group22 <- table[table$Brand == "Pop Secret" & table$Time =='105 s' & table$Microwave =='Lounge',]
group23 <- table[table$Brand == "Fastco" & table$Time =='105 s' & table$Microwave =='Room',]
group24 <- table[table$Brand == "Pop Secret" & table$Time =='105 s' & table$Microwave =='Room',]
group25 <- table[table$Brand == "Fastco" & table$Time =='135 s' & table$Microwave =='Lounge',]
group26 <- table[table$Brand == "Pop Secret" & table$Time =='135 s' & table$Microwave =='Lounge',]
group27 <- table[table$Brand == "Fastco" & table$Time =='135 s' & table$Microwave =='Room',]
group28 <- table[table$Brand == "Pop Secret" & table$Time =='135 s' & table$Microwave =='Room',]
sd1<-sd(group21$PopRate)
sd2<-sd(group22$PopRate)
sd3<-sd(group23$PopRate)
sd4<-sd(group24$PopRate)
sd5<-sd(group25$PopRate)
sd6<-sd(group26$PopRate)
sd7<-sd(group27$PopRate)
sd8<-sd(group28$PopRate)
sds<-data.frame(sd1,sd2,sd3,sd4,sd5,sd6,sd7,sd8)
Ratio<-max(sds)/min(sds)
## The ratio is 1.682247, thus no strong evidence against equal variance assumption.

xyplot(table$PopRate~table$Brand:factor(table$Time):table$Microwave)

#Activity 18
summary(aov(formula = PopRate ~ Brand * Time * Microwave, data = table))
##a.
"                     Df Sum Sq Mean Sq F value Pr(>F)  
Brand                 1    5.4    5.42   0.121 0.7308  
Time                  1  299.0  298.98   6.692 0.0162 *
Time:Brand            1  189.5  189.50   4.241 0.0505 .
Microwave             1   27.2   27.23   0.609 0.4426  
Brand:Microwave       1   20.2   20.23   0.453 0.5074  
Time:Microwave        1   10.3   10.29   0.230 0.6357  
Time:Brand:Microwave  1   29.6   29.63   0.663 0.4234  
Residuals            24 1072.3   44.68                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
attach(table)

m2 <- aov(PopRate ~ factor(Time) + Brand +  Microwave + factor(Time):Brand + factor(Time):Microwave + Brand:Microwave)
qqnorm(resid(m2))
hist(resid(m2))
##b.Yes, the residuals roughly follow a normal distribution with slight left-skewness as seen on the hist plot above
#as well as a pretty straight normal probability plot.
##c.From the P value we can reject all the null hypothesis other than h02 which is no difference in the mean response(PopRate) between
#the two Time. The conclusion: mean105 and mean135 are not equal.
##d. It is possible that this experiment is biased if the sample was not truly randomly drawn, in this 
#case the popcorn was not randomly purchased across the country due to budget confinement. The allocation 
#of samples could also bias the result if the samples are assigned nonrandomly (haphazardly), in this 
#experiment it was mentioned the samples are randomly allocated/assigned.
