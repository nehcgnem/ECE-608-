#In preparing for class on Wed Jul 11, I think the core reading in K&S Chapter 5 is 5.1 to 5.9.

#In K&S Chapter 5, please complete Activities 1, 2, 8, 15.

setwd("/Users/SMC/学/ECE 608/11 - Jul 11 - Analysis of variance")
table <- read.csv("C5 Memory.csv")

#Activity 1 
##a
#H10 Type of word list (abstract or concrete) doesn't impact memory.
#H1A Type of word list (abstract or concrete) does impact memory.
#H20 Type of distracter (mathematics or poetry) doesn't impact memory.
#H2A Type of distracter (mathematics or poetry) does impact memory.
#H30 There is no interaction between Wordlist and Distracter.
#H3A There is interaction between Wordlist and Distracter.
##b
summary(aov(formula = Score ~ Wordlist * Distracter, data=table))
"                    Df Sum Sq Mean Sq F value Pr(>F)  
Wordlist             1  22.69  22.687   6.409  0.015 *
Distracter           1   3.52   3.521   0.995  0.324  
Wordlist:Distracter  1   0.52   0.521   0.147  0.703  
Residuals           44 155.75   3.540   "
##c.
ggplot(table, aes(x = Wordlist, y = Score)) +
  geom_point() #individual value plot
ggplot(table, aes(x = Distracter, y = Score)) +
  geom_point() #individual value plot
boxplot(Score~Wordlist * Distracter ,table) #boxplot
#No, from the boxplot the box-widths in a boxplot they're mostly within a factor of ~2 of each other,
#so we can't question the equal variance assumption. 
m1<-aov(Score ~ Wordlist * Distracter, table)
hist(resid(m1))
qqnorm(m1$residuals)
qqline(m1$residuals)
summary(m1)
attach(table)
boxplot(Score ~ reorder(Wordlist:Distracter, Score, median), ylab="AvgScore (Recalled Words out of 20)")




#The residual is normally distributed, so is the error term. There is few outliers on the negative residuals.
##d.
"                    Df Sum Sq Mean Sq F value Pr(>F)  
Wordlist             1  22.69  22.687   6.409  0.015 *
Distracter           1   3.52   3.521   0.995  0.324  
Wordlist:Distracter  1   0.52   0.521   0.147  0.703  
Residuals           44 155.75   3.540   "
library(gplots)
plotmeans(Score ~ Wordlist,xlab="Wordlist",ylab="Score", p=.68, main="Main
 effect Plot-Wordlist",barcol="black", table) 
plotmeans(Score ~ Distracter,xlab="Distracter",ylab="Score", p=.68, main="Main
 effect Plot-Distracter",barcol="black", table) 
##Interaction plot
with(table, interaction.plot(x.factor = Wordlist, trace.factor = Distracter, response = Score))
#Conclusion: from the main effect plots, and P values, we can conclude the Wordlist is the main effect, 
# and we reject the null hypothesis that Type of word list (abstract or concrete) doesn't impact memory.


#redo
plot(c(1:2),seq(7,9,length=2),xaxt="n",type="n",xlab="Word List",
     ylab="Mean Number of Words Recalled")
axis(1,at=c(1:2),labels=c("Abstract","Concrete"))
points(c(1:2),tapply(Score, Wordlist, mean))
lines(c(1:2),tapply(Score, Wordlist, mean))


#Activity 2
##a. TBD double check ??
summary(aov(formula = Score ~ Wordlist * Distracter+factor(Student), data=table))
#H10 Type of word list (abstract or concrete) doesn't impact memory. P=0.015
#H1A Type of word list (abstract or concrete) does impact memory.
#H20 Type of distracter (mathematics or poetry) doesn't impact memory. P=0.324
#H2A Type of distracter (mathematics or poetry) does impact memory.
#H30 There is no interaction between Wordlist and Distracter. P=0.703
#H3A There is interaction between Wordlist and Distracter.
#H40 Student have equal performance. P=0.296 
#H4A Student have different performance. 
##b.
"                    Df Sum Sq Mean Sq F value Pr(>F)  
Wordlist             1  22.69  22.687   6.426  0.015 *
Distracter           1   3.52   3.521   0.997  0.324  
Student              1   3.94   3.944   1.117  0.296  
Wordlist:Distracter  1   0.52   0.521   0.148  0.703  
Residuals           43 151.81   3.530              "
#The P value for student is 0.296 > 0.05  indicates weak evidence against the null hypothesis, 
#so failed to reject the null hypothesis. Student variability shouldn't be the major cause of variability of data.
##c
plotmeans(Score ~ factor(Student),xlab="Student",ylab="Score", p=.95, main="Main
 effect Plot-Student",barcol="black", table)
plotmeans(Score ~ Wordlist,xlab="Wordlist",ylab="Score", p=.95, main="Main
 effect Plot-Wordlist",barcol="black", table)
plotmeans(Score ~ Distracter,xlab="Distracter",ylab="Score", p=.95, main="Main
 effect Plot-Distracter",barcol="black", table)
#A smaller P-value corresponds to a larger effect on the main effect plot.
m2<-aov(Score ~ Wordlist * Distracter+factor(Student), table)
hist(resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))
##?? We can see there are few outliers on the negative residuals, possibly caused by student #9, whom 
#has an overall lower score. Thus the conclusion should hold only for the selected 12 subjects, but not
#for entire population.

#Activity 8
library(lme4)
m3 = lmer(Score ~ Wordlist * Distracter+Major+(1|Student),data=table)
summary(aov(Score ~ Wordlist * Distracter+Major+(1|Student), data=table))

splitplot <- aov(Score ~ Major + Wordlist*Distracter + Error(as.factor(Student2)))
splitplot2 <- aov(Score ~ Major + Wordlist + Distracter+  Wordlist:Distracter + Major:Wordlist+ Major:Distracter +Error(Major/as.factor(Student2)))

"                    Df Sum Sq Mean Sq F value Pr(>F)  
Wordlist             1  22.69  22.687   6.363 0.0156 *
Distracter           1   3.52   3.521   0.987 0.3262  
Major                3   9.56   3.188   0.894 0.4524  
Wordlist:Distracter  1   0.52   0.521   0.146 0.7043  
Residuals           41 146.19   3.566        "

#H10 Type of word list (abstract or concrete) doesn't impact memory. 
#H1A Type of word list (abstract or concrete) does impact memory.
#H20 Type of distracter (mathematics or poetry) doesn't impact memory. 
#H2A Type of distracter (mathematics or poetry) does impact memory.
#H30 Major doesn't impact memory.
#H3A Major does impact memory.
#H40 There is no interaction between Wordlist and Distracter.
#H4A There is interaction between Wordlist and Distracter.
hist(resid(m3))
qqnorm(resid(m3))
#The residual is normally distributed, so is the error term. Reject only the first null hypothesis,
#and conclude that Type of word list (abstract or concrete) does impact memory.

#Activity 15
library(lmerTest)
m4 <- lmer(Score ~ Major+Wordlist+Distracter+Wordlist*Distracter+Major*Wordlist+Major*Distracter + (1 | Major/Student), data = table)
anova(m4)
"> anova(m4)
Type III Analysis of Variance Table with Satterthwaite's method
Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
Major                0.785  0.2618     3 17.011  0.2564   0.85569    
Wordlist            22.687 22.6875     1 27.000 22.2245 6.564e-05 ***
Distracter           3.521  3.5208     1 27.000  3.4490   0.07423 .  
Wordlist:Distracter  0.521  0.5208     1 27.000  0.5102   0.48118    
Major:Wordlist       8.063  2.6875     3 27.000  2.6327   0.07027 .  
Major:Distracter    44.896 14.9653     3 27.000 14.6599 7.370e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

