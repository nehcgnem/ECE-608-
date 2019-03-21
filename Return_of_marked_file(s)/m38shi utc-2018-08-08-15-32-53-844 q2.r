#1.	Why were the subjects randomly assigned to the two chemotherapy options?

" Since this experiment it is not possible to do a repeated measure for each subject, as chemotherapy 
may have a high fatal rate. So blocking design wouldn't work for this situation, and we have to use
completely randomized design so each unit (i.e., each test) would be randomly assigned to only one chemo 
treatment. It is expected to have some subject to subject variation, so we need to control any extraneous 
factors. By randomly assigning the experimental units to the treatment groups(standard, test), 
the effects of any extraneous factors tend to average out over the treatment group making the groups as 
similar as possible before the treatments are applied to the units.
"
setwd("/Users/SMC/Downloads/Final Exam")
chemo.data<-read.csv("q2/lung.csv")

#2.	Write R code to plot Kaplan-Meier curves which compare the survival times of the two 
# chemotherapy treatment groups (standard, test).

#add new column event IsNotCensored
for(row in 1:nrow(chemo.data)) {
  if(chemo.data[row,"IsCensored"] == "yes")
    chemo.data[row,"IsNotCensored"] = 0
  else
    chemo.data[row,"IsNotCensored"] = 1
}

KM.model <<- survfit( Surv( time = SurvivalDays, event = IsNotCensored ) ~ ChemoType, data = chemo.data )
summary(KM.model)

plot(KM.model,lty=c(1,1),col=c("red","blue"),xlab="Time(days)",ylab="Survival Probability",ylim=c(0,1),conf.int=FALSE, main="Kaplan-Meier curves versus ChemoType")
abline(h=0.5, col="black")
#abline(v=55)
legend("topright", # places a legend at the appropriate place 
       c("standard","test"), # puts text in the legend
       lty=c(1, 1), # gives the legend appropriate symbols (lines)
       lwd=c(1,1), # width
       col=c("red","blue"), 
       cex=0.5)  # scale

#3.	Write R code to find the log-rank test statistic and its p-value for the situation in part 1.
survdiff(formula = Surv(SurvivalDays, IsNotCensored) ~ ChemoType, data = chemo.data)

"survdiff(formula = Surv(SurvivalDays, IsNotCensored) ~ ChemoType, 
    data = chemo.data)

N Observed Expected (O-E)^2/E (O-E)^2/V
ChemoType=standard 69       64     64.5   0.00388   0.00823
ChemoType=test     68       64     63.5   0.00394   0.00823

Chisq= 0  on 1 degrees of freedom, p= 0.928 "

#4.	Interpret the results from parts 2 and 3.
"From part 2, we can see that  
 Standard Chemo's survival function crosses 0.5 at around 100 days, 
 so its median survival time is roughly 100 days.
 Test Chemo's survival function crosses 0.5 at around 50 days,
 so its median survival time is more than 50 days.
 
 From part 3,
 The p -value corresponding to the value of x2  is 0.928
 Hence, we do not have strong enough evidence to conclude 
 that the survival times of the populations
 of Standard and Test chemotherapy significantly differ."

#5. Patients with a Karnofsky percent above 60 can be considered healthier than patients with a
# Karnofsky percent of 60 or below. Using this division, write R code to plot Kaplan-Meier curves which
# compare the survival times of the four groups determined by combinations of the chemotherapy treatments and
# the high and low Karnofsky percent ranges.

for(row in 1:nrow(chemo.data)) {
  if(chemo.data[row,"KarnofskyPercent"] > 60)
    chemo.data[row,"KP"] = 1
  else
    chemo.data[row,"KP"] = 0
}



KM.model2 <<- survfit( Surv( time = SurvivalDays, event = IsNotCensored ) ~ ChemoType + KP , data = chemo.data )
summary(KM.model2)
plot(KM.model2, col = c("red", "red", "blue", "blue"), lty=c(2, 1, 2, 1), main="Kaplan-Meier curves versus Four Groups")
abline(h=0.5, col="black")
#abline(v=55)
legend("topright", # places a legend at the appropriate place 
       c("standard, KP<=60","standard, KP>60","test, KP<=60","test, KP>60"),  
       lty=c(2,1,2,1), # gives the legend appropriate symbols (lines)
       lwd=c(1,1,1,1), # width
       col=c("red", "red", "blue", "blue"), 
       cex=0.5)  # scale

#6 
survdiff(formula = Surv(SurvivalDays, IsNotCensored) ~ ChemoType +KP, data = chemo.data)
"                          N Observed Expected (O-E)^2/E (O-E)^2/V
ChemoType=standard, KP=0 40       38     31.1      1.54      2.09
ChemoType=standard, KP=1 29       26     33.4      1.65      2.30
ChemoType=test, KP=0     39       38     20.6     14.80     18.65
ChemoType=test, KP=1     29       26     42.9      6.68     10.87

Chisq= 26.4  on 3 degrees of freedom, p= 7.8e-06 "

#7 
"From part 5, we can see that the median survival time is generaly higher for those people are considered as health 
 than those are not within each treatment groups. 
 The order of median survial time (highest to lowest) is (Test, KP>60)> (Standard, KP>60) > (Standard, KP<=60) > (Test, KP<=60). 
 A conclusion could be drawn that the test chemo is beneficial for healthy subjects, and standard test is more beneficial for
 unhealty subjects.

From part 6,
The p -value corresponding to the value of x2  is 7.8e-06 
Hence, we have strong enough evidence to conclude 
that the survival times of the populations
of the four groups are significantly differ."



# q2 rubric GHF
#
# 1 (explanation)                               
#                          selection bias [/5]: 5
# 2, 5 (Kaplan-Meier plot)                      
#     survival::Surv object (time, event) [/5]: 5
#                    Health vector formed [/5]: 5 might be clearer to use high/low rather than 1/0
#          survival::survfit (two models) [/5]: 5
#                   plot, identify curves [/5]: 5
# 3, 6 (log-rank test)                          
#                       survival:survdiff [/5]: 5
# 4, 7 (interpretations)                        
#                        meaning of plots [/5]: 5
#                      meaning of p-value [/5]: 5
#                                               
# q2 grade: 40/40
