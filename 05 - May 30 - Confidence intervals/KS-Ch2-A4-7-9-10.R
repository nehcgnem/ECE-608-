library("ggplot2")
setwd("/Users/SMC/学/ECE 608/05 - May 30 - Confidence intervals")
## Activity 4
game_data = read.csv("C2 Games1.csv",sep = ",")
ggplot(game_data, aes(x=Type, y=Time)) + 
  geom_boxplot(fill="black", alpha=0.2)
# Color Game (mean:38.1, sd:3.654845)
mean_c=mean(game_data[game_data$Type=="Color","Time"])
sd_c=sd(game_data[game_data$Type=="Color","Time"])
# For standard game mean: 35.55, sd: 3.394655)
mean_s=mean(game_data[game_data$Type=="Standard","Time"])
sd_std=sd(game_data[game_data$Type=="Standard","Time"])
stripchart(game_data$Time ~ game_data$Type, vertical = 1)
boxplot(game_data$Time ~ game_data$Type)
## Activity 7
# residual calculation 
residual_c = game_data[game_data$Type=="Color","Time"] - mean_c
# residual of color game is normally distributed 
hist(residual_c)
# residual of standard game is skewed distributed
residual_std = game_data[game_data$Type=="Standard","Time"] - mean_s
hist(residual_std)
# residule of total residule is normally distributed 
hist(c(residual_c , residual_std))

#solution
Standard <- game_data[game_data$Type=="Standard",]$Time
Color <- game_data[game_data$Type=="Color",]$Time 
residuals <- c(Standard-mean(Standard),Color-mean(Color)) 
hist(residuals)
qqnorm(residuals)


## Activity 9
residual_c1 = game_data[game_data$Type=="Color",c("studentID","Time")]
residual_c1[,"Time"] = residual_c1[,"Time"] - mean_c
residual_s1 = game_data[game_data$Type=="Standard",c("studentID","Time")]
residual_s1[,"Time"] = residual_s1[,"Time"]- mean_s

#plot of color game time versus studentID
ggplot(residual_c1, aes(x=studentID, y=Time)) + 
  stat_summary(geom = 'smooth', alpha = 0.2, fill = 'black', color = 'black',
               fun.data = median_hilow, fun.args = list(conf.int = 1))

#plot of standard game time versus studentID
ggplot(residual_s1, aes(x=studentID, y=Time)) + 
  stat_summary(geom = 'smooth', alpha = 0.2, fill = 'black', color = 'black',
               fun.data = median_hilow, fun.args = list(conf.int = 1))
#the graph indicate the the observations are independent


#solution 
D1 <- (game_data$Type=="Color")*1
# this creates a new column 1 for Color and 0 for Standard 
games.reg1 <- lm(Time~Type,data=game_data)
summary(games.reg1) # to see the regression output 
residuals <- games.reg1$res 
plot(residuals~game_data$studentID,type="l",xlab="Order")
points(game_data$studentID,residuals)
abline(h = 0) 


## Activity 10
##??? not sure about the difference of the question asked, before and after 'in
#addition'
x=game_data[game_data$Type=="Color","Time"]
y=game_data[game_data$Type=="Standard","Time"]
t.test(Time~Type,data=game_data,var.equal = TRUE) 
t.test(x,y) 
#data:  x and y
#t = 2.2862, df = 37.795, p-value = 0.02794
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.2916224 4.8083776
#sample estimates:
#  mean of x mean of y 
#38.10     35.55 

#In addition 
t.test(Time ~ Type, data=game_data, conf.level=.95)
#data:  x and y
#t = 2.2862, df = 37.795, p-value = 0.02794
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.2916224 4.8083776
#sample estimates:
#  mean of x mean of y 
#38.10     35.55 

#The actual difference between two type is 2.55 
#which is within the 95% confidence interval 
#the null hypothesis can't be rejected.

#From the P value indicates a likelyhood of 0.02794 that a random chance alone would 
#create a difference between two sample means (y1 - y2) at least as large 
#as the one observed.



#Activity 11
new<-(game_data$Type=="Color")*1
m1<-lm(game_data$Time~new)


