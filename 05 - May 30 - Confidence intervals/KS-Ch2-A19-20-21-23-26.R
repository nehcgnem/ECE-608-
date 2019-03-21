ibrary("ggplot2")
setwd("/Users/SMC/学/ECE 608/05 - May 30 - Confidence intervals")
game_data = read.csv("C2 Games1.csv")
##Acivity 19
y_bar = mean(game_data[,"Time"])
#36.825
y1_bar = mean(game_data[game_data$Type=="Color","Time"])
#38.1
y2_bar = mean(game_data[game_data$Type=="Standard","Time"])
#35.5

##Acivity20
effect_distract_std = y1_bar - y_bar
#1.275
effect_distract_color = y2_bar - y_bar
#-1.275

##Acivity21
#main effect plot
mep=data.frame(typeNu=c(1,0), average=c(35.55,38.10))
ggplot(mep, aes(x=typeNu, y=average)) +
  geom_line()+
  geom_point(size=1)+
  coord_cartesian(xlim = c(0, 1), ylim =c(35.5,38.1)) 


plot(c(0,1),c(mean(Color),mean(Standard)),xlim=c(-.1,1.1),ylim=c(35.2,38.5),
     ylab="Mean Time",pch=16,xlab="Type",main="Main Effects Plot")
lines(c(0,1),c(mean(Color),mean(Standard)))
text(c(0,1),c(35.2,35.2),c("Color","Standard"))
##Acivity23
#result from summary(linear_model)
#Residual standard error: 3.527 on 38 degrees of freedom
#Multiple R-squared:  0.1209,	Adjusted R-squared:  0.09778 
#F-statistic: 5.227 on 1 and 38 DF,  p-value: 0.02791

#solution
summary(aov(Time~Type,data=game_data))

##Acivity26
residual_c = game_data[game_data$Type=="Color","Time"] - y1_bar
residual_s = game_data[game_data$Type=="Standard","Time"] - y2_bar
hist(c(residual_c,residual_s))
plot(density(c(residual_c,residual_s)))

residual_s = game_data[game_data$Type=="Standard",c("studentID","Time")]
residual_c = game_data[game_data$Type=="Color",c("studentID","Time")]

residual_s[,"Time"] = residual_s[,"Time"] - y1_bar
residual_c[,"Time"] = residual_c[,"Time"] - y2_bar
ggplot(rbind(residual_c,residual_s),aes(x=studentID,y=Time))+ 
  geom_line()+
  geom_point()

#solution 
D1<-(game_data$Type=="Color")*1
plot(c(residual_c,residual_s) ~ D1, col='red')


D1 <- (game_data$Type=="Color")*1
plot((c(residual_c,residual_s))~D1, type=)
plot((c(residual_c,residual_s))~game_data$studentID)
plot((c(residual_c,residual_s))~game_data$studentID, type='l')
plot((c(residual_c,residual_s))~game_data$studentID, type='l')+points(c(residual_c,residual_s))