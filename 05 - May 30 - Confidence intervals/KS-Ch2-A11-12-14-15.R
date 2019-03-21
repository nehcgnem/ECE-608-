library("ggplot2")
setwd("/Users/SMC/学/ECE 608/05 - May 30 - Confidence intervals")

game_data = read.csv("C2 Games1.csv")

## Activity 11
#colors=c(0,1)
#names(colors)=c("Standard", "Color")
#colors[c("Standard", "Color")]

for(row in 1:nrow(game_data)) {
  if(game_data[row,"Type"] == "Color")
    game_data[row,"typeNu"] = 1
  else
    game_data[row,"typeNu"] = 0
}

linear_model = lm(Time ~ typeNu, data=game_data)
plot(linear_model)


## Activity 12
summary(linear_model)
confint(linear_model, 'typeNu', level=0.95)
#answer 0.95 confidence interval is 4.807975 +/- 0.2920254
#the beta1 coefficient is significant different from zero

## Activity 14
cbind(game_data, predicted=predict(linear_model,game_data[,c("Time","typeNu")]))
#predicted 38.1 for Color, and 35.55 for Standard
residual_c = game_data[game_data$Type=="Color","Time"] - 38.10
residual_s = game_data[data_games1$Type=="Standard","Time"] - 35.55
#residual for color
hist(residual_c)
#residual for standard
hist(residual_s)
#Total residual
residual = rbind(residual_c,residual_s)
#Plot for total residual
hist(residual)
plot(density(residual))

## Activity 15
ggplot(game_data, aes(x=typeNu, y=Time)) + 
  geom_abline(data = game_data, intercept=35.55,slope=2.55 )+
  geom_point(size=1) 

