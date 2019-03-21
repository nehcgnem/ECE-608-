## Activity 1 
setwd("/Users/SMC/学/ECE 608/07 - Jun 13 - Linear regression")
car.csv <- read.csv("C3 Cars.csv")


#plot(Mileage, Price, data=car.csv, main="Mileage vs Price Scatter plot", 
#    xlab="Mileage", ylab="Price ", pch=16)

library(ggplot2);
plot(ggplot(car.csv, aes(x=Mileage, y=Price)) +
  geom_point())
#or
require(mosaic)
xyplot( Price ~ Mileage, data=car.csv, main="")

"The plot doesnt show a strong relationship between Mileage and Price, 
 instead it shows that the upper bound of the car price is negatively related to 
milage"

## Activity 2
"explanatory mileage 
response Price"
attach(car.csv)
#cor(Mileage,Price)
fit <- lm(Price ~ Mileage)
"Coefficients:
(Intercept)      Mileage  
24764.5590      -0.1725  
Price = 24,765 - 0.1725(Mileage).
"
plot(car.csv$Mileage, car.csv$Price, data=car.csv, main="Mileage vs Price Least Mean Square Regression", 
    xlab="Mileage", ylab="Price ", pch=16, type = "p", cex = 0.5) + 
  abline(fit$coefficients, col="red")

# R squared value: 0.02046344
R2 <- 1 - (sum((car.csv$Price-(24765 - 0.1725*car.csv$Mileage))^2)/sum((car.csv$Price-mean(car.csv$Price))^2))

# correlation coefficient:-0.1725;indicate a weak negative relationship

# t = -3.337, df = 1552.8, p-value = 0.0008669
test <- t.test(Mileage,Price)
summary(test)
"A small p-value (typically ≤ 0.05) indicates strong evidence 
against the null hypothesis, so you reject the null hypothesis.
Mileage is not a strong indicator of Price "

## Activity 3
head(car.csv)
17314.10 - (24765 - 0.1725*8221)
"-6032.778"

##Activity 4
full.model= lm(Price~Cyl+Liter+Doors+Cruise+Sound+Leather+Mileage, data =car.csv)
step.model <- step(full.model)
summary(step.model)

##Activity 5
Cars <- car.csv
require(leaps)
full.data=cbind(Cars$Mileage,Cars$Cyl,Cars$Liter,Cars$Doors,Cars$Cruise,Cars$Sound,Cars$Leather)
head(full.data)

best.lm <- leaps(full.data,Cars$Price, method = "adjr2", names =c('Mileage','Cyl','Liter','Doors','Cruise','Sound','Leather'), nbest=4)
data1 = cbind(best.lm$which,best.lm$adjr2)


## Activity 7 redo 
with(car.csv,
cars.lm2 <<- lm(Price~Cyl+Doors+Cruise+Sound+Leather+Mileage)
)
residual <- cars.lm2$residuals 
fitted <- cars.lm2$fitted.values
car.csv <- mutate(car.csv, residual, fitted)


par(mfrow=c(2,4))
plot(car.csv$fitted,cars.lm2$residual)
plot(car.csv$Cyl,cars.lm2$residuals)
plot(car.csv$Doors,cars.lm2$residuals)
plot(car.csv$Cruise,cars.lm2$residuals)
plot(car.csv$Sound,cars.lm2$residuals)
plot(car.csv$Leather,cars.lm2$residuals)
plot(car.csv$Mileage,cars.lm2$residuals)

stripchart(cars.lm2$residuals ~ car.csv$Cyl, vertical = TRUE, method = "jitter", col=c("red","blue","green"))

stripchart(cars.lm2$residuals ~ cars.lm2$fitted, vertical = TRUE, method = "jitter")
stripchart(cars.lm2$residuals ~ car.csv$Mileage, vertical = TRUE, method = "jitter")
stripchart(cars.lm2$residuals ~ car.csv$Leather, vertical = TRUE, method = "jitter")


## Activity 7 old
library(data.table)
setDT(car.csv)
fit2 <- lm(Price ~ Cyl + Doors + Cruise + Sound + Leather + Mileage)
R2_best <- 1 - sum(Price-predict(fit2,car.csv))/sum((Price-mean(Price))^2)
# ??? R2_best = 1, then why still residual?

#plot(Mileage, Price, data=car.csv, main="Mileage vs Price Least Mean Square Regression", 
#     xlab="Mileage", ylab="Price ", pch=16, type = "p", cex = 0.5) + 
#  abline(fit2, col="red")

car.csv[ , Predicted := predict(fit2, car.csv)]
car.csv[ , Residual := Price - Predicted]
car.csv[ , Predicted := cars.lm2$fitted.values]

attach(car.csv)
plot(Mileage, Residual, data=car.csv, main="Residuals vs Mileage Least Mean Square Regression", 
     xlab="Mileage", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
  abline(h = 0)

# Alternative
# plot(Mileage,residuals(fit2), main="Residuals vs Mileage Least Mean Square Regression", 
#     xlab="Fitted value", ylab="Residual ", pch=16, type = "p", cex = 0.5)

plot(Predicted, Residual, main="Residuals vs Predicted Price Least Mean Square Regression", 
     xlab="Fitted value", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
  abline(h = 0)

# a.Does the size of the residuals tend to change as mileage changes?
# The Residual versus Mileage plot is wedge shaped, the residual converges as Mileage increases

# b.Does the size of the residuals tend to change as the predicted retail price changes? You should see patterns indicating heteroskedasticity (nonconstant variance).
# The residual exhibits Heteroskedasticity, clusters as plotted 

# c.Another pattern that may not be immediately obvious from these residual plots is the right skew- ness seen in the residual versus mileage plot. Often economic data, such as price, are right skewed. To see the pattern, look at just one vertical slice of this plot. With a pencil, draw a vertical line cor- responding to mileage equal to 8000.
# Are the points in the residual plots balanced around the line Y = 0?
# no
plot(Predicted, Residual, main="Residuals vs Predicted Price Least Mean Square Regression", 
     xlab="Mileage", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
  abline(h = 0, v= 8000)


## Activity 8

fit3 <- lm(log(Price) ~ Cyl + Doors + Cruise + Sound + Leather + Mileage)
 plot(predict(fit3, car.csv),residuals(fit3), main="Residuals vs Mileage with log Least Mean Square Regression", 
     xlab="Fitted value", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
   abline(h = 0, v= 8000, col="red")

 fit4 <- lm(sqrt(Price) ~ Cyl + Doors + Cruise + Sound + Leather + Mileage)
 plot(predict(fit4, car.csv),residuals(fit4), main="Residuals vs Mileage with square root Least Mean Square Regression", 
      xlab="Fitted value", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
   abline(h = 0, v= 8000, col ="red") 
# a. the log transformation does a better job in remove right skewness 

 R2_log <- 1 - (sum((log(Price)-predict(fit3, car.csv))^2)/sum((log(Price)-mean(log(Price)))^2))
 R2_sqrt <- 1 - (sum((sqrt(Price)-predict(fit4, car.csv))^2)/sum((sqrt(Price)-mean(sqrt(Price)))^2))
 
# b. R2_log = 0.4836282; R2_sqrt = 0.4689459, 
# yes the best fit plot corresponds to a better R value(closer to 1). 
# As a larger R2 value indicates that more of the variation in the response 
# variable is explained by the model, and when the model is better fitted, 
# the residual is expected to be smaller.
 
 
## Activity 9
# car.csv[ , order := 1:nrow(car.csv)]
 fit <- lm(log(Price) ~ Cyl + Doors + Cruise + Sound + Leather + Mileage)
plot(1:nrow(car.csv),residuals(fit), main="Residuals vs Order", 
     xlab="Order", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
  abline(h = 0, v= 8000, col ="red") +
lines(1:nrow(car.csv), residuals(fit), col="blue", cex=0.1)

# answer: the order of table is ordered by make, model and trim, so the order pattern is 
# resulted from the make, model and trim of the car.

## Activity 10 
plot(1:nrow(car.csv),residuals(fit3), main="Residuals vs Order(perfect model)", 
     xlab="Order", ylab="Residual ", pch=16, type = "p", cex = 0.5) +
  abline(h = 0, v= 8000, col ="red") +
lines(1:nrow(car.csv), residuals(fit3), col="blue", cex=0.1)
# answer: The pattern is more complex, and less clustered, the residual is smaller 
# as compared with result from activity 9.

## Activity 11
#X11()

plot(Predicted, Residual, main="Outlier: Residuals vs Predicted Price Least Mean Square Regression", 
     xlab="Fitted value", ylab="Residual ", pch=16, type = "p", cex = 0.9) +
  abline(h = 0)

plot(predict(fit3, car.csv),residuals(fit3), main="Outlier: Residuals vs Mileage with log Least Mean Square Regression", 
     xlab="Fitted value", ylab="Residual ", pch=16, type = "p", cex = 0.9) +
  abline(h = 0, v= 8000, col="red")

#a. outliers are small cluster in the upper right corner of the plots

#b. no, the outliers can't be seen on the ordered plot, as they could be seperated by the order

## Activity 12
# yes removal of the outliers do affect the coefficients in the regression line
## Activity 13
plot(Mileage, log(Price), data=car.csv, main="Mileage vs TPrice regression line", 
     xlab="Mileage", ylab="Price ", pch=16, type = "p", cex = 0.5) + 
  abline(lm(log(Price) ~ Mileage), col="red")

fit5 <- lm(log(Price) ~ Mileage)
hist(residuals(fit5))

qqnorm(residuals(fit5), main="Normal probability plot", pch=16, cex = 0.1)
#a. Do the residuals appear to follow the normal distribution?
#ans: no, the residuals is right skewed

#b. Are the ten outliers visible on the normal probability plot and the histogram?
#ans: yes, in the upper right corner of the plot.