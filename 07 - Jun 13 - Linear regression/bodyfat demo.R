setwd("/Users/SMC/学/ECE 608/07 - Jun 13 - Linear regression")

df <- read.table("bodyfat.txt", header = TRUE, sep = "\t",skip=116, dec = ".", nrow=252)
y <- df$PCTBF
x <- df$Chest
plot (y ~ x)
plot(df$PCTBF ~ df$Abdomen, cex=0.2, col="red") 

plot(df$PCTBF ~ df$Age, cex=0.2, col="red", main="PCTBF Age") 

plot(df$PCTBF ~ df$Weight, cex=0.2, col="red", main="PCTBF weight") 

plot(df$Density ~ df$Weight, cex=0.2, col="red", main="Density weight") 

plot(df$Density ~ df$Height, cex=0.2, col="red", main="Density Height") 
"outlier due to wrong data "


#step 2

plot(df$PCTBF ~ df$Abdomen, cex=0.2, col="red") 
r <- cor (df$PCTBF, df$Abdomen)

r= 0.8134323 # linearity measured by correlation 

reg.model <- lm(y ~ x)
"> print(reg.model)

Call:
lm(formula = df$PCTBF ~ df$Abdomen)

Coefficients:
(Intercept)   df$Abdomen  
-39.2802       0.6313  "

"> coefficients(reg.model)
(Intercept)  df$Abdomen 
-39.2801847   0.6313044 
> reg.model$coefficients
(Intercept)  df$Abdomen 
-39.2801847   0.6313044"

beta.hat <-coefficients(reg.model)
abline(beta.hat)
e <- residuals(reg.model) # error term 
plot(e)
# residuals can see drift due to environmental changes, change of unit, change od doctor 

e.stardized <- rstandard(reg.model) # standardize unit, to visulize outliers interms of std deviation
plot(e.stardized) # one outlier at -4 sigma is suspicious 

y.hat <-  fitted.values(reg.model)

plot(e ~ y.hat) # asses vertical spread, regression line is y = 0 line 
plot(e ~ x)
plot(e.stardized ~ x)

# test on normality of the model, because the error was assumed to be normally distributed 
# histogram on error term/ residual 
hist(e.stardized, xlim = c(-4,4), ylim= c(0,0.5), freq = FALSE) 
  curve(dnorm, add = TRUE) # dnorm is density, integrate to 1

# normal probility plot 
qqnorm(e.stardized) 
qqline(e.stardized, probs = c(0.025, 0.975)) # five percent confidence interval, remove outlier, boundary with less data

# TBD: prediction error for a preticular value x
reg.sum <- summary(reg.model)
#chest.df <- data.frame (chest = 125)
prediction.interval <- predict(reg.model, interval = "predict")
