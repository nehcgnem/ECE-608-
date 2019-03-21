setwd("/Users/SMC/学/ECE 608/07 - Jun 13 - Linear regression")

df <- read.table("bodyfat.txt", header = TRUE, sep = "\t",skip=116, dec = ".", nrow=252)

with(df, { # with equal attach 
  par.save <- par(las = 1)
  reg.model <<- lm(PCTBF ~ Age+#Weight
                   Height+Neck+Chest+Abdomen+Hip+Thigh+
                     Knee+Ankle+Biceps+Forearm+Wrist)
  
  par(par.save)

  fitvalues <<- fitted.values(reg.model) # predict PCTBF
  residual <<- residuals(reg.model)
  plot(residual)
  plot(residual ~ fitvalues)
  
  # hist(residual, frequency = FALSE) 
  # curve(dnorm, add = TRUE)
  
  infmeasure <<- influence.measures(reg.model)
         
  }
)
