# compare the t versus q normal distribution, with mean improved?
# boot strap?
n <- 100
y <- rnorm(n, mean=0, sd =1)
reps=1000 # repeats
medians <- sapply(1:reps, function(x){
y.resample <-sample(y, replace = TRUE)
median(y.resample)})

interval <-quantile(medians, probs = c(0.025, 0.975)) # confidence interval 