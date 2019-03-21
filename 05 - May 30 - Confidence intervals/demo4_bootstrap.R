# compare the t versus q normal distribution, with mean improved?
# boot strap?
n <- 100
reps <-100
intervals <- sapply(1:reps, function(X){  
y <- rnorm(n, mean=0, sd =1)

bootstrap.reps=100 # repeats
medians <- sapply(1:bootstrap.reps, function(x){
y.resample <-sample(y, replace = TRUE)
median(y.resample)})
interval <- quantile(medians, probs = c(0.025, 0.975))  })

ok <- sum(intervals[1,] <= 0 & intervals[2,]>=0)/reps # how interval has two colum 