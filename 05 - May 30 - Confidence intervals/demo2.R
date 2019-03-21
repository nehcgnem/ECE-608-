# compare the t versus q normal distribution, with mean improved?

n <- 2
t.crit <- qt(p = 0.975, df = n-1) # 95% interval is 1.96 std deviation 
#t.crit <- qt(p = 0.95) # 90% interval is 1.64 std deviation 

reps=1000
intervals<-sapply(1:reps, function(x){ # x here is dummy? 
 
y <-rnorm(n, mean =0, sd=1) # generate normally distributed data 
# true mean is zero 
#find mean of y
y.bar <- mean(y)
#find variance 
sigma.squared <-var(y)
#std deviation 
sigma<-sd(y)
conf.internval.95 <-c(y.bar - t.crit*sigma/sqrt(n),
                   y.bar + t.crit*sigma/sqrt(n))} )
# length(intervals[intervals[1,] <0 & intervals[2,]>0]) # doesn't work 
#low.ok <- sum((intervals[1,] <=0 ))
#high.ok <- sum((intervals[2,] >=0 )) ## column two? 

ok <- sum(intervals[1,]<=0 & intervals[2,]>=0)/reps

# ok shows the 95% the time it falls in the interval 
