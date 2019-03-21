# find how many times the mean doesn;t fall in between the conf interval 
#z.crit <- qnorm(p = 0.975) # 95% interval is 1.96 std deviation 
z.crit <- qnorm(p = 0.95) # 90% interval is 1.64 std deviation 


n=1000
intervals<-sapply(1:n, function(x){ # x here is dummy? 
#small sample size, confidence level reduces
#large sample size, stable result 
  n <- 200
y <-rnorm(n, mean =0, sd=1) # generate normally distributed data 
# true mean is zero 
#find mean of y
y.bar <- mean(y)
#find variance 
sigma.squared <-var(y)
#std deviation 
sigma<-sd(y)
conf.internval.95 <-c(y.bar - z.crit*sigma/sqrt(n),
                   y.bar + z.crit*sigma/sqrt(n))} )
# length(intervals[intervals[1,] <0 & intervals[2,]>0]) # doesn't work 
#low.ok <- sum((intervals[1,] <=0 ))
#high.ok <- sum((intervals[2,] >=0 )) ## column two? 

ok <- sum(intervals[1,]<=0 & intervals[2,]>=0)/n

# ok shows the 95% the time it falls in the interval 
