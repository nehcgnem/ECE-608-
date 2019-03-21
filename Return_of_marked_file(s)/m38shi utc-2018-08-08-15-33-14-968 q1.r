# a
ssx = function(L, n) {
  result = rep(0,L)
  for (i in 1:L) {
    temp = rnorm(n, 0, 1) - mean(rnorm(n, 0, 1))
    result[i] = sum(temp**2)
  }
  result
  
}

ssy = function(L, n) {
  result = rep(0,L)
  for (i in 1:L) {
    temp = rnorm(n-1, 0, 1)
    result[i] = sum(temp**2)
  }
  result
}


#2
hist(ssx(1000, 10), xlim = c(0,30)) 
hist (rchisq(1000,10-1)) # chi distribution for comparison

hist(ssy(1000, 10), xlim = c(0,30))


# q1 rubric GHF
#
# 1 (functions)                                  
#                                    rnorm [/3]: 3
#                         n or n-1 samples [/2]: 2
#                                     mean [/2]: 2
#               sum of squared differences [/3]: 2 messed up mean subtraction in 'ssx'
#                           repeat L times [/3]: 3
#                                 function [/2]: 2
# 2 (histograms)                                 
#                   function call, L and n [/2]: 2
#        hist, constraints (n, xlim, freq) [/4]: 3 some constraints missing
#                                chisq, df [/3]: 2 ~did histogram of chi-square deviates instead
#                                    curve [/1]: 0
# 3 (quantile-quantile plot)                     
#                                     sort [/2]: 0 not done
#     plot, constraints (type, xlim, ylim) [/3]: 0
# 4 (regression line)                            
#                                 lm, sort [/2]: 0 not done
#                     coefficients, abline [/3]: 0
# 5 (interpretation)                             
#                   regression assumptions [/2]: 0 not done
#                       arguments specific [/2]: 0
#                       statements correct [/1]: 0
#                                                
# q1 grade: 21/40
