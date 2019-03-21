setwd("/Users/SMC/Downloads/Final Exam")
hiv.data<-read.csv("q3/hiv.csv")
#filter data
hiv.filter <- subset( hiv.data, subset = ( Aid >0  & ! is.na( Literacy ) & ! is.na( Gdp ) & ! is.na( Density ) & ! is.na( Stability )  & ! is.na( Co2 ) & ! is.na( Growth )& ! is.na( Aid )& ! is.na( Urban )& ! is.na( Hiv )) ) 

# Best model though R squared value
# The function that will be used to perform best subsets regression is also called leaps.
# The following code can be used to select the "best" models based on the highest value of R^2

require(leaps)
attach(hiv.filter)

full.data=cbind(log(Gdp),log(Density), log(Co2),log(Aid),log(Urban),Literacy,Stability,Growth) # include all explantory variable

best.lm <- leaps(full.data,hiv.filter$Hiv, method = "adjr2", names =c('log(Gdp)','log(Density)', 'log(Co2)','log(Aid)','log(Urban)','Literacy','Stability','Growth'), nbest=3)
cbind(best.lm$which,best.lm$adjr2)
"  log(Gdp) log(Density) log(Co2) log(Aid) log(Urban) Literacy Stability Growth R^2(Adjusted)         
1        0            0        1        0          0        0         0      0  0.108100391
1        1            0        0        0          0        0         0      0 -0.005330884
1        0            0        0        0          0        0         1      0 -0.009337640
2        0            0        1        0          1        0         0      0  0.135598641
2        0            0        1        0          0        1         0      0  0.116938699
2        0            1        1        0          0        0         0      0  0.100019022
3        0            0        1        0          1        1         0      0  0.163495028
3        0            1        1        0          0        1         0      0  0.127678344
3        0            0        1        1          1        0         0      0  0.103248914
4        0            1        1        0          1        1         0      0  0.133921685
4        0            0        1        1          1        1         0      0  0.133365924
4        0            0        1        0          1        1         0      1  0.124605453
5        1            0        1        1          1        1         0      0  0.106258025
5        0            1        1        1          1        1         0      0  0.100749838
5        0            1        1        0          1        1         0      1  0.095548991
6        1            1        1        1          1        1         0      0  0.101233629
6        0            1        1        1          1        1         0      1  0.059530776
6        1            0        1        1          1        1         1      0  0.058684414
7        1            1        1        1          1        1         1      0  0.055512328
7        1            1        1        1          1        1         0      1  0.049142055
7        0            1        1        1          1        1         1      1  0.004454161
8        1            1        1        1          1        1         1      1 -0.003486241"


"From above table we can find a model with the highest adjusted Rsquared value of 0.163495028, 
 so we should include log(Co2), log(Urban) and Literacy in our final model"

best.model <- lm(log(Hiv)~ log(Co2)+ log(Urban) + Literacy, data =hiv.filter)
summary(best.model)

"Call:
lm(formula = log(Hiv) ~ log(Co2) + log(Urban) + Literacy, data = hiv.filter)

Residuals:
Min      1Q  Median      3Q     Max 
-2.0137 -0.6448  0.1752  0.6950  1.7619 

Coefficients:
Estimate Std. Error t value Pr(>|t|)   
(Intercept) -6.007034   2.407518  -2.495  0.02100 * 
log(Co2)    -0.889626   0.239672  -3.712  0.00129 **
log(Urban)   1.252256   0.573797   2.182  0.04058 * 
Literacy     0.008525   0.014220   0.600  0.55525   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.105 on 21 degrees of freedom
Multiple R-squared:  0.3982,	Adjusted R-squared:  0.3123 
F-statistic: 4.632 on 3 and 21 DF,  p-value: 0.01226"

#Conclusion 
"From the above summary, 
 log(Co2) has a coefficient of -0.889626 with a p-value= 0.00129
 log(Urban) has a coefficient of 1.252256 with a p-value= 0.04058, 
 so we can conclude that log(Co2) is a statistically significant(at alpha = 0.05)
 predictor of lower hiv, while log(Urban) is a statistically significant (at alpha = 0.05)
 predictor of higher hiv. For Literacy, it does help improve the model accuracy, but it is
 not a statistically significant (at alpha = 0.05) predictor of higher hiv"

# q3 rubric GHF
#                                                                
#                able to form a multiple-regression model [/ 5]:  5
#         able to do model selection (step, leaps::leaps) [/10]: 10
#                 did something reasonable with NA values [/ 5]:  5
#       explained rationale for steps taken, chosen model [/ 5]:  5
#    evaluated quality/deficiences of models investigated [/ 5]:  5
#                showed residual normality (hist, qqnorm) [/ 5]:  0 not done
#      showed residual homescedasticity, uncorrelatedness [/ 5]:  0 not done
#                                                                
# q3 grade: 30/40
