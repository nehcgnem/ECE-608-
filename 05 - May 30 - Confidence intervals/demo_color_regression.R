df <- read.csv("C2 Games1.csv")



with(df, # with function replace $?? 
     { # quick and dirty to do two function
     stripchart(Time ~ Type)
     boxplot(Time ~ Type)
     #null hypothesis 
     #test <-t.test(Time ~ Type, var.equal = TRUE, 
     test <<-t.test(Time ~ Type, var.equal = TRUE, 
                    # <<- global environment dirty and quick
                   alternative = "two.sided", conf.level =0.95)
                   # null hypothesis two means are equal? 
                   # alternative in R is the alternative hypothesis 
     # 4.808 indicate the mean is likely 
     #to noe equal to each other the difference is 4.808 +- with 95%
     regression <<-lm(Time ~ Type)
     my.aov <<- aov(Time ~ Type) # anova is slightly different 
     }
) 
# T test
print(summary(test))
print(test)
# regression 
print(regression) 
print(summary(regression)) # P value 
# anova 
print(my.aov) 
print(summary(my.aov)) # P value 




