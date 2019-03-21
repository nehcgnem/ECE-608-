## Activity 1 
# The observational units are 37 FNA slide samples.
# Explanatory variable is the shape of nuclei.
# Response variable is the tumor type. 

## Activity 2 
library("ggplot2")
setwd("/Users/SMC/学/ECE 608/06 - Jun 06 - Testing statistical hypotheses")
table <- read.csv("C6 Table 6.1.csv")
#matrix <- as.matrix(table, rownames =TRUE)
round_proportion <- table[1,"Malignant"]/rowSums(table[1,2:3])
concave_proportion <- table[2,"Malignant"]/rowSums(table[2,2:3])
# concave sample malignant proportion 0.8095238
# round sample malignant proportion 0.4375 

## Activity 3

library(ggplot2)
library (scales) 
library(reshape2)

meltd<- melt(table, id.vars=1) 
plot(ggplot(meltd, aes(x=X, y=value, fill=variable))+
  geom_bar(stat="identity",position = "fill")+
  scale_y_continuous(labels = percent_format())+
    theme_grey())


tumors <- cbind(c(7/16,9/16),c(17/21,4/21))
barplot(tumors,xlab="Shape",legend=c("Malignant","Benign"),
        ylab="Proportion Malignant",names.arg=c("Round","Concave"),
        xlim=c(0,3.8))



#The graph shows evidence that nucleus shape is relate to the likelihood of a 
# cell being malignant as the proportion of concave cells that are malignant is
# so high as compared with round nucleus shape 
 
## Activity 6 
#create a list of object with 24 M, and 13 B
a=c(matrix(1, 24),matrix(0,13,))
count.malignant = sum(sample(a, 21))
## Activity 8
trails <- 10000
count <- sapply(1:trails, function(x){
  sum_m <-sum(sample(a,21, replace = FALSE))
  
#?? with or without replacement? same result? 
  })
hist(count)
p_value = sum(count>=17)/ 10000 

P17_sim=sum(count==17)/ 10000 
P18_sim=sum(count==18)/ 10000 
P19_sim=sum(count==19)/ 10000 
P20_sim=sum(count==20)/ 10000 
P21_sim=sum(count==21)/ 10000  
#solution 
reps <- 10000
results <- numeric(reps) 
cancer.cells <- c(rep("M",24),rep("B",13))
for (i in 1:reps) {
  results[i] <- sum(sample(cancer.cells,21, replace = FALSE)=="M")
}
sum(results>=17)/reps




## Activity 9 
comb = function(n, x) {
  factorial(n) / factorial(n-x) / factorial(x)
}
fisher = function(M,N,n,x){
  comb(M,x)*comb(N-M,n-x)/comb(N,n) 
}

P17_fisher=fisher(24,37,21,17)
P18_fisher=fisher(24,37,21,18)
P19_fisher=fisher(24,37,21,19)
P20_fisher=fisher(24,37,21,20)
P21_fisher=fisher(24,37,21,21)
Pfisher <- c(P17_fisher,P18_fisher,P19_fisher,P20_fisher,P21_fisher)

Psim <- c(P17_sim,P18_sim,P19_sim,P20_sim,P21_sim)
## Activity 10
x <- seq(17, 21, 1)
df <- data.frame(x,Pfisher,Psim)

melt.df<- melt(df, id.vars=1) 


plot(ggplot(melt.df,aes(x=x, y=value, fill=factor(variable))) +   
  geom_bar(stat="identity",position = "dodge")+
  theme_grey())

##redo 
sim = function(n) {
  reps <- 10000
  results <- numeric(reps) 
  cancer.cells <- c(rep("M",24),rep("B",13))
  for (i in 1:reps) {
    results[i] <- sum(sample(cancer.cells,21, replace = FALSE)=="M")
  }
  sum(results==n)/reps
}

reps <- 21
Pfisher <- numeric(reps) 
for (i in 1:reps) {
  #Pfisher[i] <- fisher(24,37,21,i)
  Pfisher[i] <- dhyper(i, 24, 13, 21)
}

Psim <- numeric(reps) 
for (i in 1:reps) {
  Psim[i] <- sim(i)
}


x <- seq(1, 21, 1)
df2 <- data.frame(x,Pfisher,Psim)

melt.df2<- melt(df2, id.vars=1) 

plot(ggplot(melt.df2,aes(x=x, y=value, fill=factor(variable))) +   
       geom_bar(stat="identity",position = "dodge")+
       theme_grey())


## Activity 11
c <- sum(P17_fisher,P18_fisher,P19_fisher,P20_fisher,P21_fisher)
# Pfisher17plus = 0.02247743
# p_value = sum(count>=17) = 0.0205
# The results are quite comparable 

#redo
#p(x>=17)
P17 <- 1 - phyper(16,24,13,21)
## Activity 12
P9_fisher=fisher(13,37,16,9)
P10_fisher=fisher(13,37,16,10)
P11_fisher=fisher(13,37,16,11)
P12_fisher=fisher(13,37,16,12)
P13_fisher=fisher(13,37,16,13)

Pfisher9plus <- sum(P9_fisher,P10_fisher,P11_fisher,P12_fisher,P13_fisher)
# Pfisher9plus = 0.02247743 
# The result matches with A11

## Activity 13
p = sapply(0:4, function(x){
  p = fisher(13,37,21,x)
})

Pfisher4less=sum(p)
# Pfisher4less = 0.02247743 
# The result matches with A11

## Activity 15
# where does the 10 come from ? 
p_value_2side = sum(count>=17 | count <=10)/ 10000 
# P value is 0.0344 

## Activity 16
p = sapply(8:10, function(x){
  p = fisher(24,37,21,x)
})
Pfisher10less = sum(p)
p_value_hypegeomatric = Pfisher10less + Pfisher17plus
# fisher exact value for two sided test is 0.03573574


## Activity 17
e_concave_malignant=21*24/37
# 13.62
## Activity 18
Pbenign=13/37
# benign proportion 0.3513514
e_round_benign= 16 *Pbenign
# estimated round benign 5.621622

## Activity 19
## ???how to convert oringinal data in to matrix that can be used directly
sam <- matrix(c(9,4,7,17),nrow=2,ncol=2)
chisq.test(sam,correct=FALSE)  # correct=FALSE ???
#X-squared = 4.0032, df = 1, p-value = 0.04541



