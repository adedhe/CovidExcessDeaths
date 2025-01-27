library(tidyverse) #For data manipulation
library(ggplot2) #For plotting   

# Sample adjusted bootstrap April 2023
round3 <- read.csv("/Users/.../Round3.csv")
round3<- round3 %>%
  filter (!is.na(TrueNumber))
death_holder<-round3$TrueNumber
means<-c()

for (i in 1:10000){
  
  # Simple average mu = 2.25 , sigma = 0.65
  sa <- rnorm(1, mean = 2.25, sd = 0.65)
  
  # Farrington surveillance mu = 1.01 , sigma = 0.60
  # we are considering the Farrington to be two-sided (unlike the one-sided result)
  # this just makes the margin of error more wide
  fs <- rnorm(1, mean = 1.01, sd = 0.6)
  
  # Overdispersed Poisson mu = 1.67 , sigma = 0.65
  op <- rnorm(1, mean = 1.67, sd = 0.65)
  
  # Death compensations mu = 1.43 , sigma = 0.9
  dc <- rnorm(1, mean = 1.43, sd = 0.9) 
  
  # Wisdom of crowds  
  wc<-sample(death_holder, 1, replace = TRUE)/9117   # 9117 was the "official value" used in the survey
  
  # Adding mean of this current iteration to the final vector called "means"
  new_bootstrap<-c(sa, fs, op, dc, wc)
  means<-c(means, mean(new_bootstrap))
}


means<-sort(means)
hist(means, breaks = 20)
