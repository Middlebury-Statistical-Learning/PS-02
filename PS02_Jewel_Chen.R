library(broom)
library(dplyr)
library(readr)
library(tidyverse)

# read in the data
train <- read.csv("~/Desktop/College/Senior Year 2016-2017/Statistical Learning/HW2/train.csv")

# LOOCV
counter <- 0
for (i in 1:nrow(train)) { # iterate through the data
  # if sex = female for observation i, set predicted value to 1
  predicted <- ifelse(train[i,]$Sex == "female", 1, 0)
  if (predicted == train[i,]$Survived) counter = counter + 1
}
psuedo_score <- counter / nrow(train) # calculate the percentage correct
print(psuedo_score)

# k-folds
scores <- rep(0, times = 5) # blank vector to keep score for each fold

for (i in 1:5) {
  psuedo_test <- train[(nrow(train) * (i-1)/5):(nrow(train) * (i)/5),] # psuedo_test is 1/5 of train data
  
  counter = 0
  for (x in 1:nrow(psuedo_test)){
    predicted <- ifelse(psuedo_test[x,]$Sex == "female", 1, 0)
    if (predicted == psuedo_test[x,]$Survived) counter = counter + 1
  }
  counter = counter / nrow(psuedo_test)
  scores[i] = counter
}
print(scores)
average <- mean(scores)
print(average)

We saw that Kaggle takes the test data (418 rows), only reports your score on the 
leaderboard based on half of these data, and declares the winner based on the other half which 
is withholded until the very end of the competition. Not only that, Kaggle does not tell you 
how they split the 418 rows. Say Kaggle didn’t do this and reported your score on the 
leaderboard based on the entire test data (418 rows). Write 2-3 sentences outlining a strategy 
of how you could exploit the information given by the leaderboard to get a perfect score of 100%.

I would submit the max alloted amount a day and throw everything and the kitchen sink 
into the model. Anything that improved the performance of the model on the test set would be 
included in the ultimate submissions. This leads to overfitting with regards to a particular 
data set. 
