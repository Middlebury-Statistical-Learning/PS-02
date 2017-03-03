# Emily Miller
# Stat 218 Middlebury College
# Problem Set 2


library(tidyverse)
setwd("C:/Users/Emily Miller/Documents/Stat_learning")
train <- readr::read_csv("CSVs/train.csv")

# #1: Leave one out cross validation
# Find length of train
t_length = nrow(train)
obs = seq(t_length)

 #initilize empty MSE list
count_correct = 0

# Iterate through and perform leave one out cross validation
for (val in obs) {
  pseudo_test <- train[val,]
  #pseudo_train <- train %>% 
    #anti_join(pseudo_test, by="PassengerId")
  # Skip training on and making pseudo_train because simple model?
  Pred_Survive <- ifelse(pseudo_test$Sex == "female", 1, 0)
  if (Pred_Survive == train[val,]$Survived){
    count_correct = count_correct + 1
    }
}
  
percent_correct = count_correct/t_length*100
print(percent_correct)





# #2: k = 5 fold cross validation
rm(list = ls())

setwd("C:/Users/Emily Miller/Documents/Stat_learning")
train <- readr::read_csv("CSVs/train.csv")
folds = 5

# create fractions to split data later using sample_frac
split_frac = numeric() #initialize
split_frac[1] = 1/folds
for (i in 2:(folds - 1)) {
  split_frac[i] = (1/folds)/(1 - ((i-1)/folds))
}
remaining <- train
i <- 1
var_names <- paste("fold", 1:(folds - 1), sep = "")

# split data into k = 5 folds
for (val in split_frac) {
  assign('temp', sample_frac(remaining, val, replace = FALSE))
  remaining <-remaining %>% 
    anti_join(temp, by="PassengerId")
  assign(var_names[i], temp)
  i <- i +1
  rm(temp)
}
fold5 <- remaining

MSE_list <- numeric()

# Find 5 MSE values
for (j in 1:folds) {
  assign('test_set', get(paste0('fold', j)))
  test_set$Pred_Survive <- ifelse(test_set$Sex == "female", 1, 0)
  test_set$squared_error = (test_set$Survived - test_set$Pred_Survive)^2
  MSE_list <- append(MSE_list, mean(test_set$squared_error))
  rm(test_set)
} 

Percent_correct = (1 - mean(MSE_list))*100
print(Percent_correct)

# If Kaggle reported scores on the entire data set, you could systematically
# change data to see if it would increase your score. While the 10 sumbission a day limit would
# put somewhat of a cap on this, if you were to overfit your model to the entire test data 
# and then change the model based upon your kaggle score, it would be possible to exploit
# the leaderboard strategy to get a perfect 100%