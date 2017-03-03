library(dplyr)
set.seed(45)

training <- read.csv("train.csv")

# Leave-One-Out Cross-Validation
LOOCV <- function(training)
{
count = as.numeric(count(training))
Summation = 0
for(i in 1:count)
  {pseudo_test <- slice(training,i)
  pseudo_test <- pseudo_test %>% 
    mutate(Predicted_Survived = ifelse(Sex == "female", 1, 0))
  pseudo_test <- pseudo_test %>% 
    mutate(correctness=(Survived==Predicted_Survived))
  if (pseudo_test$correctness==FALSE)
    Summation <- Summation + 1}

pseudo_MSE = (Summation/891)
pseudo_MSE

pseudo_score = 1-pseudo_MSE
pseudo_score}

LOOCV(training)

# k-fold Cross-Validation (5 splits)
k_fold <- function(training,number_of_splits)
{
MSE = 0
training_copy <- training
division = (as.numeric(count(training))/(number_of_splits))
for(i in 1:(number_of_splits))
  {
  
  pseudo_test <- sample_n(training, division)
  training_copy <- anti_join(training_copy, pseudo_test, by="PassengerId")
  pseudo_test <- pseudo_test %>% 
    mutate(Predicted_Survived = ifelse(Sex == "female", 1, 0))
  pseudo_test <- pseudo_test %>% 
    mutate(correctness=(Survived==Predicted_Survived))
  count = 0
  for (i in 1:division)
    {
    correctness_check <- slice(pseudo_test,i)
    if(correctness_check$correctness==FALSE)
      count = count + 1
    
  }
  MSE = MSE+ (count/division)
  }
  MSE = MSE/number_of_splits
  score = 1-MSE
  score
}
k_fold(training,5)

# If Kaggle reported your score on the leaderboard based on the entire dataset,
# you could incorporate variables and see how this impacts your score. You can then
# see how minor tweeks effect your score. Based on the information on the leader
# board, you could then determine whether or not to keep certain adjustments. 