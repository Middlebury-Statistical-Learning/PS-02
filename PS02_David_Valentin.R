
# Help from: Tina Chen

library(tidyverse)
library(MASS)
library(ISLR)
library(broom)

#Global Variables

train <- read.csv("/Users/dvalentin/Documents/Code/CS_Homework/MATH218_Stat_Learning/train.csv")

train <- train %>% dplyr::select(PassengerId, Survived, Sex) %>% mutate(fold=sample(1:3, replace=TRUE, size=nrow(train)))


kFoldValidation <- function(train) {
  
  #Holds the scores for kfold
  scores_kfold <- rep(0, times = 5)
  
  # creating the training pseudo sets
  for (i in 1:5) {
    pseudo_train <- train %>% 
      filter(fold != i)           #1/5
    pseudo_test <- train %>% 
      filter(fold == i)           #4/5
    
  }
  
  #looping to test pseudo train set 
  for (i in 1:5) {
    if (i <= 4) {
      score_val = pseudo_train[i:5,] %>% 
        # Create new column of predictions:
        mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
        # Compute score:
        summarize(Score = mean(Survived == Survived_predicted))
    } else { #finally check the score for the pseudo_test data
      score_val = pseudo_test[i:5,] %>% 
        # Create new column of predictions:
        mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
        # Compute score:
        summarize(Score = mean(Survived == Survived_predicted))
    }
    #assigns it to the i'th element in the array to the score_val
    scores_kfold[i] = score_val
  }
  
  #returns scores_kfold- the matrix which holds the scores
  return(scores_kfold)
  
}

leaveOneOut <- function(train) {
  
  #will hold all the scores and average them
  scores_lloc <- rep(0, times = nrow(train))
  
  #sample dataset
  pseudo_train <- train
  
  for(i in 1:nrow(train)){
    
    pseudo_train <- train %>% 
      filter(PassengerId != i)           #1/5
    pseudo_test <- train %>% 
      filter(PassengerId == i)           #4/5
    
    score_for_fold_loocv <- pseudo_test %>% 
      mutate(Predictions= ifelse(Sex == "female", 1, 0)) %>% 
      mutate(Correct = ifelse(Predictions == Survived, 1,0)) 
    
    scores_lloc[i] <- score_for_fold_loocv$Correct
  }
  
  return(scores_lloc)
}

total_kFold_scores = kFoldValidation(train)

total_lloc_scores = leaveOneOut(train)


# You could overfit the model to the training data and continue 
# to train the model on the training set so that it perfectly predicts the training data.
# THe other way is continue resubmitting and using the feadback from the leaderboard to
# improve your model, but I think you mentioned that Kaggle prevents this.




