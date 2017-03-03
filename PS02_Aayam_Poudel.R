library(tidyverse)

#given_submission <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW2/given_submission.csv")
train <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW2/train.csv")
#test <- read_csv("~/Desktop/Junior Spring/Statistical Learning/Homework/HW2/test.csv")

#LOOCV:

LOOCVscore <- train %>%
  # Create new column of predictions:
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  # Compute score:
  summarize(Score = mean(Survived == Survived_predicted))

LOOCVscore

#===========================================#

#K-FOLD CV:

n <- nrow(train)
k <- 5
train_shuf <- sample_frac(train,1,replace=TRUE) #first shuffle the train data
total_kFold = 0 #variable to store the total of the mse in each fold. we will each mse to this variable later
start = 1 #start value for the fold for each time

for(i in 1:k){
  pseudo_train <- train_shuf %>% 
    slice(start : (start + n/k))
  
  pseudo_test <- train_shuf %>% 
    anti_join(pseudo_train, by="PassengerId")
 
  total_kFold <- total_kFold + pseudo_test %>% 
    # Create new column of predictions:
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
    # Compute score:
    summarize(Score = mean(Survived == Survived_predicted))
  
  start <- start + n/k + 1 #update the start value
  
}

kFoldScore = total_kFold/k
kFoldScore

#===============================#

#If kaggle reported our score based on entire test data, then we could do first find models that give
#us good pseudo scores, and then submit results based on those models to get a good score on the leaderboard.
#We could then keep improving our models till we got a 100% on the leaderboard, which would mean we had a 100% final score.
#This doesn't work with the current model because a 100% score on the leaderboard doesn't necessarily mean a 100% final score.
