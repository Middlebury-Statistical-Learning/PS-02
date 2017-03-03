library(tidyverse)
library(MASS)
library(ISLR)
library(broom)

train <- readr::read_csv("/Users/Tina/MATH218/train.csv")

#kfold

train <- readr::read_csv("/Users/Tina/MATH218/train.csv") %>% 
  dplyr::select(PassengerId, Survived, Sex) %>% 
  mutate(fold=sample(1:5, replace=TRUE, size=nrow(train)))

# Set up blank vector to keep score for each fold
scores_kfold <- rep(0, times = 5)

for(i in 1:5){
  
  pseudo_train <- train %>% 
    filter(fold != i)           #1/5
  pseudo_test <- train %>% 
    filter(fold == i)           #4/5
  
  score_for_fold_i <- pseudo_test %>% 
    mutate(Predictions= ifelse(Sex == "female", 1, 0)) %>% # .resid^2, would I have to summarize?
    mutate(Correct=Predictions == Survived) %>% 
    summarize(score=mean(Correct)) 
  
  scores_kfold[i] <- score_for_fold_i$score
}

mean(scores_kfold)


#loocv

train <- readr::read_csv("/Users/Tina/MATH218/train.csv") %>% 
  dplyr::select(PassengerId, Survived, Sex)  

# Set up blank vector to keep score for each fold
scores_loocv <- rep(0, times = nrow(train))

for(i in 1:nrow(train)){
  
  pseudo_train <- train %>% 
    filter(PassengerId != i)           #1/5
  pseudo_test <- train %>% 
    filter(PassengerId == i)           #4/5
  
  score_for_fold_loocv <- pseudo_test %>% 
    mutate(Predictions= ifelse(Sex == "female", 1, 0)) %>% 
    mutate(Correct = ifelse(Predictions == Survived, 1,0)) 

      scores_loocv[i] <- score_for_fold_loocv$Correct
}

mean(scores_loocv)



"You would be able to exploit the score by performing a fit model 
lm(parameter~, data = train) and for the parameter, include
all of the variablles in the test data to make the score to 0."
