library(tidyverse)
library(broom)
library(MASS)
library(ISLR)

#name: Xiaoli Jin 

#k-folds
train <- train %>%
mutate(folds = sample(1:5, replace = TRUE, size = nrow(train)))

score_sum <- vector(length=0)

for (i in 1:5) {
    
    pseudo_train <- filter(train, folds != i)
    pseudo_test <- filter(train, folds == i)
    
    # model <- FitModel(input = pseudo_train) # Example regression
    # pseudo_test <- pseudo_test %>%
    #   mutate(Prediction = predict(model, newdata = pseudo_test))
    
    FitModel <- function(x){
        ifelse(x=="female", 1, 0)
    }
    pseudo_test <- pseudo_test %>%
    mutate(Prediction = FitModel(Sex)) %>%
    dplyr::select(Survived, Prediction)
    
    score_sum <- c(score_sum,mean(pseudo_test$Survived == pseudo_test$Prediction))
    
}

mean(score_sum)


#leave_one_out
score_sum2 <- vector(length=0)

for (i in 1:891) {
    
    pseudo_train <- filter(train, PassengerId!= i)
    pseudo_test <- filter(train, PassengerId == i)
    
    # model <- FitModel(input = pseudo_train) # Example regression
    # pseudo_test <- pseudo_test %>%
    #   mutate(Prediction = predict(model, newdata = pseudo_test))
    
    FitModel <- function(x){
        ifelse(x=="female", 1, 0)
    }
    pseudo_test <- pseudo_test %>%
    mutate(Prediction = FitModel(Sex)) %>%
    dplyr::select(Survived, Prediction)
    
    score_sum2 <- c(score_sum2,ifelse(pseudo_test$Survived == pseudo_test$Prediction, 1, 0))
    
}

mean(score_sum2)


#Answer to the question:
#To get a full score, I will perform a regression using all of the variables (I tested it in R, and it did give me a MSE = 0)
  
