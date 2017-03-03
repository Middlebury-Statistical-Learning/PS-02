install.packages("tidyverse")
library(tidyverse)
setwd("~/Desktop/Middlebury/Stat Learning/Titanic")
train <- read_csv("train.csv") 

train <- train %>% 
  select(PassengerId, Survived, Sex) %>% 
  mutate(Sex=ifelse(Sex=='female', 1, 0))

## LOOCV code 

mean(train$Survived)
## so we know that 0.3838384 proportion of people survived, that is our base proportion 

LOOCV_scores <- rep(0, 891)

for(i in 1:891){ 
  
  pseudo_train <- train %>% 
    filter(!PassengerId==i)
  pseudo_test <- train %>% 
    filter(PassengerId==i)  
  
  predict_survive_model <- glm(Survived ~ Sex, data = pseudo_train, family = "binomial")
  # prediction_pseudo_train <- predict(predict_survive_model, newdata=pseudo_train, type="response")  
  
  pseudo_test <- pseudo_test %>% 
    mutate(phat=predict(predict_survive_model, newdata=pseudo_test, type="response")) %>% 
    select(PassengerId, phat, Sex) %>% 
    mutate(predicted_survived=
             ifelse((phat>=0.3838384), 1, 0)) %>% 
    select(PassengerId, predicted_survived) %>% 
    mutate(predicted_survived=as.integer(predicted_survived))
    
  x <- pseudo_test$predicted_survived[1]
  
  LOOCV_scores[i] <- x
     
}

LOOCV_mean<- mean(LOOCV_scores)

## k fold CV code 1-178, 179-356, 357-534, 535-712, 713-891

      ## FIRST FOLD, i=1-178

pseudo_train1 <- train %>% 
  filter(PassengerId>178) 

pseudo_test1 <- train %>% 
  filter(PassengerId<179)


kfold_score1 <- rep(0, 178)

for(i in 1:178){ 
  
  # i <- 1 
  predict_survive_model1 <- glm(Survived ~ Sex, data = pseudo_train1, family = "binomial")
  # prediction_pseudo_train <- predict(predict_survive_model, newdata=pseudo_train, type="response")  
  
  pseudo_test_loop <- pseudo_test1 
  pseudo_test_loop <- pseudo_test_loop %>% 
    mutate(phat=predict(predict_survive_model1, newdata=pseudo_test_loop, type="response")) %>% 
    select(PassengerId, phat, Sex) %>% 
    mutate(predicted_survived=
             ifelse((phat>=0.3838384), 1, 0)) %>% 
    select(PassengerId, predicted_survived, Sex) %>% 
    mutate(predicted_survived=as.integer(predicted_survived))
  
  x <- pseudo_test_loop$predicted_survived[i]
  
  kfold_score1[i] <- x
  
}
mean1 <- mean(kfold_score1)



      ## SECOND FOLD, i=179-356
pseudo_train2 <- train %>% 
  filter(!(PassengerId>178 & PassengerId<357))

pseudo_test2 <- train %>% 
  filter(PassengerId>178 & PassengerId<357)


kfold_score2 <- rep(0, 178)

for(i in 1:178){ 
  
  # i <- 1 
  predict_survive_model2 <- glm(Survived ~ Sex, data = pseudo_train2, family = "binomial")
  # prediction_pseudo_train <- predict(predict_survive_model, newdata=pseudo_train, type="response")  
  
  pseudo_test_loop <- pseudo_test2 
  pseudo_test_loop <- pseudo_test_loop %>% 
    mutate(phat=predict(predict_survive_model2, newdata=pseudo_test_loop, type="response")) %>% 
    select(PassengerId, phat, Sex) %>% 
    mutate(predicted_survived=
             ifelse((phat>=0.3838384), 1, 0)) %>% 
    select(PassengerId, predicted_survived, Sex) %>% 
    mutate(predicted_survived=as.integer(predicted_survived))
  
  x <- pseudo_test_loop$predicted_survived[i]
  
  kfold_score2[i] <- x
  
}
mean2 <- mean(kfold_score2)

      ## THIRD FOLD, i=357-534
pseudo_train3 <- train %>% 
  filter(!(PassengerId>356 & PassengerId<535))

pseudo_test3 <- train %>% 
  filter(PassengerId>356 & PassengerId<535)


kfold_score3 <- rep(0, 178)

for(i in 1:178){ 
  
  # i <- 1 
  predict_survive_model3 <- glm(Survived ~ Sex, data = pseudo_train3, family = "binomial")
  # prediction_pseudo_train <- predict(predict_survive_model, newdata=pseudo_train, type="response")  
  
  pseudo_test_loop <- pseudo_test3
  pseudo_test_loop <- pseudo_test_loop %>% 
    mutate(phat=predict(predict_survive_model3, newdata=pseudo_test_loop, type="response")) %>% 
    select(PassengerId, phat, Sex) %>% 
    mutate(predicted_survived=
             ifelse((phat>=0.3838384), 1, 0)) %>% 
    select(PassengerId, predicted_survived, Sex) %>% 
    mutate(predicted_survived=as.integer(predicted_survived))
  
  x <- pseudo_test_loop$predicted_survived[i]
  
  kfold_score3[i] <- x
  
}
mean3 <- mean(kfold_score3)

      ## FOURTH FOLD, i=535-712

pseudo_train4 <- train %>% 
  filter(!(PassengerId>534 & PassengerId<713))

pseudo_test4 <- train %>% 
  filter(PassengerId>534 & PassengerId<713)

kfold_score4 <- rep(0, 178)

for(i in 1:178){ 
  
  # i <- 1 
  predict_survive_model4 <- glm(Survived ~ Sex, data = pseudo_train4, family = "binomial")
  # prediction_pseudo_train <- predict(predict_survive_model, newdata=pseudo_train, type="response")  
  
  pseudo_test_loop <- pseudo_test4 
  pseudo_test_loop <- pseudo_test_loop %>% 
    mutate(phat=predict(predict_survive_model4, newdata=pseudo_test_loop, type="response")) %>% 
    select(PassengerId, phat, Sex) %>% 
    mutate(predicted_survived=
             ifelse((phat>=0.3838384), 1, 0)) %>% 
    select(PassengerId, predicted_survived, Sex) %>% 
    mutate(predicted_survived=as.integer(predicted_survived))
  
  x <- pseudo_test_loop$predicted_survived[i]
  
  kfold_score4[i] <- x
  
}
mean4 <- mean(kfold_score4)

      ## FIFTH FOLD, i=713-891

pseudo_train5 <- train %>% 
  filter(PassengerId<713)

pseudo_test5 <- train %>% 
  filter(PassengerId>712)

kfold_score5 <- rep(0, 179)

for(i in 1:179){ 
  
  # i <- 1 
  predict_survive_model5 <- glm(Survived ~ Sex, data = pseudo_train5, family = "binomial")
  # prediction_pseudo_train <- predict(predict_survive_model, newdata=pseudo_train, type="response")  
  
  pseudo_test_loop <- pseudo_test5 
  pseudo_test_loop <- pseudo_test_loop %>% 
    mutate(phat=predict(predict_survive_model5, newdata=pseudo_test_loop, type="response")) %>% 
    select(PassengerId, phat, Sex) %>% 
    mutate(predicted_survived=
             ifelse((phat>=0.3838384), 1, 0)) %>% 
    select(PassengerId, predicted_survived, Sex) %>% 
    mutate(predicted_survived=as.integer(predicted_survived))
  
  x <- pseudo_test_loop$predicted_survived[i]
  
  kfold_score5[i] <- x
  
}
mean5 <- mean(kfold_score5)

kfoldCV_score <- sum(mean1, mean2, mean3, mean4, mean5)/5

## Kaggle Leaderboard Scoring Question: 
## If Kaggle reported scores on the leaderboard given the all the of the test
## data then using you could continuously iterate your own predictions, only
## changing one Passenger's prediction of survival at a time, and mark whether
## your score went up or down. If you score went up then you can "cement" that
## prediction as correct and then move onto the next passenger until you have
## verified all of your predictions. Since Kaggle allows you to submit 10 
## submissions per day, it would only take you 42 days to reach perfect you 
## score to 100% correct. 





scoring <- train %>% 
  select(PassengerId, Survived) %>% 
  mutate(Prediction=0) %>% 
  mutate(Correct=0) 


scoring <- scoring %>% 
  mutate(Prediction = as.integer(Prediction)) %>% 
  mutate(Correct=ifelse(Survived==1&Prediction==1 | Survived==0 & Prediction==0, 1, 0))



train_1 <- train %>% 
  filter(!PassengerId==1)

## Make logistic regression model to predict survival with new training data 
## without PassengerId = n 

predict_survive_model <- glm(Survived ~ Sex, data = train_1, family = "binomial")

prediction_train_1 <- predict(predict_survive_model, newdata=train_1, type="response")  

## Make test data, just one entry line, the one that was left out of training above

## remove everything but the one row that was removed above 
test_1 <- train %>% 
  filter(PassengerId==1)

## fit the model to the test data and make a prediction about survival
test_1 <- test_1 %>% 
  mutate(phat=predict(predict_survive_model, newdata=test_1, type="response")) %>% 
  select(PassengerId, phat, Sex) %>% 
  mutate(predicted_survived=
           ifelse((phat>=0.3838384), 1, 0)) %>% 
  select(PassengerId, predicted_survived)

scoring <- train %>% 
  select(PassengerId, Survived) 

scoring_final <- full_join(scoring, test_1, by="PassengerId") %>% 
  mutate(predicted_survived = as.integer(predicted_survived)) %>% 
  mutate(correct=ifelse(Survived==1&predicted_survived==1 | Survived==0 & predicted_survived==0, 1, 0))


## how to switch predicted_survived from a dbl to an int to then help with my ifelse statement below 

scoring_final <- scoring_final 
## can you create logical statements across data frames in order to create a score
## eg if train=0 & test=0 or train=1 & test=1 then correct=1 in the scoring DF 
## do the above line for each PassengerId=n for n=1:891

