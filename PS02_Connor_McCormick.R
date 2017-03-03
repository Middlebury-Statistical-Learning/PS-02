title: "MATH 218 Homework 2"
author: "Connor"
library(tidyverse)

train <- readr::read_csv("Titanic/train.csv")

clean <- train %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, Sex, Age, SibSp) %>%
  mutate(Age = as.numeric(Age)) %>%
  mutate(child = Age<14) %>%
  mutate(child = ifelse(is.na(child), "N/A", child)) %>%
  mutate(female=ifelse(Sex=='female', 1, 0))  %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp)

predictsurive <- glm(Survived ~ female + Pclass + child + SibSp, data=clean, family="binomial")


#LOOCV

predictions <- clean %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp) %>%
  mutate(phat = fitted(predictsurive))


#How did the model do?
prophecy <- predictions %>%
  mutate(guess = ifelse(phat>=.5, 1, 0))

### redo variables
errors <- prophecy %>%
  mutate(correct = ifelse(guess==Survived, 1, 0))

loocv <- errors %>%
  mutate(loo = (sum(correct)-correct)/890)

loocv <- loocv %>%
  summarize(prop_right = mean(loo))
kable(loocv)


#k folds cv

#put a disjoin at the beginning of each run to separate fold(test data) from training data

require(dplyr)

set.seed(21)
fold1 <- clean %>% 
  sample_frac(0.2)
fold2 <- clean %>% 
  anti_join(fold1, by="PassengerId") %>%
  sample_frac(.25)
fold3 <- clean %>%
  anti_join(fold1, by="PassengerId") %>%
  anti_join(fold2, by="PassengerId") %>%
  sample_frac(.33) 
fold4 <- clean %>%
  anti_join(fold1, by="PassengerId") %>%
  anti_join(fold2, by="PassengerId") %>%
  anti_join(fold3, by="PassengerId") %>%
  sample_frac(.5) 
fold5 <- clean %>%
  anti_join(fold1, by="PassengerId") %>%
  anti_join(fold2, by="PassengerId") %>%
  anti_join(fold3, by="PassengerId") %>%
  anti_join(fold4, by="PassengerId")

#fold1 
antifold1 <- clean %>%
  anti_join(fold1, by="PassengerId")
predictsurive1 <- glm(Survived ~ female + Pclass + child + SibSp, data=antifold1, family="binomial")
fold1res <- fold1 %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp) %>%
  mutate(phat=predict(predictsurive, newdata = fold1, type = "response"))
testprophecy1 <- fold1res %>%
  mutate(guess = ifelse(phat>=.5, 1, 0))
testerrors1 <- testprophecy1 %>%
  mutate(correct = ifelse(guess==Survived, 1, 0))
testpropright1 <- testerrors1 %>%
  summarize(prop_right=mean(correct))
kable(testpropright1)

#fold2
antifold2 <- clean %>%
  anti_join(fold12, by="PassengerId")
predictsurive2 <- glm(Survived ~ female + Pclass + child + SibSp, data=antifold2, family="binomial")
fold2res <- fold2 %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp) %>%
  mutate(phat=predict(predictsurive, newdata = fold2, type = "response"))
testprophecy2 <- fold2res %>%
  mutate(guess = ifelse(phat>=.5, 1, 0))
testerrors2 <- testprophecy2 %>%
  mutate(correct = ifelse(guess==Survived, 1, 0))
testpropright2 <- testerrors2 %>%
  summarize(prop_right=mean(correct))
kable(testpropright2)

#fold3
antifold3 <- clean %>%
  anti_join(fold3, by="PassengerId")
predictsurive3 <- glm(Survived ~ female + Pclass + child + SibSp, data=antifold3, family="binomial")
fold3res <- fold3 %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp) %>%
  mutate(phat=predict(predictsurive, newdata = fold3, type = "response"))
testprophecy3 <- fold3res %>%
  mutate(guess = ifelse(phat>=.5, 1, 0))
testerrors3 <- testprophecy3 %>%
  mutate(correct = ifelse(guess==Survived, 1, 0))
testpropright3 <- testerrors3 %>%
  summarize(prop_right=mean(correct))
kable(testpropright3)

#fold4
antifold4 <- clean %>%
  anti_join(fold4, by="PassengerId")
predictsurive4 <- glm(Survived ~ female + Pclass + child + SibSp, data=antifold4, family="binomial")
fold4res <- fold4 %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp) %>%
  mutate(phat=predict(predictsurive, newdata = fold4, type = "response"))
testprophecy4 <- fold4res %>%
  mutate(guess = ifelse(phat>=.5, 1, 0))
testerrors4 <- testprophecy4 %>%
  mutate(correct = ifelse(guess==Survived, 1, 0))
testpropright4 <- testerrors4 %>%
  summarize(prop_right=mean(correct))
kable(testpropright4)

#fold5
antifold5 <- clean %>%
  anti_join(fold5, by="PassengerId")
predictsurive5 <- glm(Survived ~ female + Pclass + child + SibSp, data=antifold5, family="binomial")
fold5res <- fold5 %>%
  dplyr::select(PassengerId, Survived, Pclass, Name, female, Age, child, SibSp) %>%
  mutate(phat=predict(predictsurive, newdata = fold5, type = "response"))
testprophecy5 <- fold5res %>%
  mutate(guess = ifelse(phat>=.5, 1, 0))
testerrors5 <- testprophecy5 %>%
  mutate(correct = ifelse(guess==Survived, 1, 0))
testpropright5 <- testerrors5 %>%
  summarize(prop_right=mean(correct))
kable(testpropright5)

meank5 <- testpropright1 %>%
  bind_rows(testpropright2, testpropright3, testpropright4, testpropright5) %>%
  mutate(mean = mean(prop_right))
kable(meank5)




#If Kaggle used all 418 rows from the test set, 
#you could generate a model that identifies specific passengers based on all of the given variables.  
#This model, instead of predicting outcomes, would search for specific passengers and 
#give them an outcome in Survived based on specifics rather than predictors.
