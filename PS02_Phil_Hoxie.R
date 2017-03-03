library(broom)
library(ISLR)
library(tidyverse)
library(MASS)

## Data
train <- read.csv("train.csv", header=TRUE) %>% tbl_df()

## Model

train <- train %>% 
  mutate(Survived_hat = ifelse(Sex == "female", 1, 0))

## Cross Validate

## K-Fold (K=5)

## creat the test sets
test_1 <- sample_frac(train, 1/5)
train_1 <- anti_join(train, test_1)

test_2 <- sample_frac(train_1, 1/4)
temp_1 <- anti_join(train_1, test_2)

test_3 <- sample_frac(temp_1, 1/3)
temp_2 <- anti_join(temp_1, test_3)

test_4 <- sample_frac(temp_2, 1/2)
test_5 <- anti_join(temp_2, test_4)

## Fill in the train sets

train_2 <- anti_join(train, test_2)
train_3 <- anti_join(train, test_3)
train_4 <- anti_join(train, test_4)
train_5 <- anti_join(train, test_5)

# now we have 5 train and test sets

## Compute Scores

score_1 <- summarize(test_1, Score = mean(Survived == Survived_hat))
score_2 <- summarize(test_2, Score = mean(Survived == Survived_hat))
score_3 <- summarize(test_3, Score = mean(Survived == Survived_hat))
score_4 <- summarize(test_4, Score = mean(Survived == Survived_hat))
score_5 <- summarize(test_5, Score = mean(Survived == Survived_hat))

## total score (average)

K_fold_score <- (score_1+score_2+score_3+score_4+score_5)/5
K_fold_score

## LOOCV

# this would in theorey be the same as applying the model 
# to the whole data set

LOOCV_score <- summarize(train, Score = mean(Survived == Survived_hat))
LOOCV_score

## Questions ##

# If Kaggle used the entire test set for the data then we
# would be able to keep submitting models in order to over-fit 
# a model to the test set. This would effectively reveal
# the outcome variable for the test set. 
# 