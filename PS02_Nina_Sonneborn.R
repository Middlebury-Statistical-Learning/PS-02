# Problem Set 02: Cross Validation with Titanic Data
# Nina Sonneborn
# MATH 218
#
# Kaggle competetition Titanic (https://www.kaggle.com/c/titanic/data)
# 
# Using the train.csv data from the Kaggle Titanic competititon and for the gender survival model, 
# compute what I called the “pseudo-scores”: estimates of the Kaggle “score” of 76.555%. 
# Do this via both leave-one-out cross-validation and
# k=5 Fold cross-validation.

# Load relevant libraries, the training data 

library(tidyverse)
setwd("~/Documents/Statistical Learning/Titanic")
titanic <- read_csv('train.csv')

#---------------------------------------------------------------------------
# LOOCV

# Since the model is not really being "trained," it isn't necessary to go through removing
# each value, psuedo-training to get a psuedo score.
# We can get predicted values for each row and then get psuedo score.

titanic %>% 
  mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>%
  summarise(pseudo_score = mean(survived_predicted == Survived))
# This returns psuedo-score of 61.62%

#---------------------------------------------------------------------------
# k-fold CV with k=5

# 891/5 = 178 passengers in each pseudo_test set
# For this random resampling, I am assuming rows are already randomly ordered.
# Thus, each fold will be representative when I slice by row number

# Note: This would be an efficient way to execute this test. However, there is a bug in
# my for loop that I can't figure out right now, so I did it a different way.

# Set up blank vector to keep score for each fold
# scores <- rep(0, times = 5)
# For each fold
# for(i in 1:4){
#   # Do your thing, do your thing, do your thing, to compute score_for_fold_i
#   pseudo_test <- titanic %>% slice((178*(i-1) + 1): 178*i + 1)
#   pseudo_train <- titanic %>% anti_join(pseudo_test, by="PassengerId")
#   score_for_fold_i <- pseudo_test %>% 
#     mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>%
#     summarise(score = mean(survived_predicted == Survived, na.rm=TRUE))
#   # Save the score corresponding to the fold in scores
#   scores[i] <- score_for_fold_i$score
# }

# Initialize score vector
scores <- rep(0, times = 5)

score_for_fold_1 <- titanic %>% 
  slice(1:178) %>% # this creates pseudo-test. Pseudo-train is unnecessary for this model.
  mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>% 
  summarise(score = mean(survived_predicted == Survived, na.rm=TRUE)) # this is pseudo score
scores[1] <- score_for_fold_1$score

# Below this is repeated for k=2, k=3, k=4, k=5....

score_for_fold_2 <- titanic %>% 
  slice(179:356) %>% 
  mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>%
  summarise(score = mean(survived_predicted == Survived, na.rm=TRUE))
scores[2] <- score_for_fold_2$score

score_for_fold_3 <- titanic %>% 
  slice(357:534) %>% 
  mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>%
  summarise(score = mean(survived_predicted == Survived, na.rm=TRUE))
scores[3] <- score_for_fold_3$score

score_for_fold_4 <- titanic %>% 
  slice(534:712) %>% 
  mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>%
  summarise(score = mean(survived_predicted == Survived, na.rm=TRUE))
scores[4] <- score_for_fold_4$score

score_for_fold_5 <- titanic %>% 
  slice(713:891) %>% 
  mutate(survived_predicted = ifelse(Sex == "Female", 1, 0)) %>%
  summarise(score = mean(survived_predicted == Survived, na.rm=TRUE))
scores[5] <- score_for_fold_5$score

# Finally, take the mean of the scores
mean(scores)


# ------------------------------------------------------------------
# We saw that Kaggle takes the test data (418 rows), only reports your score on 
# the leaderboard based on half of these data, and declares the winner based on 
# the other half which is withholded until the very end of the competition. 
# Not only that, Kaggle does not tell you how they split the 418 rows. 
# Say Kaggle didn’t do this and reported your score on the leaderboard based
# on the entire test data (418 rows). Write 2-3 sentences outlining a strategy 
# of how you could exploit the information given by the leaderboard to get a perfect 
# score of 100%.

# You could overfit your data to the test set, using a high-degree polynomial model that
# isn't generalized but works well on those specific 418 rows. Since you can send multiple 
# submissions per day, you could send, recieve score, tamper with the model, and repeat until
# your score was perfect.


