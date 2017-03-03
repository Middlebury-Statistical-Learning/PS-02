################  5-fold  ##################################
# Load packages:
library(tidyverse)
library(MASS)

library(broom)

#shuffle data
n <- nrow(train)
shuffledTrain <- train[sample(n),]

# Set up blank vector to keep score for each fold
scores5Fold <- rep(0, times = 5)

for (i in 1:5) {
  indices <- ((i-1) * (1/5)*nrow(shuffledTrain) + 1):(i*(1/5) * nrow(shuffledTrain))
  
  pseudo_test5Fold <- shuffledTrain[indices,]
  
  testWithPredictions5Fold <-  mutate(pseudo_test5Fold, Survived_predicted = ifelse(Sex == 'female', 1, 0))
    
  scores5Fold[i] = summarize(testWithPredictions5Fold, Score = mean(Survived == Survived_predicted))
}

avg5Fold <- scores5Fold %>% unlist() %>% mean()

  

print( avg5Fold )


################  LOOCV  ####################################

#shuffle data
n <- nrow(train)
shuffledTrain <- train[sample(n),]

# Set up blank vector to keep score for each fold
scoresLOOCV <- rep(0, times = n)

for (i in 1:n) {
  indices <- ((i-1) * (1/n)*nrow(shuffledTrain) + 1):(i*(1/n) * nrow(shuffledTrain))
  
  pseudo_testLOOCV <- shuffledTrain[indices,]
  
  testWithPredictionsLOOCV <-  mutate(pseudo_testLOOCV, Survived_predicted = ifelse(Sex == 'female', 1, 0))
  
  scoresLOOCV[i] = summarize(testWithPredictionsLOOCV, Score = mean(Survived == Survived_predicted))
}

avgLOOCV <- scoresLOOCV %>% unlist() %>% mean()

print( avgLOOCV )

# The reason that Kaggle only grades you on half of their test data for the leaderboard score
# is because you could fit your data for the test data due to their lenient restrictions on how many submissions you can make.
# The way you could game the leaderboard is by overfitting.  You could keep submitting your predictions
# with different algorithms, gradually improving and improving your score based on the sample in the test set.
# If you get to 95% you can get to 96% just by singling out those 3 or 4 people you are missing and basically hard coding the algorithm
# to get those ones correct.
