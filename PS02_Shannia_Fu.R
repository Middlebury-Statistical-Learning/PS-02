library(tidyverse)
train <- readr::read_csv("train.csv")
loo_score <- c()
for(i in 1:nrow(train)) {
  new_train <- train %>% 
    filter(PassengerId != i) %>% 
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
    summarize(Score = mean(Survived == Survived_predicted))
  loo_score <- c(loo_score, as.numeric(new_train))
}
loo_score
mean(loo_score)

set.seed(100)
k_train <- train [sample(1:nrow(train)), ]

k1_test <- anti_join(k_train, slice(k_train, 1:(nrow(k_train)/5)))
k2_test <- anti_join(k_train, slice(k_train, (nrow(k_train)/5):(2*(nrow(k_train)/5))))
k3_test <- anti_join(k_train, slice(k_train, (2*(nrow(k_train)/5)):(3*(nrow(k_train)/5))))
k4_test <- anti_join(k_train, slice(k_train, (3*(nrow(k_train)/5)):(4*(nrow(k_train)/5))))
k5_test <- anti_join(k_train, slice(k_train, (4*(nrow(k_train)/5)):(5*(nrow(k_train)/5))))

k_score <- c()
for(i in 1:5) {
  name <- paste("k", i, "_test", sep = "")
  for(j in 1:nrow(eval(as.name(name))))
    new_train <- eval(as.name(name)) %>% 
      mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
      summarize(Score = mean(Survived == Survived_predicted))
    k_score <- c(k_score, as.numeric(new_train))
}
k_score
mean(k_score)

#We saw that Kaggle takes the test data (418 rows), only reports your score
#on the leaderboard based on half of these data, and declares the winner based
#on the other half which is withholded until the very end of the competition.
#Not only that, Kaggle does not tell you how they split the 418 rows.
#Say Kaggle didn’t do this and reported your score on the leaderboard based on
#the entire test data (418 rows). Write 2-3 sentences outlining a strategy of
#how you could exploit the information given by the leaderboard to get a perfect
#score of 100%.

#You can submit any permutation of 1s and 0s (survived/died) until your score
#is 1.00000. If they let out all the scores without randomizing, at some point
#one of your 891^2 permutations will match all of the correct outcomes.
