library(tidyverse)
# You may need to change your directory path:
train <- readr::read_csv("titanic/train.csv")
test <- readr::read_csv("titanic/test.csv")

pseudo_train <- train %>% 
  sample_frac(0.8)
pseudo_test <- train %>% 
  anti_join(pseudo_train, by="PassengerId")

loocv_scores=c()
for (i in 1:nrow(train)) {
  ps_train <- filter(train, PassengerId != i)
  ps_test <- filter(train, PassengerId == i)
  ps_test_nc <- mutate(ps_test, Survived_predicted = ifelse(Sex == "female", 1, 0))
  output <- summarize(ps_test_nc, Score = mean(Survived == Survived_predicted))
  loocv_scores <- rbind(loocv_scores, output)
}
score <- summarise(loocv_scores, Score = mean(loocv_scores[["Score"]]))
score

k <- 5
kf_scores <- c()
for(i in c(1:k)){
  si <- as.integer((i-1)/k*nrow(train))+1
  ei <- as.integer(i/k*nrow(train))
  pseudo_test <- train %>% 
    filter(si, ei)
  pseudo_train <- train %>% 
    anti_join(pseudo_test, by="PassengerId")
  
  ps_test_nc <- mutate(pseudo_test, Survived_predicted = ifelse(Sex == "female", 1, 0))
  output <- summarize(ps_test_nc, Score = mean(Survived == Survived_predicted))
  kf_scores <- rbind(kf_scores, output)
}

score <- summarise(kf_scores, Score = mean(kf_scores[["Score"]]))
score

# From a brute force perspective, it could be possible to generate all possible outcomes for the test data. You could then see which set of outcomes gives you a score of 100% and use that.