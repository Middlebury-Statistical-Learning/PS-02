train <- readr::read_csv("~/Documents/Spring_2017/Statistical_Learning/Titantic/Data/train.csv") %>% 
  dplyr::select(PassengerId, Survived, Sex)

### Leave-One-Out Cross Validation ### 
scores_loo <- rep(0, 891)
for(i in 1:891) { 
  pseudo_test <- train %>% 
    filter(PassengerId == i) 
  
  pseudo_train <- train %>%  
    anti_join(pseudo_test, by="PassengerId")
  
  score_for_i <- pseudo_test %>% 
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
    summarize(score = mean(Survived == Survived_predicted))
  
  scores_loo[i] <- score_for_i$score
}

mean(scores_loo)

### k-Fold Cross Validation ###

# First, assign each observation to one fold # 
train <- train %>% 
  mutate(fold=sample(1:5, replace=TRUE, size=nrow(train)))

scores_kfold <- rep(0, 5)
for(i in 1:5) { 
  pseudo_test <- train %>% 
    filter(fold == i) 
  
  pseudo_train <- train %>%  
    anti_join(pseudo_test, by="fold")
  
  score_for_i <- pseudo_test %>% 
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
    summarize(score = mean(Survived == Survived_predicted))
  
  scores_kfold[i] <- score_for_i$score
}

mean(scores_kfold)
