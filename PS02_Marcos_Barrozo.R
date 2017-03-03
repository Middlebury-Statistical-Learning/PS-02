## HW2 - Cross-Validation
# Marcos Barrozo
library(tidyverse)

datapath <- "D:\\Users\\users\\OneDrive - Middlebury College\\Stat Learning\\Lec03\\data"
setwd(datapath)

train <- readr::read_csv("train.csv")


### PART I - LEAVE ONE OUT ###

# With this model, I could just summarize the mean(Survived == Survived_predicted)
# and that would be the same result, but I want to emulate the sampling process
# for future uses in cross validation

#Shuffle the observations, and then number them
train_shuffle <- train[sample(nrow(train)),] %>%
  mutate(n= row_number())
score <- train_shuffle %>% 
  filter(n==1) %>%
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  summarize(Score = mean(Survived == Survived_predicted)) #generate a score for the first

#Now a loop for the others
for (k in 2:nrow(train_shuffle)) {
  indvscore <- train_shuffle %>%
    filter(n==k) %>%
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
    summarize(Score = mean(Survived == Survived_predicted))
  
  score <- bind_rows(score, indvscore) #append current score with those already calculated
}

#Now compute avg score, which should be the same as if I weren't sampling one by one
score %>% summarize(avgscore=mean(Score))
# final score == .787565


####### PART II - 5-FOLD ##########

# Creating the k=5 subsets
n_folds <- 5
train_1 <- train %>% 
  sample_frac(1/n_folds)
remainder <- train %>% 
  anti_join(train_1, by="PassengerId")
train_2 <- remainder %>%
  sample_frac(1/(n_folds-1))
remainder <- anti_join(remainder, train_2)
train_3 <- remainder %>%
  sample_frac(1/(n_folds-2)) 
remainder <- anti_join(remainder, train_3)
train_4 <- remainder %>%
  sample_frac(1/(n_folds-3)) 
remainder <- anti_join(remainder, train_4)
train_5 <- remainder %>%
  sample_frac(1/(n_folds-4))

#Score'em  ----- HOW CAN I LOOP THROUGH A LIST of dfs?
score_1 <- train_1 %>%
  # Create new column of predictions:
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  # Compute score:
  summarize(Score = mean(Survived == Survived_predicted)) 
score_2 <- train_2 %>%
  # Create new column of predictions:
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  # Compute score:
  summarize(Score = mean(Survived == Survived_predicted)) 
score_3 <- train_3 %>%
  # Create new column of predictions:
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  # Compute score:
  summarize(Score = mean(Survived == Survived_predicted)) 
score_4 <- train_4 %>%
  # Create new column of predictions:
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  # Compute score:
  summarize(Score = mean(Survived == Survived_predicted)) 
score_5 <- train_5 %>%
  # Create new column of predictions:
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  
  # Compute score:
  summarize(Score = mean(Survived == Survived_predicted)) 
score_5fold <- bind_rows(score_1,score_2,score_3,score_4,score_5) %>%
  summarize(Score = mean(Score))



#Trying the process above with a loop (doesn't work))
#having trouble manipulating df name within loop

k_fold <- train %>% 
  mutate(fold = sample(1:5, replace=TRUE, size=nrow(train)))

#n_folds <- 5
#remainder <- train
#for (k in 1:n_folds) {
#  train_k <- remainder %>% 
#    sample_frac(1/n_folds+1-k)
#  remainder <- anti_join(remainder, train_k)
#}


##### PART III - HOW TO GET A PERFECT SCORE ######
# I would start with a column of 0s. Then, I would change the first value from 0 to 1.
# If my kaggle score goes up, I'll keep the 1 for that value; 
# if it goes down I'll change it back to 0
# repeat the above process for all 418 observations.
