library(dplyr)

# LOOCV

training <- readr::read_csv("Documents/Math218/train.csv") %>%
  mutate(predict = (Sex == "female"),
         correct = (predict == Survived),
         loo_score = (sum(correct) - correct)/890
         ) %>%
  summarize(score = mean(loo_score))

print(as.numeric(training))

# K Fold CV

train <- readr::read_csv("Documents/Math218/train.csv")
CV_output <- train %>%
  mutate(k = sample(1:5, nrow(train), replace = TRUE),
         predict = (Sex == "female"),
         correct = (predict == Survived)
         ) %>%
  group_by(k) %>%
  summarize(score = mean(correct))

print(mean(CV_output$score))


# Response
# If Kaggle allowed users to see their score on the entire test set then it would be possible
# to effectively train models on the test data, rather than on the training set given. Through 
# trial and error, a user could make a model that fits the test data perfectly and ignores the 
# training data given. 


