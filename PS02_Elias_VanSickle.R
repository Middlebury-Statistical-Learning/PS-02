setwd("~/Dropbox/Middlebury/Semesters/9. Spring 2017/Statistical Learning/Assignments/#2")
library(tidyverse)

train <- readr::read_csv("train.csv")

#1 LOOCV
#Full data set broken down into testing and training
#Test data is the one piece
#Train is all the rest
scores_vector_1 <- rep(0,891)
for(i in 1:891){
  pseudo_test <- train[i,] #select one row of training data for pseudo_test
  pseudo_train <- train %>%
    anti_join(pseudo_test, by="PassengerId") #select all rows not in pseudo_test for pseudo_train
  
  score <- pseudo_test %>%
              mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% #create new column with Survived_predicted
              summarize(Score = mean(Survived == Survived_predicted)) #create score by 
        scores_vector_1[i] <- as.integer(score)
}
avg1 <- mean(scores_vector_1) 


#2 Five Fold Cross Validation
#Breaking data into chunks of 1/5(total)
scores_vector_2 <- rep(0,5)
for(i in 1:5){
  index <- i*178
  print(c(index-177," : ",index))
  pseudo_test <- train[index-177:index,]
  pseudo_train <- train %>%
    anti_join(pseudo_test, by="PassengerId")
  
  score <- pseudo_test %>%
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>%
    summarize(Score = mean(Survived == Survived_predicted))
  scores_vector_2[i] <- as.double(score)
}
avg2 <- mean(scores_vector_2)




#STRATEGY TO OBTAIN 100% SCORE ON KAGGLE
#If Kaggle were to test on all 418 rows,
#I would use LOOCV with an added caveate. 
#For each test row i I would compute two average based on
#two different survived predicted values, first 1 and then 0.
#In this way, I would be able to see which gave the better overall result.
#I would then note the true answer for that row and
#repeat the process for each row in the data set.
#By the end I would have determined the true survived value
#for each row and would therefore be able to obtain a score of 100%.

