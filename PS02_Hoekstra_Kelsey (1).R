library(tidyverse)
library(dplyr)
#import data
train<- train_1_



#leave one out CV 


#set up data set
pseudo_test <- train

#need to return the data to pseudo_test otherwise the mutate doesn't impact it
 pseudo_test<- pseudo_test %>%
  
    mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>%
    mutate(Score = ifelse(Survived == Survived_predicted, 1, 0)) 

#after that's all done I take the average of the score
 #for some reason summarize doesn't cooperate
mean(pseudo_test$Score)


#K fold
  
#randomize rows
base<- train[sample(nrow(train)),]
#split into k=5 groups
#all about equal sized ... is there a more efficient way of doing this

g1<- slice(base, 1:178) %>% mutate(Group = 1)
g2 <- slice(base, 179:356) %>% mutate(Group = 2)
g3<- slice(base, 357:535) %>% mutate(Group = 3)
g4<- slice(base, 536:713) %>% mutate(Group = 4)
g5<- slice(base, 714:891) %>% mutate(Group =5 )
base_test <- bind_rows(g1, g2, g3, g4, g5)
#4/5 are train, 1/5 is test
#repeat

base_test %>% 
  group_by(Group) %>% 
  mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>%
  summarize(Score = mean(Survived == Survived_predicted))

#Say Kaggle didn’t do this 
#and reported your score on the leaderboard based on the entire test data (418 rows). 
#Write 2-3 sentences outlining a strategy of how you 
#could exploit the information given by the leaderboard to get a perfect score of 100%.

#ANSWER: I would try to overfit the model. You get 10 submissions a day- so I would use a combination of 
#cross validation and submissions to make sure my model works for this specific data set.
#I would not need to worry about generalizing this model because I know exactly how it works on this specific data set
