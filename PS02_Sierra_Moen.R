library(dplyr)
library(train)
library(plyr)

p2 <- pseudo_train %>% mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  summarize(Score = mean(Survived == Survived_predicted))

p3 <- sample_n(train, 890, replace = FALSE)
p4 <- p3 %>% mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) %>% 
  summarize(Score = mean(Survived == Survived_predicted))


k9 <- pseudo_train %>% mutate(Survived_predicted = ifelse(Sex == "female", 1, 0)) 
k1 <- sample_n(k9, 713, replace = FALSE)
k2 <- dplyr::slice(k1, 1:179) %>% summarise(mean = mean(Survived))
k5 <- dplyr::slice(k1, 180:358) %>% summarise(mean = mean(Survived))
 

# I am sorry but I don't know :( I thought about it for a long time but it's been a really tough week. I'll do better next time!)







