
#CROSS VALIDATION EXERCISE

library(tidyverse)
full<-readr::read_csv("titanicTrain.csv")

prediction <- full %>%
  mutate(PredictSurvived = ifelse(Sex=="female", 1, 0)) %>% 
  mutate(Right=ifelse(PredictSurvived==Survived,1,0))

size<-nrow(full)

#LOOCV

#WITH FOR LOOP
# scores_LOOCV<-numeric(size)
# for (i in 1:size){
#   validset<-prediction %>% filter(PassengerId!=i)
#   scores_LOOCV[i]<-sum(validset$Right)/(size-1)
# }
# pseudo_score_LOOCV<-mean(scores_LOOCV)

pseudo_score_LOOCV<-sum(prediction$Right)/size

#K-Fold CV
num_fold<-5
fold<-ceiling(nrow(full)/num_fold)
scores_K<-numeric(num_fold)
set.seed(100)
shuffled<-prediction[sample(nrow(prediction)),] 
shuffled$id<-seq.int(nrow(shuffled))
for(i in 1:num_fold){
  validset<-shuffled %>% filter(id>(i*fold)|id<=((i-1)*fold))
  scores_K[i]<-sum(validset$Right)/(nrow(validset))
}

pseudo_score_K<-mean(scores_K)


#SHORT ANSWER QUESTION

# You could submit 419 submissions, where in the first submission you predict
# 'survived' for all passengers, and in the next 418 submissions, you change
# each passenger one at a time to 'did not survive' to observe how the score 
# changes.  For example, for submission #2, you would change the first passenger
# prediction to be 'did not survive', and see if your score goes down or goes
# down.  If it goes down, you would change that passenger's prediction back to
# 'survived'; if the score goes down, you would keep that passenger's prediction
# to be 'did not survive'. Then, you would repeat this for all 418 passengers.

