#Rebecca Conover 3/3/17

#LEAVE ONE OUT
score_loo<-c()
for(i in 1:nrow(train)) {
  test_<-train[i,]
  test_<-test_ %>% 
  mutate(Survived_predicted=ifelse(Sex=="female",1,0)) %>% 
  summarize(score=mean(Survived == Survived_predicted))
  
score_loo<-score_loo %>% 
  bind_rows(test_)

}

overall_score_loo<-score_loo %>% 
  summarize(mean(score))

#K-FOLD

score_kf<-c()
shuffled_train<-shuffle(train)
for (i in c(1:5)) {
  if(i!=4){
  range_l<-(i-1)*178+1
  range_u<-(i*178)
  test_<-shuffled_train[range_l:range_u,]
  }
  else{
  range_l<-(i-1)*178+1
  range_u<-(i*178+1)
  test_<-shuffled_train[range_l:range_u,]
  }
  
test_<-test_ %>% 
  mutate(Survived_predicted=ifelse(Sex=="female",1,0)) %>% 
  summarize(score=mean(Survived == Survived_predicted))

score_kf<-score_kf %>% 
  bind_rows(test_)

}  
overall_score_kf<-score_kf %>% 
  summarize(mean(score))
#        
# # We saw that Kaggle takes the test data (418 rows), only reports your 
# score on the leaderboard based on half of these data, and declares the winner
# based on the other half which is withholded until the very end of the competition. 
# Not only that, Kaggle does not tell you how they split the 418 rows. Say Kaggle 
# didn’t do this and reported your score on the leaderboard based on the entire test data 
# (418 rows). Write 2-3 sentences outlining a strategy of how you could exploit the 
# information given by the leaderboard to get a perfect score of 100%.         
# #       
# 
# If all 418 rows of test data were used to compute the score then I could keep tweaking 
# and checking my predictions through resubmission until I got a perfect schoo. Essentially
# if all of the data were used in calculating the score, with enough work I could deduce
# the actual survival data from the test set through changes in scores from submission to submission.
