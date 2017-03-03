train <- read.csv("train.csv")

#Our model = M 0, F = 1 
# CV for our model is Leave-One-Out CV & K-fold CV 
# Our score will be a proportion correct (Correct)/Total Number of Guesses

#Leave one out CV 
#Essentially there is no training, there is no model except f=1, m=0

train$Prediction <- ifelse(train$Sex==c("female"), 1, 0)

train$Prediction
#Our prediction is based off of female, give me 1, male give me 0 

#Now I need to tabulate if I was correct in the prediction 

Correct <- filter(train, Prediction==Survived)
#Give me the rows in which the value within prediction matched the value within survived 

Correct$Prediction
#There are 701 observations in Correct, meaning the model predicted correctly 701/891 observations 
Correct$Survived

ScoreLOOCV <- 701/892

#K-fold CV 
#K5 Fold 

train$group <- sample(1:5, replace = TRUE, nrow(train))
#I have identifiers for 5 different groups 

K1 <- filter(train, group==1)

K1Correct <- filter(K1, Prediction==Survived)

#K1 had orginally 180 obs, I filtered such that the rows only remained for when prediction correctly matched survived 

ScoreK1 <- 139/180

K2 <- filter(train, group==2)

K2Correct <- filter(K2, Prediction==Survived)

ScoreK2 <- 131/162

K3 <- filter(train, group==3)

K3Correct <- filter(K3, Prediction==Survived)

ScoreK3 <- 132/179

K4 <- filter(train, group==4)

K4Correct <- filter(K4, Prediction==Survived)

ScoreK4 <- 156/198

K5 <- filter(train, group==5)

K5Correct <- filter(K5, Prediction==Survived)

ScoreK5 <- 143/172

ScoreK <- mean(ScoreK1, ScoreK2, ScoreK3, ScoreK4, ScoreK5)
 