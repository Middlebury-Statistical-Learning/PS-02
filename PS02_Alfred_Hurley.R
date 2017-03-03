# This is a function I wrote to perform cross-validation. 
#tble input in a data set, k is the number of divisions one 
#would want for each cross-validation (k=5 would be the input for k=5 
#cross validation, and k=891 for LOOCV)
CV <- function(tble, k) {
  rows <- nrow(tble)
  scores <- 0
  for (i in 1:k) {
    n <- i*(rows/k)
    pseudo_test <- dplyr::slice(tble, ((i-1)*(rows/k)+1):(i*(rows/k)))
    pseudo_train <- tble %>% 
      anti_join(pseudo_test, by = "PassengerId")
    
    score <- pseudo_test %>%
      mutate(Survived_predicted = ifelse(Sex =="female", 1, 0)) %>%
      summarize(Score = mean(Survived == Survived_predicted))
    scores <- scores + score
  }
  avg <- scores/k
  return(avg)
}

CV(train, 5) #this is for k=5 cross validation
CV(train, 891) #this is for leave on out cross validation


#Answer: If I a good amount of time (and possibly a few accounts), 
#I could submit different answers, each with only one predction saying 
#'survived' and the rest saying 'perished.' By changing only one variable 
#each time and then looking at my score, I could then figure out what each 
#row of the test data is.
