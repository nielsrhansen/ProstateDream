# random survival forest
# loads the a required package and defines a function that takes a test and training data set
# and returns the prediction for the test data set.


library(randomForestSRC)

survForest <- function(train, test) {

  rf.form <- as.formula(paste("Surv(LKADT_P, DEATH) ~", 
                              paste(setdiff(names(train), c("LKADT_P", "DEATH")), collapse = " + "), 
                              collapse = ""))
  
  rf.o <- rfsrc(rf.form, data = train,
                mtry = 20, nsplit = 10, nodesize = 6)
  predict(rf.o, newdata = test)$predicted
  
}




  
