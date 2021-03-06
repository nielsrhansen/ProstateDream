## Required packages
library(randomForestSRC)

# random survival forest
# loads the a required package and defines a function that takes a test and training data set
# and returns the prediction for the test data set.

survForest <- function(train, test, varNames = NULL, save.object = FALSE,
                       file.seed = NULL) {

  if(is.null(varNames)) {
    # no variable selection, all predictors are used for fitting the model
    varNames <- setdiff(names(train), c("LKADT_P", "DEATH"))
  }
    
  rf.form <- as.formula(paste("Surv(LKADT_P, DEATH) ~", 
                              paste(varNames, collapse = " + "), 
                              collapse = ""))
  
  rf.o <- rfsrc(rf.form, data = train,
                mtry = 20, nsplit = 10, 
                nodesize = 6,
                importance = "none")
  
  if (save.object) save(rf.o, file = paste("rsf", substitute(train), file.seed
                                           , ".RData", sep = ""))
  
  predict(rf.o, newdata = test[, varNames])$predicted
}