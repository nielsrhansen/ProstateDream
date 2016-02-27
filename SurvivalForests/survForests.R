## Required packages
library(randomForestSRC)

# random survival forest
# loads the a required package and defines a function that takes a test and training data set
# and returns the prediction for the test data set.

survForest <- function(train, test, varNames = NULL) {

  if(is.null(varNames)) {
    ## No variable selection
    varNames <- setdiff(names(train), c("LKADT_P", "DEATH"))
  }
    
  rf.form <- as.formula(paste("Surv(LKADT_P, DEATH) ~", 
                              paste(varNames, collapse = " + "), 
                              collapse = ""))
  
  rf.o <- rfsrc(rf.form, data = train,
                mtry = 20, nsplit = 10, nodesize = 6)
  
  pr <- rep(NA, nrow(test))
  pr[complete.cases(test[,varNames])] <- predict(rf.o, 
                                      newdata = test[complete.cases(test[,varNames]),])$predicted
  pr
  
}