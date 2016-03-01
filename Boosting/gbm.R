# gradient boosting machine
# loads the needed packages and defines a function that takes the
# training and test data sets and returns a prediction for the test data set
# from the gradient boosting machine.

# variable selection is done by the Lasso or by specifying varNames.
# full = TRUE means that all predictor variables are used.

# GBMCI-implementation of gbm is needed for 'distribution = "sci"' in the gbm command
  # run devtools::install_github("uci-cbcl/GBMCI") to install it


library(glmnet)
library(gbm)

gradBM <- function(train, test, varNames = NULL, ntrees = 1000, 
                   shrinkage = 1, verbose = FALSE, full = FALSE){
  
  if(full){
    
    varNames <- setdiff(names(train), c("LKADT_P", "DEATH"))
    l.table <- apply(train[ ,varNames], 2, function(x) length(table(x)))
    varNames <- varNames[l.table > 1]
    
  } else {
    
    if(is.null(varNames)) {
      # variable selection using the Lasso
      predVar <- setdiff(names(train), c("LKADT_P", "DEATH"))
      
      # remove predictors that are constant
      l.table <- apply(train[ ,predVar], 2, function(x) length(table(x)))
      predVar <- predVar[l.table > 1]
      
      cox.mm <- model.matrix(~ . - 1, data = train[, predVar])
      cox.cv <- cv.glmnet(cox.mm, Surv(train$LKADT_P, train$DEATH),
                          family = "cox")
      
      coefs <- coef(cox.cv, s = "lambda.min")
      act.index <- which(coefs != 0)
      
      # select the active variables to be fitted in the gbmsci
      var.index <- unique(attributes(cox.mm)$assign[act.index])
      varNames <- predVar[var.index]
    } else {
      
      # remove predictors that are constant
      l.table <- apply(train[ ,varNames], 2, function(x) length(table(x)))
      varNames <- varNames[l.table > 1]
      
    }
  }
  
  
  gbm.form <- as.formula(paste("Surv(LKADT_P, DEATH) ~", 
                               paste(varNames, collapse = " + "), 
                               collapse = ""))
  
  
  gbm.o <- gbm(gbm.form, data = train,
               weights = rep(1, nrow(train)),
               var.monotone = rep(0, length(varNames)),
               distribution = "sci",
               n.trees = ntrees,
               shrinkage = shrinkage,
               interaction.depth = 3,
               bag.fraction = 0.5,
               train.fraction = 1,
               cv.folds = 5,
               n.minobsinnode = 10,
               keep.data = TRUE,
               verbose = verbose)
  
  best.iter <- gbm.perf(gbm.o, plot.it = FALSE, method = "cv")
  - predict(gbm.o, test[, varNames], best.iter)
}

