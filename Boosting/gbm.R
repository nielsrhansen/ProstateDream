# gradient boosting machine
# loads the needed packages and defines a function that takes the
# training and test data sets and returns a prediction for the test data set
# from the gradient boosting machine.
# Automated variable selection is done
# with a Cox model with LASSO penalty.

# note that one should install the gbm package
  # but not the CRAN version. Instead:
  # use the GBMCI-implementation of gbm 
  # installed from github with the following command
  # install_github("uci-cbcl/GBMCI")
  # where the install_github function is from the devtools package)

library(glmnet)
library(gbm)


gradBM <- function(train, test, ntrees = 1000, shrinkage = 1){
  
  # fit a Cox LASSO
  cox.mm <- model.matrix(~ . - 1, data = 
                           train[,setdiff(names(train), c("LKADT_P", "DEATH"))])
  cox.cv <- cv.glmnet(cox.mm, Surv(train$LKADT_P, train$DEATH),
                      family = "cox")
  
  coefs <- coef(cox.cv, s = "lambda.min")
  act.index <- which(coefs != 0)

  # select the active variables to be fitted in the gbmsci
  var.index <- unique(attributes(cox.mm)$assign[act.index])
  xx.v <- (train[,setdiff(names(train), c("LKADT_P", "DEATH"))])[, var.index]
  
  
  gbm.form <- as.formula(paste("Surv(LKADT_P, DEATH) ~", 
                                 paste(names(xx.v), collapse = " + "), 
                                 collapse = "")) 
  
  
  gbm.o <- gbm(gbm.form, data = train,
               weights = rep(1, nrow(train)),
               var.monotone = rep(0, ncol(xx.v)),
               distribution = "sci",
               n.trees = ntrees,
               shrinkage = shrinkage,
               interaction.depth = 3,
               bag.fraction = 0.5,
               train.fraction = 1,
               cv.folds = 5,
               n.minobsinnode = 10,
               keep.data = TRUE,
               verbose = FALSE)
  
  best.iter <- gbm.perf(gbm.o, method = "cv")
  -predict(gbm.o, test, best.iter)
  
}

