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
