## Required packages
library(survival)
library(glmnet)
library(ggplot2)

## Stability selection using glmnet for lasso penalized Cox regression
## The function is hardcoded to using the Prostate response variables
## train: the training data as a data frame
## B: number of subsamples
## q: fraction of data points per subsample 
##
## returns a dataframe with variable names and selection frequencies
 
stabSelect <- function(train, B = 100, q = 1/2, trace = FALSE) {

predVar <- setdiff(colnames(train), c("LKADT_P", "DEATH"))  
XX <- model.matrix(~ . - 1, train[, predVar])
YY <- Surv(train$LKADT_P, train$DEATH)

n <- nrow(XX)
p <- length(predVar)
select <- matrix(0, p, B)
rownames(select) <- predVar

for (b in seq_len(B)) {
  ii <- sample(n, q * n)
  survNet <- cv.glmnet(XX[ii, ], YY[ii, ], family = "cox")
  betahat <- coef(survNet, s = "lambda.min") 
  ind <- as.logical(betahat != 0)
  select[unique(attributes(XX)$assign[ind]), b] <- 1
  if(trace) 
    cat("Iter:", b, "  Nr. selected:", sum(select[, b]), "\n")
}

selectFreq <- rowMeans(select)
selectSort <- sort(selectFreq, decreasing = TRUE)
varSort <- factor(names(selectFreq), levels = names(selectSort))  
structure(
  data.frame(
    freq = selectFreq,
    var = varSort
  ), class = c("stabSelect", "data.frame"))
}

## Plot method for objects of class stabSelect

plot.stabSelect <- function(x, n = 20, ...) {
  ind <- order(x$freq, decreasing = TRUE)[seq_len(n)]
  qplot(var, freq, data = x[ind, ]) + 
    theme(axis.text.x = element_text(angle = -90)) + 
    scale_y_continuous("Selection proportion") +
    scale_x_discrete("Variable")  
}

## Helper functions for fitting just lasso and debiased lasso

cox <- function(train, test, varNames) {
  survForm <- as.formula(paste("Surv(LKADT_P, DEATH) ~", 
                               paste(varNames, collapse = " + "), 
                               collapse = ""))
  coxModel <- coxph(survForm, train)
  predict(coxModel, newdata = test)
}

lasso <- function(train, test, debiased = FALSE) {
  predVar <- setdiff(colnames(train), c("LKADT_P", "DEATH"))  
  XX <- model.matrix(~ . - 1, train[, predVar])
  YY <- Surv(train$LKADT_P, train$DEATH)
  survNet <- cv.glmnet(XX, YY, family = "cox")
  
  coefs <- coefficients(survNet, s = "lambda.min") 
  act.index <- which(coefs != 0)
  var.index <- unique(attributes(XX)$assign[act.index])
  varNames <- predVar[var.index]
  
  if (debiased) {
    predicted <- cox(train, test, varNames = varNames)
  } else {
    tt <- test[complete.cases(test[, varNames]), ]
    ind <- which(names(tt) %in% c("DEATH", "LKADT_P"))
    tt <- tt[,-ind]
    for (var in setdiff(names(tt), c("DEATH", "LKADT_P", varNames))){
      tt[var][is.na(tt[var])] <- tt[var][!is.na(tt[var])][1]
    }
    XXpred <- model.matrix(~ . - 1, tt)
    predicted <- rep(NA, nrow(test))
    predicted[complete.cases(test[, varNames])] <- predict(survNet, 
                                                            newx = XXpred, s = "lambda.min")
  }
  predicted
}







