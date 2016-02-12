## Required packages
library(mgcv)

gamPred <- function(train, test, varNames, conVar, disVar) {
  conVarNames <- intersect(conVar, varNames)
  disVarNames <- intersect(disVar, varNames)
  gamForm <- as.formula(
    paste("LKADT_P ~", 
          paste(paste("s(", conVarNames, ")", collapse = "+"), 
                paste(disVarNames, collapse = " + "), 
                sep = "+"), 
          collapse = "")
  )
  
  survGam <- gam(gamForm, data = train, family = cox.ph(), weight = DEATH)
  predict(survGam, newdata = test)
}