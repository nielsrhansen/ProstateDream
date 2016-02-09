#Function that finds the "num.preds" strongest marginal predictors
#for a given variable ("response") using all variables in the 
#dataset that have at least "min.obs.num" non-missing observations
#and whose names are not included in the "avoid" vector.
predsForm <- function(data=CoreTable, 
                      avoid=NULL,
                      response, alpha=0.05, transform="id",
                      num.preds=1, modeltype="linear",
                      min.obs.num=0,
                      print.mode=FALSE) {
  avoid <- c(response, avoid)
  predictors <- subset(data, select=!(names(data) %in% avoid))
  n.predictors <- ncol(predictors)
  use.names <- c("dropme")
  use.ps <- numeric(0)
  for (i in 1:n.predictors) {
    working.vars <- na.omit(cbind(data[,response],predictors[,i]))
    if (nrow(na.omit(working.vars)) > 0 & 
          length(levels(factor(working.vars[,2]))) > 1 ) {
      if (transform=="id") {
        if (modeltype=="linear") {
          model <- lm(data[,response] ~ predictors[,i])
          testtype <- "F"
        }
        if (modeltype=="binary") {
          model <- glm(data[,response] ~ predictors[,i], 
                       family=binomial)
          testtype <- "LRT"
        }
      }
      if (transform=="log") {
        model <- lm(log(data[,response]+0.1) ~ predictors[,i], 
                    subset=!is.na(data[,response]))
        testtype <- "F"
      }
      p <- anova(model, test=testtype)["predictors[, i]",5]
      if (p <=alpha) {
        use.names <- c(use.names, names(predictors)[i])
        use.ps <- c(use.ps, p)
      }
    }
  }
  use.names <- use.names[-1]
  use.frame <- data.frame(use.names, use.ps)
  use.frame <- use.frame[order(use.frame$use.ps),]
  
  if (length(use.names) <= num.preds) {
    max.preds=length(use.names)
  } else {max.preds <- num.preds}
  
  if (length(use.names) >= 1) {
    use.us <- c("dropme")
    pred.num <- 0
    firstname <- use.frame$use.names[1]
    data.use <- data[, "LKADT_P"] #arbitrary variable with no NAs
    i <- 1
    still.vars.left <- TRUE
    while (pred.num < max.preds & still.vars.left) {
      name <- as.character(use.frame$use.names[i])
      data.use.maybe <- cbind(data.use, data[,name])
      
      if (nrow(na.omit(data.use.maybe)) >= min.obs.num) {
        use.us <- c(use.us,name)
        data.use <- data.use.maybe
        pred.num <- pred.num + 1
      }
      i <- i+1
      if (i == length(use.names)) {
        still.vars.left <- FALSE
      } 
      num.preds <- length(use.us)-1
    }
    use.us <- use.us[-1]
  } else {use.us <- c("1")}
  if (length(use.us)==0) {use.us <- c("1")}
  if (transform=="log") {
    resp.str <- paste("log(", response, "+0.1) ~ ", sep="")
  } else {
    resp.str <- paste(response, "~", " ")
  }
  returnform <- as.formula(paste(resp.str, 
                                 paste(use.us, collapse="+")))  
  returnform
}


#Function that takes a formula object and, if "useResp"=T,
#adds the Nelson-Aalen estimate of the cumulative hazard and 
#the status indicator from the provided "survobject" as 
#predictors, fits a model and returns the variable to be 
#imputed with predicted values from that model in every 
#NA occurence.
MARimp <- function(data, modeltype="linear", 
                   survobject=Surv(CoreTable$LKADT_P, 
                                   CoreTable$DEATH=="YES"),
                   form, transform="id", key="RPT",
                   useResp=T) {
  imp.varname <- as.character(form)[2]
  if (substr(imp.varname, 1,4)=="log(") {
    imp.varname <- substr(imp.varname, 5, nchar(imp.varname)-7)
  }
  
  if (useResp) {
    time <- survobject[, "time"]
    status <- survobject[, "status"]
    data <- cbind(data, time, status)
    #Compute Nelson-Aalen estimate of cumulative hazard
    H.hat <- nelsonaalen(data=data, time=time, status=status)
    data <- cbind(data, H.hat)
    form <- update(form, ~. + status + H.hat)
  }
  #Linear regression 
  if (modeltype=="linear") {
    model <- lm(form, data=data)
    estimates <- predict(model, newdata=data)
    #Log-transformation
    if (transform=="log") {
      returnvar <- exp(estimates)-0.1    
    }
  }
  #Binary regression/classification
  if (modeltype=="binary") {
    data[, imp.varname] <- factor(data[, imp.varname])
    imp.var.levels <- levels(factor(data[,imp.varname]))
    ref.level <- imp.var.levels[1]
    data[,imp.varname] <- relevel(data[, imp.varname], ref=ref.level)
    model <- glm(form, data=data, family=binomial)
    estimates <- predict(model, newdata=data, type="response")
    for (i in 1:length(estimates)) {
      if (!is.na(estimates[i])) {
        if (estimates[i]<0.5) {
          estimates[i] <- ref.level
        } else if (estimates[i]>=0.5) { 
          estimates[i] <- imp.var.levels[2]
        }
      }
    }
    estimates <- factor(estimates)
  }
  is.missing <- data[is.na(data[, imp.varname]), key]
  returnvars <- data[, c(imp.varname, key)]
  returnvars[returnvars[,key] %in% is.missing, 
             imp.varname] <- estimates[returnvars[,key] %in%
                                         is.missing]
  returnvar <- returnvars[, imp.varname]
  returnvar
}


#Function that does MCAR imputation for the "impvar" variable
MCARimp <- function(data, impvar) {
  n <- nrow(data)
  outvar <- data[, impvar]
  impvar_noNA <- outvar[!is.na(outvar)]
  n_noNA <- length(impvar_noNA)
  ind <-  sample(1:n_noNA, n-n_noNA, replace=T)
  outvar[is.na(outvar)] <- impvar_noNA[ind]
  outvar
}

#Wrapper function that performs imputation
imp <- function(data, impvars, classFrame=NULL, alpha=0.05,
                min.obs.num=nrow(data), num.preds=NULL,
                survobject=Surv(CoreTable$LKADT_P,
                                CoreTable$DEATH=="YES"),
                key="RPT", impType, avoid) {
  dataout <- data
  nImp <- length(impvars)
  if (impType == "MCAR") {
    for (i in 1:nImp) {
      outvar <- impvars[i]
      dataout[, outvar] <- MCARimp(data, outvar)
    }
  }
  if (impType == "MARresp" | impType=="MAR") {
    useResp <- F
    if (impType=="MARresp") useResp <- T 
    for (i in 1:nImp) {
      outvar <- impvars[i]
      modeltype <- classFrame[classFrame$names==outvar, "modeltype"]
      if (modeltype=="factor") {
        dataout[, outvar] <- MCARimp(data, outvar)
      } else {
        transform <- classFrame[classFrame$names==outvar, "transform"]
        form <- predsForm(data, avoid=avoid, outvar,
                          alpha, transform, num.preds, 
                          modeltype, min.obs.num)
        dataout[, outvar] <- MARimp(data, modeltype, 
                                    survobject, form,
                                    transform, key,
                                    useResp=useResp)
      }
    }
  }
  dataout
}
