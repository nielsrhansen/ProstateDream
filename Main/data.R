# DataManipulation
discard <-  c("HGTBLCAT", "WGTBLCAT", "HEAD_AND_NECK", 
              "PANCREAS", "THYROID", "CREACLCA", "CREACL", 
              "GLEAS_DX", "STOMACH")
CoreTable <- subset(CoreTable, select = -which(colnames(CoreTable) %in% discard))

CoreTable <- transform(CoreTable, 
                      PSA = ifelse(PSA == 0, 0.01, PSA),
                      ECOG_C = factor((ECOG_C >= 1) + (ECOG_C >= 2)))

for (i in seq_len(ncol(CoreTable))) {
  if (is.factor(CoreTable[, i])) {
    tmp <- as.character(CoreTable[, i])
    tmp[tmp == ""] <- "No"
    CoreTable[, i] <- factor(tmp)
  }
}

# classFrame
logPreds <- c("CREAT", "LDH", "NEU", "PSA", "WBC", "CREACL", 
              "MG", "BUN", "CCRC")
catPreds <- c("RACE_C", "REGION_C", "ECOG_C", "STUDYID",
              "AGEGRP2", "SMOKE", "SMOKFREQ", "AGEGRP")
binPreds <- names(CoreTable)[50:122]

classFrame <- data.frame(names=names(CoreTable), 
                         modeltype=factor(rep("linear",
                                              ncol(CoreTable)),
                                          levels=c("linear",
                                                   "factor",
                                                   "binary")),
                         transform=factor(rep("id", ncol(CoreTable)),
                                          levels=c("id", "log")))

for (i in 1:length(logPreds)) {
  classFrame[classFrame$names==logPreds[i], "transform"] <- "log"
}
for (i in 1:length(catPreds)) {
  classFrame[classFrame$names==catPreds[i], "modeltype"] <- "factor"
}
for (i in 1:length(binPreds)) {
  classFrame[classFrame$names==binPreds[i], "modeltype"] <- "binary"
}
