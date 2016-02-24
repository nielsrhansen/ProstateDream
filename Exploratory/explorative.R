library(lattice)
# Fraction of missing observations
frac <- 0.5
missings <- names(training[apply(training, 2, function(x) sum(is.na(x))) > 1600*frac]) 
subTable <- subset(training, select = names(training)[!(names(training) %in% missings)])
disVarCT <- disVarCT[disVarCT %in% names(subTable)]
conVarCT <- conVarCT[conVarCT %in% names(subTable)]
# Levels of variables
varLevels <- sapply(training, function(x) length(unique(x)))
twoLevels <- names(varLevels[varLevels == 2])
# Level plot
csubTable <- subTable[complete.cases(subTable[, twoLevels]), which(names(subTable) %in% twoLevels)]
cp <- cor(data.matrix(csubTable), method = "spearman")
ord <- rev(hclust(as.dist(1-abs(cp)))$order)
colPal <- colorRampPalette(cbPalette[c(8,7)], space = "rgb")(100)
levelplot(cp[ord[1:33],ord[1:33]], xlab = "", ylab = "", col.regions = colPal,
                    at = seq(-1, 1, length.out = 100), aspect="fill",
                    colorkey = list(space = "top"),
                    scales = list(x = list(rot = 90),
                                  y = list(draw = FALSE)))
# Scatter plot matrix
csubTable <- subTable[complete.cases(subTable[,c("LKADT_P", conVarCT)]),c("LKADT_P", conVarCT)]
splom(csubTable, 
             upper.panel = panel.splom,
             pscale = 0, 
             varname.cex = .3,
             nbins = 15, xlab = NULL,
             lower.panel = function(x, y, ...) {
               panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y) * 4 + 5)])
               cpl <- current.panel.limits()
               panel.text(mean(range(x)), mean(range(y)), 
                          paste('P:',round(cor(x, y), digits = 2),'\n', 'S:', 
                                round(cor(x, y, method='spearman'), digits = 2)),
                          cex = .4)
             },
             type = c("smooth")
)