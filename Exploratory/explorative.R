library(lattice)
library(RColorBrewer)
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
ord <- rev(hclust(as.dist(1-abs(cp)), method = "centroid")$order)
colPal <- colorRampPalette(c("purple", "yellow"), space = "rgb")(100)
levelplot(cp[ord,ord], xlab = "", ylab = "", col.regions = colPal,
                    at = seq(-1, 1, length.out = 100), aspect="fill",
                    colorkey = list(space = "top"),
                    scales = list(x = list(rot = 90),
                                  y = list(draw = FALSE)))
# Scatter plot matrix
# csubTable <- subTable[complete.cases(subTable[,c("LKADT_P", conVarCT)]),c("LKADT_P", conVarCT)]
csubTable <- subTable[, c("LKADT_P", conVarCT)]
cp <- cor(data.matrix(csubTable), use = "pairwise.complete.obs")
ord <- rev(hclust(as.dist(1-abs(cp)), method = "average")$order)
splom(csubTable[, ord], 
             upper.panel = panel.splom,
             pscale = 0, 
             varname.cex = .6,
             nbins = 15, xlab = NULL, lwd = 2, 
             lower.panel = function(x, y, ...) {
               panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y, use = "pairwise.complete.obs") * 4 + 5)])
               cpl <- current.panel.limits()
               panel.text(mean(range(x, na.rm = TRUE)), mean(range(y, na.rm = TRUE)), 
                          round(cor(x, y, use = "pairwise.complete.obs"), digits = 2),
                          cex = .8)
             },
             type = c("smooth")
)