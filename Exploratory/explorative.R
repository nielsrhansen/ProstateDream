library(lattice)
library(RColorBrewer)

# Fraction of missing observations
frac <- 0.5
missings <- names(training[apply(training, 2, function(x) sum(is.na(x))) > 1600*frac]) 
subTable <- subset(training, select = names(training)[!(names(training) %in% missings)])
disVar <- disVarCT[disVarCT %in% names(subTable)][-c(1, 2, 4, 5)]
conVar <- conVarCT[conVarCT %in% names(subTable)][-1]
# Levels of variables
varLevels <- sapply(training, function(x) length(unique(x)))
twoLevels <- names(varLevels[varLevels == 2])
# Level plot
csubTable <- subTable[, intersect(twoLevels, disVar)]
cp <- cor(data.matrix(csubTable))
ord <- rev(hclust(as.dist(1-abs(cp)))$order)
colPal <- col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", 
                                    "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                                    "#4393C3", "#2166AC", "#053061"))(200)

postscript(file = "corrplotDis.eps", width = 14, height = 14,
           onefile = FALSE, horizontal = FALSE, paper = "special")
print(
  levelplot(cp[ord,ord], xlab = "", ylab = "", col.regions = colPal,
            at = seq(-1, 1, length.out = 100), aspect="fill",
            colorkey = list(space = "top"),
            scales = list(x = list(rot = 90)))
)
dev.off()

postscript(file = "corrplotDisSmall.eps", width = 5, height = 5,
           onefile = FALSE, horizontal = FALSE, paper = "special")
print(
  levelplot(cp[ord,ord], xlab = "", ylab = "", col.regions = colPal,
            at = seq(-1, 1, length.out = 100), aspect="fill",
            colorkey = list(space = "top"),
            scales = list(x = list(draw = FALSE), y = list(draw = FALSE)))
)
dev.off()

# Scatter plot matrix
csubTable <- subTable[, conVar]
cp <- cor(data.matrix(csubTable), use = "pairwise.complete.obs")
ord <- rev(hclust(as.dist(1-abs(cp)), method = "average")$order)

postscript(file = "corrplotCont.eps", width = 10, height = 10,
           onefile = FALSE, horizontal = FALSE, paper = "special")
print(
  splom(csubTable[, ord], 
        upper.panel = panel.splom,
        pscale = 0, 
        varname.cex = .8,
        nbins = 15, 
        xlab = NULL, 
        lwd = 2, 
        lower.panel = function(x, y, ...) {
          panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y, use = "pairwise.complete.obs") * 4 + 5)])
          cpl <- current.panel.limits()
          panel.text(mean(range(x, na.rm = TRUE)), mean(range(y, na.rm = TRUE)), 
                     round(cor(x, y, use = "pairwise.complete.obs"), digits = 2),
                     cex = .8)
        },
        type = "smooth"
  )
)
dev.off()