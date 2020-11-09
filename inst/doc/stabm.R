## ----include=FALSE------------------------------------------------------------
library(stabm)

## -----------------------------------------------------------------------------
listStabilityMeasures()

## -----------------------------------------------------------------------------
feats = list(1:3, 1:4, c(1:3, 5:7))
stabilityJaccard(features = feats)
stabilityNogueira(features = feats, p = 10)

## -----------------------------------------------------------------------------
mat = 0.92 ^ abs(outer(1:10, 1:10, "-"))
set.seed(1)
stabilityIntersectionCount(features = feats, sim.mat = mat, N = 1000)

## ---- fig.width=4.5, fig.height=3, fig.align="center", message=FALSE----------
plotFeatures(feats)

