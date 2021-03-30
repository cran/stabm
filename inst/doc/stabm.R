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

## -----------------------------------------------------------------------------
library(rpart) # for classification trees
data("BostonHousing2", package = "mlbench")

# remove feature that is a version of the target variable
dataset = subset(BostonHousing2, select = -cmedv)

## -----------------------------------------------------------------------------
fit_tree = function(target = "medv", data = dataset, ratio = 0.67, cp = 0.01) {
    n = nrow(data)
    i = sample(n, n * ratio)
    formula = as.formula(paste(target,  "~ ."))
    model = rpart::rpart(formula = formula, data = data, subset = i, 
      control = rpart.control(maxsurrogate = 0, cp = cp))
    names(model$variable.importance)
}

set.seed(1)
fit_tree()

## -----------------------------------------------------------------------------
set.seed(1)
selected_features = replicate(30, fit_tree(), simplify = FALSE)

## -----------------------------------------------------------------------------
# Selected in each repetition:
Reduce(intersect, selected_features)

# Sorted selection frequency across all 30 repetitions:
sort(table(unlist(selected_features)), decreasing = TRUE)

## -----------------------------------------------------------------------------
plotFeatures(selected_features)

## -----------------------------------------------------------------------------
stabilityJaccard(selected_features)

## -----------------------------------------------------------------------------
set.seed(1)
selected_features2 = replicate(30, fit_tree(cp = 0.02), simplify = FALSE)
stabilityJaccard(selected_features2)
plotFeatures(selected_features2)

## -----------------------------------------------------------------------------
dataset2 = subset(BostonHousing2, select = -town)
dataset2$chas = as.numeric(dataset2$chas)

set.seed(1)
selected_features3 = replicate(30, fit_tree(target = "rm", data = dataset2, cp = 0.075), 
  simplify = FALSE)

## -----------------------------------------------------------------------------
# similarity matrix
sim.mat = abs(cor(subset(dataset2, select = -rm)))

sel.feats = unique(unlist(selected_features3))
sim.mat[sel.feats, sel.feats]

## -----------------------------------------------------------------------------
plotFeatures(selected_features3, sim.mat = sim.mat)

## -----------------------------------------------------------------------------
stabilityIntersectionCount(selected_features3, sim.mat = sim.mat, N = 1000)

## -----------------------------------------------------------------------------
no.sim.mat = diag(nrow(sim.mat))
colnames(no.sim.mat) = row.names(no.sim.mat) = colnames(sim.mat)
stabilityIntersectionCount(selected_features3, sim.mat = no.sim.mat)

## -----------------------------------------------------------------------------
set.seed(1)

# select a subset of instances for visualization purposes
inds = sample(nrow(dataset2), 50)
dataset.cluster = dataset2[inds, ]

# run k-means clustering with k = 3 30 times
km = replicate(30, kmeans(dataset.cluster, centers = 3), simplify = FALSE)

# change cluster names for comparability
best = which.min(sapply(km, function(x) x$tot.withinss))
best.centers = km[[best]]$centers
km.clusters = lapply(km, function(kmi) {
  dst = as.matrix(dist(rbind(best.centers, kmi$centers)))[4:6, 1:3]
  rownames(dst) = colnames(dst) = 1:3
  # greedy choice of best matches of clusters
  new.cluster.names = numeric(3)
  while(nrow(dst) > 0) {
    min.dst = which.min(dst)
    row = (min.dst - 1) %% nrow(dst) + 1
    row.o = as.numeric(rownames(dst)[row])
    col = ceiling(min.dst / nrow(dst))
    col.o = as.numeric(colnames(dst)[col])
    new.cluster.names[row.o] = col.o
    dst = dst[-row, -col, drop = FALSE]
  }
  new.cluster.names[kmi$cluster]
})

# for each cluster, create a list containing the instances 
# belonging to this cluster over the 30 repetitions
clusters = lapply(1:3, function(i) {
  lapply(km.clusters, function(kmc) {
    which(kmc == i)
  })
})

## -----------------------------------------------------------------------------
stab.cl = sapply(clusters, stabilityJaccard)
stab.cl

## -----------------------------------------------------------------------------
w = sapply(clusters, function(cl) {
  mean(lengths(cl))
})

sum(stab.cl * w) / sum(w)

