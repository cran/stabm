% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stability_functions_adjusted.R
\encoding{UTF-8}
\name{stabilityIntersectionMBM}
\alias{stabilityIntersectionMBM}
\title{Stability Measure Adjusted Intersection MBM}
\usage{
stabilityIntersectionMBM(
  features,
  sim.mat,
  threshold = 0.9,
  correction.for.chance = "estimate",
  N = 10000,
  impute.na = NULL
)
}
\arguments{
\item{features}{\code{list (length >= 2)}\cr
Chosen features per dataset. Each element of the list contains the features for one dataset.
The features must be given by their names (\code{character}) or indices (\code{integerish}).}

\item{sim.mat}{\code{numeric matrix}\cr
Similarity matrix which contains the similarity structure of all features based on
all datasets. The similarity values must be in the range of [0, 1] where 0 indicates
very low similarity and 1 indicates very high similarity. If the list elements of
\code{features} are integerish vectors, then the feature numbering must correspond to the
ordering of \code{sim.mat}. If the list elements of \code{features} are character
vectors, then \code{sim.mat} must be named and the names of \code{sim.mat} must correspond
to the entries in \code{features}.}

\item{threshold}{\code{numeric(1)}\cr
Threshold for indicating which features are similar and which are not. Two features
are considered as similar, if and only if the corresponding entry of \code{sim.mat} is greater
than or equal to \code{threshold}.}

\item{correction.for.chance}{\code{character(1)}\cr
How should the expected value of the stability score (see Details)
be assessed? Options are "estimate", "exact" and "none".
For "estimate", \code{N} random feature sets of the same sizes as the input feature
sets (\code{features}) are generated.
For "exact", all possible combinations of feature sets of the same
sizes as the input feature sets are used. Computation is only feasible for very
small numbers of features and numbers of considered datasets (\code{length(features)}).
For "none", the transformation \eqn{(score - expected) / (maximum - expected)}
is not conducted, i.e. only \eqn{score} is used.
This is not recommended.}

\item{N}{\code{numeric(1)}\cr
Number of random feature sets to consider. Only relevant if \code{correction.for.chance}
is set to "estimate".}

\item{impute.na}{\code{numeric(1)}\cr
In some scenarios, the stability cannot be assessed based on all feature sets.
E.g. if some of the feature sets are empty, the respective pairwise comparisons yield NA as result.
With which value should these missing values be imputed? \code{NULL} means no imputation.}
}
\value{
\code{numeric(1)} Stability value.
}
\description{
The stability of feature selection is defined as the robustness of
the sets of selected features with respect to small variations in the data on which the
feature selection is conducted. To quantify stability, several datasets from the
same data generating process can be used. Alternatively, a single dataset can be
split into parts by resampling. Either way, all datasets used for feature selection must
contain exactly the same features. The feature selection method of interest is
applied on all of the datasets and the sets of chosen features are recorded.
The stability of the feature selection is assessed based on the sets of chosen features
using stability measures.
}
\details{
The stability measure is defined as (see Notation)
\deqn{\frac{2}{m(m-1)}\sum_{i=1}^{m-1} \sum_{j=i+1}^{m}
\frac{I(V_i, V_j) - E(I(V_i, V_j))}{\sqrt{|V_i| \cdot |V_j|} - E(I(V_i, V_j))}}
with \deqn{I(V_i, V_j) = |V_i \cap V_j| + \mathop{\mathrm{MBM}}(V_i \setminus V_j, V_j \backslash V_i).}
\eqn{\mathop{\mathrm{MBM}}(V_i \setminus V_j, V_j \backslash V_i)} denotes the size of the
maximum bipartite matching based on the graph whose vertices are the features
of \eqn{V_i \setminus V_j} on the one side and the features of \eqn{V_j \backslash V_i}
on the other side. Vertices x and y are connected if and only if \eqn{\mathrm{Similarity}(x, y)
\geq \mathrm{threshold}.}
Requires the package \CRANpkg{igraph}.
}
\section{Notation}{
 For the definition of all stability measures in this package,
the following notation is used:
Let \eqn{V_1, \ldots, V_m} denote the sets of chosen features
for the \eqn{m} datasets, i.e. \code{features} has length \eqn{m} and
\eqn{V_i} is a set which contains the \eqn{i}-th entry of \code{features}.
Furthermore, let \eqn{h_j} denote the number of sets that contain feature
\eqn{X_j} so that \eqn{h_j} is the absolute frequency with which feature \eqn{X_j}
is chosen.
Analogously, let \eqn{h_{ij}} denote the number of sets that include both \eqn{X_i} and \eqn{X_j}.
Also, let \eqn{q = \sum_{j=1}^p h_j = \sum_{i=1}^m |V_i|} and \eqn{V = \bigcup_{i=1}^m V_i}.
}

\examples{
feats = list(1:3, 1:4, 1:5)
mat = 0.92 ^ abs(outer(1:10, 1:10, "-"))
stabilityIntersectionMBM(features = feats, sim.mat = mat, N = 1000)
}
\references{
Bommert A, Rahnenführer J (2020).
\dQuote{Adjusted Measures for Feature Selection Stability for Data Sets with Similar Features.}
In \emph{Machine Learning, Optimization, and Data Science}, 203--214.
\doi{10.1007/978-3-030-64583-0_19}.

Bommert A (2020).
\emph{Integration of Feature Selection Stability in Model Fitting}.
Ph.D. thesis, TU Dortmund University, Germany.
\doi{10.17877/DE290R-21906}.
}
\seealso{
\link{listStabilityMeasures}
}
