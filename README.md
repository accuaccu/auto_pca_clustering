# auto_pca_clustering
Automated data clustering and principal component transformation.

Estimates principal component transformation and 'optimal' number of k centers. Optimal k is found by repeating clustering algortihm with k = 2,...,10 b times (user defined) and choosing highest average silhouette width out of the set. Furthermore, first two principal components are visualised and the points are annotated by the cluster class. Moreover, within group outliers are emphasized. k-Medoids clustering is used as k-Means is not too robust.

- x is a data frame or matrix wiht numerical values
- b is an integer indicating how many iterations the k = 2,...,10 should be repeated (larger b = cost more computation time)
- label_text is the pre-defined labels for the points, if available
- loadarrows is boolean, if true the first two principal component loadings (eigen vectors of the centered data covariance matrix) are visualized
