pca_clustering <- function(x, b = 2, label_text = NULL, loadarrows = F) {
  
  require(cluster)
  
  fit_pca <- princomp(x, cor = T)
  dist_pca <- dist(fit_pca$scores, method = 'euclidean')
  
  # Do clustering, based on median, using k = 2,...,10, and replicate b times:
  result_clustering <- list()
  for (i in 1:9) {
    result_clustering[[i]] <- replicate(b,pam(dist_pca, k = i+1)$clustering)
  }
  
  # Calculate average silhouette widths for all (k,b) replicates
  result_silhouette <- sapply(result_clustering, function(x) apply(x,2, function(x) mean(silhouette(x,dist_pca)[,3]) ))
  result_silhouette_mean <- apply(result_silhouette,2,mean)
  
  # Find the k which yields highest average silhouette width:
  k <-  which.max(result_silhouette_mean)
  
  # Obtain classes by fitting kmeans clustering using k centers:
  k_cluster_classes <- pam(dist_pca, k = k+1)$clustering
  
  # Obtain 2 first PC and classes:
  pca_scores <- fit_pca$scores[,1:2]
  pca_loadings <- fit_pca$loadings[,1:2]
  pca_scores_and_classes <- as.data.frame(cbind(pca_scores,k_cluster_classes))
  if(!is.null(label_text)) {
    pca_scores_and_classes <- as.data.frame(cbind(pca_scores_and_classes,label_text))
  }
  
  pca_scores_and_classes <- pca_scores_and_classes[order(pca_scores_and_classes[,3]),]
  
  # Calculate within class outliers:
  dum_1 <- apply(pca_scores_and_classes[,1:2],2, function(x) tapply(x, pca_scores_and_classes[,3], function(x) boxplot.stats(x)$out))
  dum_2 <- sapply(dum_1, unlist)
  outliers_by_tukey <- unique(c(which(pca_scores_and_classes[,1] %in% dum_2[[1]]),which(pca_scores_and_classes[,2] %in% dum_2[[2]])))
  
  # Calculate total variance by PC
  variances <- round(fit_pca$sdev^2 / sum(fit_pca$sdev^2),3)[1:2]
  variance_labels <- paste0('Comp.',1:2,', %Variance ', variances*100)
  plot_limit <- ceiling(max(abs(pca_scores)))
  
  # Force square plot area:
  par(pty = 's')
  
  # Visualize and use maximum silhouette width clusters as classifiers:
  plot(pca_scores_and_classes[,1], pca_scores_and_classes[,2], xlab = variance_labels[1], ylab = variance_labels[2], yaxt = 'n', pch = 16, col = pca_scores_and_classes[,3],
       xlim = c(-plot_limit,plot_limit),
       ylim = c(-plot_limit,plot_limit));axis(2, las = 2)
  title(adj = 0, main = paste0('k = ',k+1))
  
  # Visualize within class outliers:
  if(length(outliers_by_tukey) > 0) {
    points(pca_scores_and_classes[outliers_by_tukey,1], pca_scores_and_classes[outliers_by_tukey,2], pch = 8, cex = 1.25, col = pca_scores_and_classes[outliers_by_tukey,3] )
  }
  if(!is.null(label_text)) {
    text(pca_scores_and_classes[,1], pca_scores_and_classes[,2], pca_scores_and_classes[,4], pos = 4)
  }
  # Visualize PC transformation loadings:
  if(loadarrows == T) {
    par(new = T)
    plot(0,0,pch = NA, xlim = c(-1,1), ylim = c(-1,1), axes = F, ylab = '', xlab = '')
    arrows(0,0,pca_loadings[,1], pca_loadings[,2], length = 0.075)
    text(pca_loadings[,1], pca_loadings[,2], rownames(pca_loadings), pos = 4)
    
  }
  
}

# Example iris (clustering separates only two classes):
pca_clustering(iris[,-5], b = 10)
pca_clustering(iris[,-5], b = 10, loadarrows = T)
pca_clustering(iris[,-5], b = 10, loadarrows = T, label_text = iris[,5])

# Example Prostate (clustering separeates two classes, which match quite well with Gleason score < 7 vs. Gleason score >= 7):
library(lasso2)
data("Prostate")
pca_clustering(Prostate[,-c(5,7,8)], b = 10)
# With labels:
pca_clustering(Prostate[,-c(5,7,8)], b = 100, label_text = Prostate$gleason)
# With lables and PC loadings:
pca_clustering(Prostate[,-c(5,7,8)], b = 100, label_text = Prostate$gleason, loadarrows = T)

# Example with simulated data with obvious structure:
x <- c(rnorm(100,4,0.3), rnorm(100,1,0.5), rnorm(100,-3,0.1), rnorm(100,4,0.3), rnorm(100,1,0.5))
y <- c(rnorm(100,4,0.3), rnorm(100,1,0.5), rnorm(100,-3,0.1), rnorm(100,-3,0.1), rnorm(100,-3,0.1))
plot(x,y)

pca_clustering(data.frame(x,y), 10, NULL, loadarrows = T)

