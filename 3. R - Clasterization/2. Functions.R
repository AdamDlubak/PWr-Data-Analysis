#  ------------------------- Read Data Functions -------------------------

read_dataset <- function(path_to_file) {
  dataset <- read_csv(path_to_file)
  
  # Set class as factor (from char)
  dataset$class<-as.factor(dataset$class)
  
  # Shuffle dataset
  set.seed(4367)
  g <- runif(nrow(dataset))
  dataset_r <- dataset[order(g),]
  
  return (dataset_r)
}

getDatasetToTest <- function(path_to_file, min, max) {
  dataset <- read_dataset(path_to_file)
  return (dataset[,min:max])
}

#  -----------------------------------------------------------------------

#  ------------------------- Present Dataset Functions -------------------------

show_dataset_information <- function(dataset) {
  str(dataset)
  table(dataset$class)
  names(dataset)
  summary(dataset)
  View(dataset)
}

draw_class_bar <- function(dataset) {
  barplot(table(dataset$class), main = "", 
          col = c("orange1", "chocolate4", "coral3", "blue", "chartreuse4", "burlywood3", "darkgoldenrod1", "azure"))
}

draw_class_matrix <- function(dataset) {
  require(GGally)
  
  ggpairs(data=dataset, # data.frame with variables
          columns=2:ncol(dataset), # columns to plot, default to all.
          title="", # title of the plot
          aes(colour = class, alpha = 0.4)) # aesthetics, ggplot2 style
}

#  -----------------------------------------------------------------------

#  ------------------------- Clasterization -------------------------

printConfusionMatrix <- function (cluster, class) {
  print(table(cluster, class))
}

clusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}

analizeDataset <- function(dataset, data, nstart, minClusters, maxClusters, val1, val2) {
  
  cluster <- kmeans(data, maxClusters, nstart = nstart)
  
  clusters <- data.frame(k=minClusters:maxClusters) %>% group_by(k) %>% do(cluster=kmeans(data, .$k))
  
  clustering  <- clusters %>% group_by(k) %>% do(tidy(.$cluster[[1]]))
  assignments <- clusters %>% group_by(k) %>% do(augment(.$cluster[[1]], data)) 
  clusterings <- clusters %>% group_by(k) %>% do(glance(.$cluster[[1]]))
  
  p1 <- ggplot(assignments, aes_string(val1, val2)) + geom_point(aes(color=.cluster)) + facet_wrap(~ k)
  p2 <- p1 + geom_point(data = clustering, aes(x1, x2), size = 5, shape = "x")
  
  print(ggplot(clusterings, aes(k, tot.withinss)) + geom_line())
  print(p1)
}

testClusterization <- function(dataset_to_test, dataset) { 
  print("K-Mean Algorithm")
  for(i in c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")) {
    for (j in c(2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100)) {
      cluster <- kmeans(dataset_to_test, j, nstart = 20, algorithm = i)
      clusterizationStatistics(dataset_to_test, dataset, cluster$cluster, length(cluster$size))
    } 
    cat(sprintf("\n\n\n"))
  }
  print("PAM Algorithm")
  for(i in c("euclidean", "manhattan")) {
    for (j in c(2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 50, 100)) {
      pamCluster <- pam(dataset_to_test, j, metric = i)
      clusterizationStatistics(dataset_to_test, dataset, pamCluster$clustering, length(pamCluster$id.med))
    }
    cat(sprintf("\n\n\n"))
  }
}

clusterizationStatistics <- function(dataToCount, dataset, cluster, length) {
  d<-dist(dataToCount)
  DBIndex <- index.DB(dataToCount, cluster, d, centrotypes="centroids")
  
  dunn <- dunn(d, cluster, dataToCount)
  
  datasetToCompare <- as.numeric(as.character(dataset$class))
  randIndex <- rand.index(cluster, datasetToCompare)
  
  purity <-clusterPurity(cluster, dataset$class)
  cat(sprintf("%d\t%f\t%f\t%f\t%f\n", length, DBIndex$DB, dunn, randIndex, purity)) 
}

# ------------------------ Best scores (and plotting) per dataset ------------------------

bestClasterizationTest <- function(dataset_to_test, dataset, clasters, algorithmKMeans, algorithmPAM) {
  cluster <- kmeans(dataset_to_test, clasters, nstart = 20, algorithm = algorithmKMeans)
  clusterization_statistics(dataset_to_test, dataset, cluster$cluster, length(cluster$size))
  
  pamCluster <- pam(dataset_to_test, clasters, metric = algorithmPAM)
  clusterization_statistics(dataset_to_test, dataset, pamCluster$clustering, length(pamCluster$id.med))
}

plotBestClasterizationKMean <- function(dataset_to_test, dataset, clasters, algorithmKMeans, val1, val2) {
  cluster <- kmeans(dataset_to_test, clasters, nstart = 20, algorithm = algorithmKMeans) 
  cluster$cluster <- as.factor(cluster$cluster)
  ggplot(dataset, aes_string(val1, val2, color = cluster$cluster)) + geom_point(show.legend=F)
}

plotBestClasterizationPAM <- function(dataset_to_test, dataset, clasters, algorithmPAM, val1, val2) {
  pamCluster <- pam(dataset_to_test, clasters, metric = algorithmPAM)
  pamCluster$clustering <- as.factor(pamCluster$clustering)
  ggplot(dataset, aes_string(val1, val2, color = pamCluster$clustering)) + geom_point(show.legend=F)
}

#  ---------------------------------------------------------------------------------------
