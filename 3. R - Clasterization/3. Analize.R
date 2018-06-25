#  -------------------- Read Data -------------------------

path_to_file_iris <- "Datasets/iris.csv"
path_to_file_wine <- "Datasets/wine.csv"
path_to_file_glass <- "Datasets/glass.csv"
path_to_file_pima <- "Datasets/pima-indians-diabetes.csv"
path_to_file_knowledge <- "Datasets/user-knowledge.csv"

# Usage Example
dataset <- read_dataset(path_to_file_pima)

#  --------------------------------------------------------

#  -------------------- Present Dataset -------------------

head(dataset)
draw_class_bar(dataset)
draw_class_matrix(dataset)
show_dataset_information(dataset)
summary(dataset)
#  -------------------- Example Usage ---------------------

# Generate Clusters
cluster <- kmeans(dataset[,2:8], 3, nstart = 20)

# Print Confusion Matrix
printConfusionMatrix(cluster$cluster, dataset$class)

# Print K [1, 9] differents clusters
analizeDataset(dataset, dataset[,2:8], 20, 1, 9, "plasma_concentration", "triceps_skin")

#  --------------------------------------------------------

#  -------------------- Dataset for Clustering -------------------

dataset_iris <- read_dataset(path_to_file_iris)
dataset_to_test_iris <- getDatasetToTest(path_to_file_iris, 2, 5)

dataset_wine <- read_dataset(path_to_file_wine)
dataset_to_test_wine <- getDatasetToTest(path_to_file_wine, 2, 14)

dataset_glass <- read_dataset(path_to_file_glass)
dataset_to_test_glass <- getDatasetToTest(path_to_file_glass, 2, 10)

dataset_pima <- read_dataset(path_to_file_pima)
dataset_to_test_pima <- getDatasetToTest(path_to_file_pima, 2, 9)

dataset_knowledge <- read_dataset(path_to_file_knowledge)
dataset_to_test_knowledge <- getDatasetToTest(path_to_file_knowledge, 2, 5)

#  ---------------------------------------------------------------


# --------------- Test all clasterization parameters per dataset ---------------

testClusterization(dataset_to_test_iris, dataset_iris)
testClusterization(dataset_to_test_wine, dataset_wine)
testClusterization(dataset_to_test_glass, dataset_glass)
testClusterization(dataset_to_test_pima, dataset_pima)
testClusterization(dataset_to_test_knowledge, dataset_knowledge)

# ------------------------ Test best scores per dataset ------------------------

bestClasterizationTest(dataset_to_test_glass, dataset_glass, 6, "Hartigan-Wong", "euclidean")
bestClasterizationTest(dataset_to_test_wine, dataset_wine, 4, "Forgy", "euclidean")
bestClasterizationTest(dataset_to_test_pima, dataset_pima, 3, "MacQueen", "manhattan")
bestClasterizationTest(dataset_to_test_knowledge, dataset_knowledge, 7, "Lloyd", "euclidean")

# ------------------------ Plot result of clasterization -----------------------

ggplot(dataset_glass, aes(al, si, color = class)) + geom_point(show.legend=F)  
plotBestClasterizationKMean(dataset_to_test_glass, dataset_glass, 6, "Hartigan-Wong", "al", "si")
plotBestClasterizationPAM(dataset_to_test_glass, dataset_glass, 6, "euclidean", "al", "si")

ggplot(dataset_wine, aes(flavanoids, proline, color = class)) + geom_point(show.legend=F)  
plotBestClasterizationKMean(dataset_to_test_wine, dataset_wine, 4, "Forgy", "flavanoids", "proline")
plotBestClasterizationPAM(dataset_to_test_wine, dataset_wine, 4, "euclidean", "flavanoids", "proline")

ggplot(dataset_pima, aes(plasma_concentration, blood_pressure, color = class)) + geom_point(show.legend=F)  
plotBestClasterizationKMean(dataset_to_test_pima, dataset_pima, 3, "MacQueen", "plasma_concentration", "blood_pressure")
plotBestClasterizationPAM(dataset_to_test_pima, dataset_pima, 3, "manhattan", "plasma_concentration", "blood_pressure")

ggplot(dataset_knowledge, aes(lpr, peg, color = class)) + geom_point(show.legend=F)  
plotBestClasterizationKMean(dataset_to_test_knowledge, dataset_knowledge, 7, "Lloyd", "lpr", "peg")
plotBestClasterizationPAM(dataset_to_test_knowledge, dataset_knowledge, 7, "euclidean", "lpr", "peg")

#  -----------------------------------------------------------------------------
