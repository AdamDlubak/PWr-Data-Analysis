#  -------------------- Read Data -------------------------

path_to_file <- "Datasets/iris.csv"
path_to_file <- "Datasets/wine.csv"
path_to_file <- "Datasets/glass.csv"
path_to_file <- "Datasets/pima-indians-diabetes.csv"
path_to_file <- "Datasets/user-knowledge.csv"

dataset <- read_dataset(path_to_file)

#  --------------------------------------------------------

#  -------------------- Present Dataset -------------------

draw_class_bar(dataset)
draw_class_matrix(dataset)
show_dataset_information(dataset)

#  --------------------------------------------------------

#  -------------------- Cross Validation Test -------------------

iterations = 20
is_not_prima_diabetes_dataset = FALSE
is_prima_diabetes_dataset = TRUE
folds_to_check_glass = c(2, 4, 6, 8, 9)
folds_to_check = c(2, 4, 6, 8, 10, 20, 40)

dataset_wine <- read_dataset("Datasets/wine.csv")
dataset_glass <- read_dataset("Datasets/glass.csv")
dataset_pima <- read_dataset("Datasets/pima-indians-diabetes.csv")
dataset_knowledge <- read_dataset("Datasets/user-knowledge.csv")

# Cross Validation - Wine Dataset
test_cross_validation(iterations, dataset_wine, is_not_prima_diabetes_dataset, folds_to_check)

# Cross Validation - Glass Dataset
test_cross_validation(iterations, dataset_glass, is_not_prima_diabetes_dataset, folds_to_check_glass)

# Cross Validation - Pima Indians Diabetes Dataset
test_cross_validation(iterations, dataset_pima, is_prima_diabetes_dataset, folds_to_check)

#  -------------------------------------------------------------

#  -------------------- Parameter noGlobalPruning Test -------------------

list = no_global_pruning_test(dataset_wine, is_not_prima_diabetes_dataset, C5.0Control(noGlobalPruning = FALSE), C5.0Control(noGlobalPruning = TRUE))
list = no_global_pruning_test(dataset_glass, is_not_prima_diabetes_dataset, C5.0Control(noGlobalPruning = FALSE), C5.0Control(noGlobalPruning = TRUE))
list = no_global_pruning_test(dataset_pima, is_prima_diabetes_dataset, C5.0Control(noGlobalPruning = FALSE), C5.0Control(noGlobalPruning = TRUE))

#  -----------------------------------------------------------------------

#  -------------------- Parameter fuzzyThreshold Test -------------------

list = no_global_pruning_test(dataset_wine, is_not_prima_diabetes_dataset, C5.0Control(fuzzyThreshold = FALSE), C5.0Control(fuzzyThreshold = TRUE))
list = no_global_pruning_test(dataset_glass, is_not_prima_diabetes_dataset, C5.0Control(fuzzyThreshold = FALSE), C5.0Control(fuzzyThreshold = TRUE))
list = no_global_pruning_test(dataset_pima, is_prima_diabetes_dataset, C5.0Control(fuzzyThreshold = FALSE), C5.0Control(fuzzyThreshold = TRUE))

#  -----------------------------------------------------------------------

#  -------------------- Parameter CF Test -------------------

ind = createDataPartition(dataset_pima$class, p = 3/4, list= FALSE)
train_set <- dataset_pima[ind,]
test_set <- dataset_pima[-ind,]

X_train <- train_set[, 2:ncol(train_set)]
y_train <- train_set$class
X_test <- test_set$class

list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(CF = 0.1), C5.0Control(CF = 0.2))
list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(CF = 0.4), C5.0Control(CF = 0.6))
list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(CF = 0.8), C5.0Control(CF = 1.0))

#  -----------------------------------------------------------------------

#  -------------------- Parameter minCase Test -------------------

ind = createDataPartition(dataset_glass$class, p = 3/4, list= FALSE)
train_set <- dataset_glass[ind,]
test_set <- dataset_glass[-ind,]

X_train <- train_set[, 2:ncol(train_set)]
y_train <- train_set$class
X_test <- test_set$class

list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(minCases = 2), C5.0Control(minCases = 3))
list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(minCases = 4), C5.0Control(minCases = 5))
list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(minCases = 10), C5.0Control(minCases = 20))

#  -----------------------------------------------------------------------

#  -------------------- Best Results for Dataset -------------------
dataset <- dataset_knowledge
  ind = createDataPartition(dataset$class, p = 3/4, list= FALSE)
  train_set <- dataset[ind,]
  test_set <- dataset[-ind,]
  
  X_train <- train_set[, 2:ncol(train_set)]
  y_train <- train_set$class
  X_test <- test_set$class

# Wine
list = cf_test(X_train, y_train, X_test, y_test, is_not_prima_diabetes_dataset, C5.0Control(minCases = 3, CF = 0.25, fuzzyThreshold = TRUE,noGlobalPruning = TRUE), C5.0Control(minCases = 3))
# Glass
list = cf_test(X_train, y_train, X_test, y_test, is_not_prima_diabetes_dataset, C5.0Control(minCases = 4, CF = 0.25, fuzzyThreshold = FALSE,noGlobalPruning = FALSE), C5.0Control(minCases = 5))
# Pima Indians Diabetes
list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(minCases = 10, CF = 0.3, fuzzyThreshold = TRUE,noGlobalPruning = FALSE), C5.0Control(minCases = 20))
# User Knowledge
list = cf_test(X_train, y_train, X_test, y_test, is_prima_diabetes_dataset, C5.0Control(minCases = 10, CF = 0.3, fuzzyThreshold = TRUE,noGlobalPruning = FALSE), C5.0Control(minCases = 20))

#  -----------------------------------------------------------------------
