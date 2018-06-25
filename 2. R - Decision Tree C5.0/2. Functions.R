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

#  ------------------------- Cross Validation Functions -------------------------

cross_validation <- function(dataset, is_stratified, is_pima_diabetes, folds){
  ind = createDataPartition(dataset$class, p = 2/3, list= FALSE)
  
  trainDF <- dataset[ind,]
  testDF <- dataset[-ind,]
  
  if(is_stratified) {
    # Stratified - caret::createFolds does stratified folds by default
    ready_folds <- createFolds(trainDF$class, k=folds)  
  } else {
    # Non stratified using cvTools
    folds <- cvFolds(nrow(trainDF), K=folds, type="random")
    df <- data.frame(fold = folds$which, index = folds$subsets)
    ready_folds <- lapply(split(df, df$fold), FUN=function(x) x$index)
  }
  
  model <- train(class~. , data = trainDF, method="C5.0", trControl = caret::trainControl(
    method = "cv",
    returnData = FALSE,
    index = ready_folds
  ))
  
  predictions<-predict(model, testDF)
  t<-table(predictions=predictions, actual=testDF$class)
  mat <- confusionMatrix(data = predictions, reference = testDF$class, mode = "everything")
  if(is_pima_diabetes) {
    b = table(mat$byClass[7])  
    return(as.double(names(b)))
  } else {
    b = table(colMeans(mat$byClass))  
    as.vector(b)
    return (as.double(names(b)[3]))
  }
}

#  -----------------------------------------------------------------------

#  ------------------------- Parameter Test Functions -------------------------

no_global_pruning_test <- function(dataset, is_pima_diabetes, control_1, control_2) {
  
  # Make subsets
  ind = createDataPartition(dataset$class, p = 3/4, list= FALSE)
  train_set <- dataset[ind,]
  test_set <- dataset[-ind,]
  
  X_train <- train_set[, 2:ncol(train_set)]
  y_train <- train_set$class
  X_test <- test_set$class
  
  # Model creation
  model_false <- C5.0(X_train, y_train, control = control_1)
  model_true <- C5.0(X_train, y_train, control = control_2)
  # summary(model_false)
  # summary(model_true)
  
  
  # plot(model_true)
  print(summary(model_false))
  print(summary(model_true))
  
  # Test subset + Test model
  prediction_false = predict(model_false, test_set)
  prediction_true = predict(model_true, test_set)
  # prediction_false
  # table(prediction_false)
  
  # Confusion Matrix and statistics
  matrix_false <- confusionMatrix(data = prediction_false, reference = X_test, mode = "prec_recall")
  matrix_true <- confusionMatrix(data = prediction_true, reference = X_test, mode = "everything")
  #  matrix_false$byClass
  #  matrix_true$byClass
  if(is_pima_diabetes) {
    
    print(as.vector(matrix_false$byClass)[11])
    print(as.vector(matrix_false$byClass)[5])
    print(as.vector(matrix_false$byClass)[6])
    print(as.vector(matrix_false$byClass)[7])
    print(" ")
    print(as.vector(matrix_true$byClass)[11])
    print(as.vector(matrix_true$byClass)[5])
    print(as.vector(matrix_true$byClass)[6])
    print(as.vector(matrix_true$byClass)[7])
   
    returnListFalse <- list(as.vector(matrix_false$byClass)[11], as.vector(matrix_false$byClass)[5], as.vector(matrix_false$byClass)[6], as.vector(matrix_false$byClass)[7])
    returnListTrue <- list(as.vector(matrix_true$byClass)[11], as.vector(matrix_true$byClass)[5], as.vector(matrix_true$byClass)[6], as.vector(matrix_true$byClass)[7])
    returnList <- list(returnListFalse, returnListTrue)
    return (returnList)
    
  } else {

    print(as.vector(colMeans(matrix_false$byClass))[11])
    print(as.vector(colMeans(matrix_false$byClass))[5])
    print(as.vector(colMeans(matrix_false$byClass))[6])
    print(as.vector(colMeans(matrix_false$byClass))[7])
    print(" ")
    print(as.vector(colMeans(matrix_true$byClass))[11])
    print(as.vector(colMeans(matrix_true$byClass))[5])
    print(as.vector(colMeans(matrix_true$byClass))[6])
    print(as.vector(colMeans(matrix_true$byClass))[7])

    returnListFalse <- list(as.vector(colMeans(matrix_false$byClass))[11], as.vector(colMeans(matrix_false$byClass))[5], as.vector(colMeans(matrix_false$byClass))[6], as.vector(colMeans(matrix_false$byClass))[7])
    returnListTrue <- list(as.vector(colMeans(matrix_true$byClass))[11], as.vector(colMeans(matrix_true$byClass))[5], as.vector(colMeans(matrix_true$byClass))[6], as.vector(colMeans(matrix_true$byClass))[7])
    returnList <- list(returnListFalse, returnListTrue)
    return (returnList)
  }
  
}

#  -----------------------------------------------------------------------



cross_validation <- function(dataset, is_stratified, is_pima_diabetes, folds){
  ind = createDataPartition(dataset$class, p = 2/3, list= FALSE)
  
  trainDF <- dataset[ind,]
  testDF <- dataset[-ind,]
  
  if(is_stratified) {
    # Stratified - caret::createFolds does stratified folds by default
    ready_folds <- createFolds(trainDF$class, k=folds)  
  } else {
    # Non stratified using cvTools
    folds <- cvFolds(nrow(trainDF), K=folds, type="random")
    df <- data.frame(fold = folds$which, index = folds$subsets)
    ready_folds <- lapply(split(df, df$fold), FUN=function(x) x$index)
  }
  
  model <- train(class~. , data = trainDF, method="C5.0", trControl = caret::trainControl(
    method = "cv",
    returnData = FALSE,
    index = ready_folds
  ))
  
  predictions<-predict(model, testDF)
  t<-table(predictions=predictions, actual=testDF$class)
  mat <- confusionMatrix(data = predictions, reference = testDF$class, mode = "everything")
  if(is_pima_diabetes) {
    b = table(mat$byClass[7])  
    return(as.double(names(b)))
  } else {
    b = table(colMeans(mat$byClass))  
    as.vector(b)
    return (as.double(names(b)[3]))
  }
}

#  -----------------------------------------------------------------------

#  ------------------------- Parameter Test Functions -------------------------


cf_test <- function(X_train, y_train, X_test, y_test, is_pima_diabetes, control_1, control_2) {
  

  
  # Model creation
  model_false <- C5.0(X_train, y_train, control = control_1)
  model_true <- C5.0(X_train, y_train, control = control_2)
  # summary(model_false)
  # summary(model_true)
  
  
  # plot(model_true)
  print(summary(model_false))
  print(summary(model_true))
  
  # Test subset + Test model
  prediction_false = predict(model_false, test_set)
  prediction_true = predict(model_true, test_set)
  # prediction_false
  # table(prediction_false)
  
  # Confusion Matrix and statistics
  matrix_false <- confusionMatrix(data = prediction_false, reference = X_test, mode = "prec_recall")
  matrix_true <- confusionMatrix(data = prediction_true, reference = X_test, mode = "everything")
  #  matrix_false$byClass
  #  matrix_true$byClass
  if(is_pima_diabetes) {
    
    print(as.vector(matrix_false$byClass)[11])
    print(as.vector(matrix_false$byClass)[5])
    print(as.vector(matrix_false$byClass)[6])
    print(as.vector(matrix_false$byClass)[7])
    print(" ")
    print(as.vector(matrix_true$byClass)[11])
    print(as.vector(matrix_true$byClass)[5])
    print(as.vector(matrix_true$byClass)[6])
    print(as.vector(matrix_true$byClass)[7])
    
    returnListFalse <- list(as.vector(matrix_false$byClass)[11], as.vector(matrix_false$byClass)[5], as.vector(matrix_false$byClass)[6], as.vector(matrix_false$byClass)[7])
    returnListTrue <- list(as.vector(matrix_true$byClass)[11], as.vector(matrix_true$byClass)[5], as.vector(matrix_true$byClass)[6], as.vector(matrix_true$byClass)[7])
    returnList <- list(returnListFalse, returnListTrue)
    return (returnList)
    
  } else {
    
    print(as.vector(colMeans(matrix_false$byClass))[11])
    print(as.vector(colMeans(matrix_false$byClass))[5])
    print(as.vector(colMeans(matrix_false$byClass))[6])
    print(as.vector(colMeans(matrix_false$byClass))[7])
    print(" ")
    print(as.vector(colMeans(matrix_true$byClass))[11])
    print(as.vector(colMeans(matrix_true$byClass))[5])
    print(as.vector(colMeans(matrix_true$byClass))[6])
    print(as.vector(colMeans(matrix_true$byClass))[7])
    
    returnListFalse <- list(as.vector(colMeans(matrix_false$byClass))[11], as.vector(colMeans(matrix_false$byClass))[5], as.vector(colMeans(matrix_false$byClass))[6], as.vector(colMeans(matrix_false$byClass))[7])
    returnListTrue <- list(as.vector(colMeans(matrix_true$byClass))[11], as.vector(colMeans(matrix_true$byClass))[5], as.vector(colMeans(matrix_true$byClass))[6], as.vector(colMeans(matrix_true$byClass))[7])
    returnList <- list(returnListFalse, returnListTrue)
    return (returnList)
  }
  
}

#  -----------------------------------------------------------------------