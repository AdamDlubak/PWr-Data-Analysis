
# Classification data and its labels
X <- dataset_r[,1:4]
Y <- dataset_r[,5]
tc <- trainControl("cv",10)
# Convert variable interpreted as integer to factor
Y <- as.factor(Y)

X <- train(Species~., data=iris, method="C5.0", trControl=tc, na.action = na.omit)
pred <- predict(X, iris) 
summary(X)
cf <- confusionMatrix(pred, iris$Species, mode = "everything")
print(cf)

errs.c50 <- rep(NA, length(folds))
form <- "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
folds <- split(dataset_r, cut(sample(1:nrow(dataset_r)),10))
for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- C5.0(as.formula(form), train)
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$Species, tmp.predict)
  errs.c50[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}
conf.mat
print(sprintf("average error using k-fold cross validation and C5.0 decision tree algorithm: %.3f percent", mean(errs.c50)))






# Divide dataset on training and test subset
 
  # Training subset + model creation
  model_1 <- C5.0(iris_r[1:100, -5], iris_r[1:100, 5])
  model_1
  summary(model_1)

  # Test subset + Test model
  prediction_1 = predict(model_1, iris_r[101:150,])
  prediction_1
  summary(prediction_1)


  
  
  
  
  
  
  
  prediction_2 = predict(model_2, iris_r[101:150,])
  table_3 = table(prediction_2, iris_r[101:150,5])
  table_3
  mat <- confusionMatrix(data = model_2, reference = iris_r[101:150,5], mode = "prec_recall")

  mat <- confusionMatrix(data = prediction_2, reference = iris_r[101:150,5])
  mat <- confusionMatrix(data = prediction_1, reference = iris_r[101:150,5], mode = "prec_recall")
  mat <- confusionMatrix(data = prediction_1, reference = iris_r[101:150,5])
  colMeans(mat$byClass)
  
  length(iris_r[101:150,5])
# Test result as confusion matrix
table_1 = table(iris_r[101:150,5], Predicted = prediction_1)
table_2 = table(prediction_1, iris_r[101:150,5])

precision(table_2)
precision(data = prediction_1, reference = iris_r[101:150,5], relevant = "Relevant")

#Plot decision tree
plot(model_1)
plot(model_2)