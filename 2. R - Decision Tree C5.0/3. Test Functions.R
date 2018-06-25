test_cross_validation <-function(iteration, dataset, is_stratified, folds) {
  
  for(fold in folds) {
    vec <- vector()
    strat_vec <- vector()
    for (i in 1:iteration) {
      vec[i] <- cross_validation(dataset, FALSE, is_stratified, fold)
      strat_vec[i] <- cross_validation(dataset, TRUE, is_stratified, fold)
    }
    print(fold)
    print(mean(vec, na.rm = TRUE))
    print(mean(strat_vec, na.rm = TRUE))
    print("------------")
  }
  
}

test_no_global_pruning <-function(iteration, dataset, is_pima_diabetes, control_1, control_2) {
  
  for(fold in folds) {
    vec <- vector()
    strat_vec <- vector()
    for (i in 1:iteration) {
      vec[i] <- cross_validation(dataset, FALSE, is_stratified, fold)
      strat_vec[i] <- cross_validation(dataset, TRUE, is_stratified, fold)
    }
    print(fold)
    print(mean(vec, na.rm = TRUE))
    print(mean(strat_vec, na.rm = TRUE))
    print("------------")
  }
  
}