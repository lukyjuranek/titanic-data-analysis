my_model = function(test_set) {
  # Preprocess your test set stored in 'test_set'
  # Combines Parch and SibsSP
  test_set$Relatives <- test_set$SibSp + test_set$Parch
  # Keeps only selected columns
  # Takes the first letter from the cabin value and converts it to a number
  test_set$CabinNumber <- ifelse(!(test_set$Cabin == ""), match(substr(test_set$Cabin, 1, 1), LETTERS), 0)
  # Changes the values of Survived from 0 and 1 to Died and survived
  # Disbaled this beacuse it causes problems with the random forest model
  # data$Survived <- ifelse(data$Survived == 1, "Survived", "Died")
  test_set <- test_set[ , c("Survived", "Pclass","Sex", "Age", "Fare", "Relatives", "CabinNumber")]
  
  # Use the best classifier to predict survival
  pred = predict(bestclassifier, test_set, type="class")
  # Create the confusion matrix
  conf_matrix = table(test_set$Survived, pred, dnn=c("Actual value", "Classifier prediction"))
  conf_matrix_prop = prop.table(conf_matrix)
  # Calculate error estimates
  accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
  precision = conf_matrix[1,1] / sum(conf_matrix[,1])
  specificity = conf_matrix[2,2] / sum(conf_matrix[,2])

  return(list(prediction = pred,
              conf_matrix = conf_matrix,
              conf_matrix_prop = conf_matrix_prop,
              accuracy = accuracy,
              precision = precision,
              specificity = specificity))
}