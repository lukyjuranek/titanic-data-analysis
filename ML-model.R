library("rpart.plot")
library("rpart")

# Cleans up the given data by selecting specific columns, combining Parch and SibsSP, etc. 
# Args:
#   data: A data frame containing the Titanic dataset
# Returns:
#   A cleaned up data frame
cleanup_data <- function(data){
  # Keeps only selected columns
  data <- data[ , c("Survived", "Pclass","Sex", "Age", "Fare")]
  # Combines Parch and SibsSP
  data$Relatives <- titanic.train$SibSp + titanic.train$Parch
  # Takes the first letter from the cabin value and converts it to a number
  data$CabinNumber <- ifelse(!(titanic.train$Cabin == ""), match(substr(titanic.train$Cabin, 1, 1), LETTERS), NA)
  data
  # Changes the values of Survived from 0 and 1 to Died and survived
  data$Survived <- ifelse(titanic.train$Survived == 1, "Survived", "Died")
  
  return(data)
}

# Splits the data into a training and test set
# @param data: the data to be split
# @param split_percentage: the percentage of data to be used for training (default is 0.8)
# @return a list containing the training and test sets
split_data <- function(data, split_percentage=0.8){
  split = sample(1:dim(data)[1], floor(nrow(data)*0.8))
  training_set = data[split,]
  test_set = data[-split,]
  
  return(list(training_set, test_set))
}

# Trains a decision tree model using the provided training set and displays the tree diagram if show_tree is set to TRUE.
# Args:
#   training_set: The data set used to train the decision tree model.
#   show_tree: A boolean indicating whether to display the tree diagram or not. Default is TRUE.
# Returns:
#   The trained decision tree model.
train_decision_tree <- function(training_set, show_tree=TRUE){
  # Train a decision tree
  tree = rpart(formula=Survived~., data=training_set, method="class")
  # Show the tree diagram if show_tree argument is TRUE
  if(show_tree){
    prp(mytree,
          type=2,
          extra=106,
          nn=TRUE,
          box.palette = "auto",
          digits=2,
          roundint=FALSE)
  }
  
  return(tree)
}


# This function takes in a dataset and a trained model, and returns the predicted values for the test set.
# Parameters:
#   data: The dataset to be used for prediction.
#   trained_model: The trained model to be used for prediction.
# Returns:
#   pred: The predicted values for the test set.
my_model <- function(data, trained_model){
  pred = predict(trained_model, test_set, type="class")
  return(pred)
}


# Evaluates the quality of a machine learning model by computing the confusion matrix and error estimates.
# Args:
#   test_set: A data frame containing the test set.
#   prediction: A vector containing the predicted values.
# 
# Returns:
#   A list containing the model quality metrics (accuracy, precision, and specificity).
evaluate_model <- function(test_set, prediction) {
  # Computes the confusion matrix
  conf_matrix = table(test_set$Survived, prediction, dnn=c("Actual value","Classifier prediction"))
  print(conf_matrix)
  conf_matrix_prop = prop.table(conf_matrix)
  print(conf_matrix_prop)
  
  # Computes error estimates
  model_quality <- data.frame(accuracy=NA, precision=NA, specificity=NA) # Creates an empty df
  model_quality$accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
  model_quality$precision = conf_matrix[1,1]/sum(conf_matrix[,1])
  model_quality$specificity = conf_matrix[2,2]/sum(conf_matrix[,2])
  
  return(list(model_quality = model_quality))
}