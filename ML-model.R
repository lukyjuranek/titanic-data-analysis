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
my_model <- function(data_to_predict, trained_model){
  pred = predict(trained_model, data_to_predict, type="class")
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

# Performs k-fold cross validation on a given dataset
# Args:
#   data: the dataset to perform cross validation on
#   nfolds: the number of folds to use in the cross validation (default is 10)
# Returns:
#   Doesn't return anything, but plots a histogram and boxplot of the model quality results
kfold_cross_validate <- function(data, nfolds=10){
  library(caret)
  set.seed(123)
  folds = createFolds(data$Survived, k = nfolds)
  
  cv = lapply(folds, function(x) {
    kfold_training_set = data[-x,]
    kfold_test_set = data[x,]
    
    kfold_tree = train_decision_tree(kfold_training_set, show_tree=FALSE)
    
    kfold_pred = my_model(kfold_test_set, kfold_tree)
    
    model_quality = evaluate_model(kfold_test_set, kfold_pred)$model_quality
    return(model_quality)
  })

  # Create a data frame to store the results of the cross validation
  plotmodelqualityresults=data.frame(values=unlist(cv),
                                   parameter=as.factor(c(rep(c("accuracy",
                                                               "sensitivity",
                                                               "specificity"),nfolds))))

  # Plot a histogram of the model quality results
  library(ggplot2)
  ggplot(data=plotmodelqualityresults)+aes(x=values,fill=parameter)+
    geom_histogram(bins=10, colour="black",aes(y=..density..))+
    geom_density( colour="black",alpha=0,size=1)+facet_grid(.~parameter)

  # Plot a boxplot of the model quality results
  ggplot(data=plotmodelqualityresults)+aes(x=parameter,fill=parameter,y=values)+
    geom_boxplot()

}
