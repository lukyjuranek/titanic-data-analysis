library("rpart.plot")
library("rpart")

# Cleans up the given data by selecting specific columns, combining Parch and SibsSP, etc.
# Args:
#   data: A data frame containing the Titanic dataset
# Returns:
#   A cleaned up data frame
cleanup_data <- function(data){
  # Combines Parch and SibsSP
  data$Relatives <- data$SibSp + data$Parch
  # Keeps only selected columns
  # Takes the first letter from the cabin value and converts it to a number
  data$CabinNumber <- ifelse(!(data$Cabin == ""), match(substr(data$Cabin, 1, 1), LETTERS), NA)
  # Changes the values of Survived from 0 and 1 to Died and survived
  data$Survived <- ifelse(data$Survived == 1, "Survived", "Died")
  data <- data[ , c("Survived", "Pclass","Sex", "Age", "Fare", "Relatives", "CabinNumber")]
  
  return(data)
}

# Splits the data into a training and test set
# Args:
#   data: the data to be split
#   split_percentage: the percentage of data to be used for training (default is 0.8)
# Return:
#   A list containing the training and test sets
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
  tree = rpart(formula=Survived~., data=training_set, method="class", control=rpart.control(minsplit=8, maxdepth=5, cp=0.01))
  # Show the tree diagram if show_tree argument is TRUE
  if(show_tree){
    prp(tree,
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
  #print(conf_matrix)
  conf_matrix_prop = prop.table(conf_matrix)
  #print(conf_matrix_prop)
  
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
    geom_density( colour="black",alpha=0,linewidth=1)+facet_grid(.~parameter)

  # Plot a boxplot of the model quality results
  ggplot(data=plotmodelqualityresults)+aes(x=parameter,fill=parameter,y=values)+
    geom_boxplot()

}


hyper_param_selection_with_kfold <- function(data){
  d_minsplit=seq(from=1,to=40,by=1)
  d_maxdepth=seq(from=1,to=7,by=1)
  d_cp=10^(-seq(from=2,to=4,by=1))
  parameters=expand.grid(minsplit=d_minsplit,maxdepth=d_maxdepth,cp=d_cp)
  cat("Trying ", nrow(parameters), " combinations of parameters")

  library(caret)
  library("rpart")
  # Applying k-Fold Cross Validation
  nfolds = 10
  folds = createFolds(data$Survived, k = nfolds)

  # Iterative process using apply
  cv_hyper = apply(parameters,1,function(y){
    cat(".")
    cv = lapply(folds, function(x) {
      # Select training and test set according to current split
      training_set = data[-x,]
      test_set = data[x,]
      mytree=rpart(formula=Survived ~., data=training_set, method="class",
                  control = rpart.control(minsplit=y[1],maxdepth=y[2],cp=y[3]))
      # Use the function predict to apply the classification algorithm
      # with test set
      pred = predict(mytree,test_set,type="class")
      # Compute the confusion matrix
      conf_matrix = table(test_set$Survived,pred,dnn=c("Actual value","Classifier prediction"))
      conf_matrix_prop = prop.table(conf_matrix)
      
      # Compute error estimates
      accuracy = sum(diag(conf_matrix))/sum(conf_matrix)
      sensitivity = conf_matrix[1,1]/sum(conf_matrix[,1])
      specificity = conf_matrix[2,2]/sum(conf_matrix[,2])
      return(c(accuracy,sensitivity,specificity))
    })
    
    modqualres = data.frame(t(matrix(unlist(cv),nrow = 3)))
    names(modqualres)=c("accuracy","sensitivity","specificity")
    
    return(c(mean(modqualres$accuracy),mean(modqualres$sensitivity),mean(modqualres$specificity)))
    
  })

  plotmodelqualityresults=data.frame(values=c(cv_hyper[1,],
                                              cv_hyper[2,],
                                              cv_hyper[3,]),
                                    parameter=as.factor(c(rep("accuracy",dim(cv_hyper)[2]),
                                                          rep("sensitivity",dim(cv_hyper)[2]),
                                                          rep("specificity",dim(cv_hyper)[2]))))



  # Plots the results
  library(ggplot2)
  ggplot(data=plotmodelqualityresults)+aes(x=values,fill=parameter)+
    geom_histogram(bins=10, colour="black",aes(y=..density..))+
    geom_density( colour="black",alpha=0,linewidth=1)+facet_grid(.~parameter)

  ggplot(data=plotmodelqualityresults)+aes(x=parameter,fill=parameter,y=values)+
    geom_boxplot()

  # The best hyperparameter combination for maximum mean accuracy corresponds to
  print("The best hyperparameter combination for maximum mean accuracy:")
  print(parameters[which.max(cv_hyper[1,]),])
  print(cv_hyper[,which.max(cv_hyper[1,])])
  # The best hyperparameter combination for maximum mean sensitivity corresponds to
  print("The best hyperparameter combination for maximum mean sensitivity:")
  print(parameters[which.max(cv_hyper[2,]),])
  print(cv_hyper[,which.max(cv_hyper[2,])])
  # The best hyperparameter combination for maximum mean specificity corresponds to
  print("The best hyperparameter combination for maximum mean specificity:")
  print(parameters[which.max(cv_hyper[3,]),])
  print(cv_hyper[,which.max(cv_hyper[3,])])
  # The best hyperparameter combination for maximum mean for the three parameters corresponds to
  print("The best hyperparameter combination for maximum mean for the three parameters:")
  print(parameters[which.max(apply(cv_hyper,MARGIN = 2,FUN=mean)),])
  print(cv_hyper[,which.max(apply(cv_hyper,MARGIN = 2,FUN=mean))])
  
  #View(plotmodelqualityresults)

}
