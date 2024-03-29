# Load the my_model object from ML-model.R
setwd("C:/dev/UC3M/titanic-data-analysis")
load("./titanic_train.RDATA")
source("ml_utilities.R")
# check_and_install_packages()

# Loads packages
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

## Cleans up data
data <- cleanup_data(titanic.train)

## Divides the data using the the split_data function with ration specified in the split_percentage argument
divided_data <- split_data(data, split_percentage = 0.8)
training_set = divided_data[[1]]
test_set = divided_data[[2]]

## Train tree
decision_tree <- train_decision_tree(training_set, show_tree=TRUE)
random_forest <- train_random_forest(training_set)
# knn_model <- train_knn(training_set)
# predicted_knn <- knn_predict(test_set, knn_model)

varImpPlot(random_forest,main = "Importance of variables in the Titanic dataset")

## Uses the model to predict new data
prediction = predict(decision_tree, test_set, type="class")


# Use the same test_set in the previous prediction as in the testing in the following section
## Evaluates the model
results <- evaluate_model(test_set, prediction, print_results=FALSE)
print(results$model_quality)

kfold_cross_validate(data, model="decisionTree", nfolds=10, print_results=TRUE)
kfold_cross_validate(data, model="randomForest", nfolds=10, print_results=TRUE)

decision_tree_hyper_param_selection_with_kfold(data)
random_forest_hyper_param_selection_with_kfold(data)

# Saves the final model and the function used for prediction
source("final_model.R")
bestclassifier = random_forest
#save(bestclassifier, my_model, file = "my_model.RData")

# Loads the model and the function used for prediction
load("./my_model.RDATA")
load("./titanic_train.RDATA")
pred = my_model(titanic.train)
print(pred$accuracy)
print(pred$precision)
print(pred$specificity)
