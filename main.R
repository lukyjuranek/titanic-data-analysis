# Load the my_model object from ML-model.R
source("ML-model.R")
load("./titanic_train.RDATA")
setwd("C:/dev/UC3M/titanic-data-analysis")

## Cleans up data
data <- cleanup_data(titanic.train)

## Divides the data using the the split_data function with ration specified in the split_percentage argument
divided_data <- split_data(data, split_percentage = 0.8)
training_set = divided_data[[1]]
test_set = divided_data[[2]]

## Train tree
mytree <- train_decision_tree(training_set, show_tree=TRUE)

## Uses the model to predict new data
prediction <- my_model(test_set, mytree)

# Use the same test_set in the previous prediction as in the testing in the following section
## Evaluates the model
results <- evaluate_model(test_set, prediction)
print(results$model_quality)

kfold_cross_validate(data, nfolds=10)
