my_model <- (test_data){
    library("rpart.plot")
    library("rpart")

    pred = predict(my.fitted.model, test_data, type="class")
    return(pred)
}