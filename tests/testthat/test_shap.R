


 library("shapper")
 library("DALEX")
 library("randomForest")
 Y_train <- HR$status
 x_train <- HR[ , -6]
 set.seed(123)
 model_rf <- randomForest(x = x_train, y = Y_train)
 p_fun <- function(x, data){
   predict(x, newdata = data, type = "prob")
 }
res <- individual_variable_effect(x = model_rf, data = x_train,
                                     predict_function = p_fun,
                                     new_observation = x_train[1,], nsamples = 50)



test_that("Output format", {
   expect_is( plot(res), "gg")
})
