library(shapper)

library("DALEX")
library("randomForest")
#Y_train <- factor(as.numeric(HR$status))
Y_train <- HR$status
x_train <- HR[ , -6]
x_train$gender <- as.numeric(x_train$gender)

set.seed(123)
model_rf <- randomForest(x = x_train, y = Y_train)

p_fun <- function(x, data){
  predict(x, newdata = data, type = "prob")
}

individual_variable_importance(x = model_rf, data = x_train, predict_function = p_fun,
                               new_observation = x_train[1,])

