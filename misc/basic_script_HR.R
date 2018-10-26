library(reticulate)
sklearn <- import("sklearn")
pd <- import("pandas")
shap <- import("shap")

library("DALEX")
library("randomForest")
Y_train <- factor(as.numeric(HR$status))
x_train <- HR[ , -6]
x_train$gender <- as.numeric(x_train$gender)

set.seed(123)
model_rf <- randomForest(x = x_train, y = Y_train)


pfun <- function(data){
  data <- as.data.frame(as.matrix(data))
  colnames(data) <- colnames(x_train)
#  data$gender <- factor(data$gender, levels = levels(x_train$gender))
  res <- matrix(predict(model_rf, newdata = data, type = "prob"), ncol = 3)
  colnames(res) <- c("fired", "ok","promoted")
  res
}

pfun(x_train[2:10,])

explainer = shap$KernelExplainer(pfun, x_train)


X_train <- r_to_py(x_train[1:3,])
shap_values = explainer$shap_values(X_train, nsamples=100)
shap_values






