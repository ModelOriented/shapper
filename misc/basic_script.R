library(reticulate)

boston <- shap$datasets$boston()

# shap <- import("shap")
# sklearn <- import("sklearn")
# iri <- shap$datasets$iris()
# X_train <- iri[[1]]
# Y_train <- iri[[2]]
# svm <- sklearn$svm$SVC(kernel='rbf', probability=TRUE)
# svm <- svm$fit(X_train, Y_train)

library(DALEX)
data(apartments)

library(randomForest)

set.seed(1313)
x_train <- py_to_r(boston[[1]])[,c(4,5)]
Y_train <- boston[[2]]

rf <- e1071::svm(x = x_train, y = Y_train)

# x_train <- apartments[,-1]
# Y_train <- apartments$m2.price


pfun <- function(data){
  #newdata <- py_to_r(data)
  matrix(predict(rf, newdata = data))
}
pfun(x_train)

explainer = shap$KernelExplainer(pfun, x_train)
explainer = shap$kmeans(x_train, 3)


#explainer = shap$KernelExplainer(svm$predict_proba, X_train, link="logit")
X_train <- r_to_py(x_train)
shap_values = explainer$shap_values(X_train, nsamples=100)
shap_values




shap$initjs()

p <- shap$force_plot(explainer$expected_value[1],
                     shap_values[[1]],
                     X_train$iloc$`__getitem__`(0L),
                     link="logit")

shap$force_plot(explainer$expected_value[1], shap_values[[1]], X_train, link="logit")

matplotlib <- reticulate::import("matplotlib")
matplotlib$get_backend()
matplotlib$use('TKAgg')



