library(reticulate)

sklearn <- import("sklearn")
pd <- import("pandas")
shap <- import("shap")

boston <- shap$datasets$boston()

x_train <- py_to_r(boston[[1]])[,c(4,5)]
Y_train <- boston[[2]]

library(randomForest)
set.seed(1313)
model <- randomForest(x = x_train, y = Y_train)


pfun <- function(data){
  #newdata <- py_to_r(data)
  matrix(predict(model, newdata = data))
}
pfun(x_train)

explainer = shap$KernelExplainer(pfun, x_train)

X_train <- r_to_py(x_train)
shap_values = explainer$shap_values(X_train, nsamples=100)
shap_values



# shap$initjs()
#
# p <- shap$force_plot(explainer$expected_value[1], shap_values[[1]], X_train, link="logit")
#
# matplotlib <- reticulate::import("matplotlib")
# matplotlib$get_backend()
# matplotlib$use('TKAgg')


library(DALEX)
data(apartments)

head(apartments)
x_train <- apartments[, -c(1, 6)]
x_train <- apartments[, -1]
Y_train <- array(apartments$m2.price)

library(randomForest)
model <- randomForest(x = x_train, y = Y_train)
model <- lm(m2.price~., data = cbind(x_train, m2.price = Y_train))

pfun <- function(data){
  #newdata <- py_to_r(data)
  data <- as.data.frame(as.matrix(data))
  colnames(data) <- colnames(x_train)
  data$construction.year <- as.numeric(data$construction.year)
  data$surface <- as.numeric(data$surface)
  data$floor <- as.numeric(data$floor)
  data$no.rooms <- as.numeric(data$no.rooms)
  data$district <- factor(data$district, levels = levels(x_train$district))
  matrix(predict(model, newdata = data))
}

pfun <- function(data){
  #newdata <- py_to_r(data)
  matrix(predict(model, newdata = data))
}
pfun(x_train)

explainer = shap$KernelExplainer(pfun, x_train, link="logit")

X_train <- r_to_py(x_train[1:10,])
shap_values = explainer$shap_values(X_train, nsamples=20)
shap_values


IPython <- import("IPython")
IPython$core$display$display_html(y, raw = TRUE)



