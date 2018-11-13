# shapper
An R wrapper of SHAP python library

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/shapper)](https://CRAN.R-project.org/package=shapper)
[![Build Status](https://travis-ci.org/ModelOriented/shapper.svg?branch=master)](https://travis-ci.org/ModelOriented/shapper)



## Instalation

```
devtools::install_github("ModelOriented/shapper")
```
You can install shap Python library via

```
shapper::install_shap()
```

## Example

```
library(shapper)

library("DALEX")
library("randomForest")
Y_train <- HR$status
x_train <- HR[ , -6]
x_train$gender <- as.numeric(x_train$gender)

set.seed(123)
model_rf <- randomForest(x = x_train, y = Y_train)

p_fun <- function(x, data){
  predict(x, newdata = data, type = "prob")
}

individual_variable_effect(x = model_rf, data = x_train, predict_function = p_fun,
                               new_observation = x_train[1,])

```
