library(DALEX)

ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
df <- data.frame(group = group, weight = weight, ran = rnorm(20))

model_reg <- lm(weight ~ ., df)
model_class <- glm(group ~ ., df, family = "binomial")

ive_rf_reg <- individual_variable_effect(model_reg, data = df[ , -2],
                                         new_observation = df[1, -2], nsamples = 50)

ive_rf_class <- individual_variable_effect(model_class, data = df[ , -1],
                                         new_observation = df[1, -1], nsamples = 50)

explainer_reg <- explain(model = model_reg, data = df[ , -2])
ive_exp_reg <- individual_variable_effect(explainer_reg,
                                         new_observation = df[1, -2], nsamples = 50)

