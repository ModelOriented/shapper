context("plot individual_variable_effects")

test_that("test plotting parameters", {
  skip_if_no_shap()
  expect_is(plot(ive_rf_reg), "gg")
  expect_is(plot(ive_rf_reg, show_predcited = FALSE, show_attributions = FALSE), "gg")
})

