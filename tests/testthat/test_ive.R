context("individual_variable_effects")

source("objects_for_tests.R")

test_that("calculating ive for classification", {
  expect_is(ive_rf_class, "individual_variable_effect")
  expect_equal(nrow(ive_rf_class), 2)
})

test_that("calculating ive for regression", {
  expect_is(ive_rf_reg, "individual_variable_effect")
  expect_equal(nrow(ive_rf_reg), 2)
})


test_that("ive works for DALEX explainers", {
  expect_is(ive_exp_reg, "individual_variable_effect")
})
