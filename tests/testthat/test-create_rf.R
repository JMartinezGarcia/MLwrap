hyperparams_list = list(

  mtry = c(3,5),
  trees = 150,
  min_n = c(2,5)

)

rf_hyp = HyperparamsRF$new(hyperparams_list)

rf_model_classification = create_rf(rf_hyp, "classification")

rf_model_regression = create_rf(rf_hyp, "regression")

test_that("Random Forest model works properly", {

  expect_equal(rf_model_classification$mode, "classification")
  expect_equal(rf_model_regression$mode, "regression")
  expect_equal(class(rf_model_classification)[1], "rand_forest")
  expect_equal(class(rf_model_regression)[1], "rand_forest")

})

