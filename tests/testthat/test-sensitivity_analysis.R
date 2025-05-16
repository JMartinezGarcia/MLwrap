test_that("PFI works properly regression", {

  formula = "psych_well ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  tidy_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  tidy_object <- build_model(tidy_object = tidy_object,
                             model_names = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  tidy_object <- fine_tuning(tidy_object = tidy_object,
                             tuner = "Bayesian Optimization",
                             metrics = "rmse",
                             verbose = F)

  tidy_object <- sensitivity_analysis(tidy_object, type = "PFI") %>%
    sensitivity_analysis(type = "SHAP") %>%
    sensitivity_analysis(type = "Integrated Gradients") %>%
    sensitivity_analysis(type = "Olden")

  pfi <- tidy_object$sensitivity_analysis$PFI
  shap <- tidy_object$sensitivity_analysis$SHAP
  int_grad <- tidy_object$sensitivity_analysis$IntegratedGradients
  olden <- tidy_object$sensitivity_analysis$Olden

  expect_equal(pfi$Importance[[1]], 17.76971, tolerance = 1e-2)
  expect_equal(shap$depression[1], 18.74176, tolerance = 1e-2)
  expect_equal(int_grad$depression[1], 0.7978044, tolerance = 1e-2)
  expect_equal(olden[1], 0.02784, tolerance = 1e-3)


})
