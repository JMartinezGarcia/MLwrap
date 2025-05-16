test_that("fine_tuning Bayesian Optimization works properly classification", {

  formula = "psych_well_bin ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  tidy_object <- preprocessing(df = sim_data, formula = formula, task = "classification")

  tidy_object <- build_model(tidy_object = tidy_object,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list)

  tidy_object <- fine_tuning(tidy_object = tidy_object,
                             tuner = "Bayesian Optimization",
                             metric = "roc_auc",
                             verbose = F)

  fit <- tidy_object$tuner_fit

  expect_equal(length(fit$.iter), 9)

  expect_equal(fit$.predictions[[9]]$hidden_units[1], 7)

  expect_equal(fit$.predictions[[9]]$activation[1], "sigmoid")

  expect_equal(fit$.predictions[[9]]$.pred_High[1], 0.9233055, tolerance = 1e-2)

})

test_that("fine_tuning Grid Search CV works properly regression", {

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
  fit <- tidy_object$tuner_fit

  expect_equal(length(fit$.iter), 18)

  expect_equal(fit$.predictions[[18]]$hidden_units[1], 10)

  expect_equal(fit$.predictions[[18]]$activation[1], "tanh")

  expect_equal(fit$.predictions[[18]]$.pred[1], 69.14775, tolerance = 1e-2)

})

test_that("Check fine_tuning wrong metric",{

  formula = "psych_well ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  tidy_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  tidy_object <- build_model(tidy_object = tidy_object,
                             model_names = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  expect_error(fine_tuning(tidy_object = tidy_object,
                             tuner = "Bayesian Optimization",
                             metrics = "roc_auc",
                             verbose = F))

})

test_that("Check fine_tuning plot_results not Boolean",{

  formula = "psych_well ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  tidy_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  tidy_object <- build_model(tidy_object = tidy_object,
                             model_names = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  expect_error(fine_tuning(tidy_object = tidy_object,
                           tuner = "Bayesian Optimization",
                           metrics = "rmse",
                           verbose = F,
                           plot_results = "re"))

})

test_that("Check fine_tuning tuner typo",{

  formula = "psych_well ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  tidy_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  tidy_object <- build_model(tidy_object = tidy_object,
                             model_names = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  expect_error(fine_tuning(tidy_object = tidy_object,
                           tuner = "Bayesian Optimisation",
                           metrics = "rmse",
                           verbose = F))

})





