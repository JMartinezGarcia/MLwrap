test_that("fine_tuning Bayesian Optimization works properly classification", {

  formula = "psych_well_bin ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  set.seed(123)

  analysis_object <- preprocessing(df = sim_data, formula = formula, task = "classification")

  analysis_object <- build_model(analysis_object = analysis_object,
                               model_name = "Neural Network",
                               hyperparameters = hyper_nn_tune_list)

  analysis_object <- fine_tuning(analysis_object = analysis_object,
                             tuner = "Bayesian Optimization",
                             metric = "roc_auc",
                             verbose = F)

  fit <- analysis_object$tuner_fit

  expect_equal(length(fit$.iter), 6)

  expect_equal(fit$.predictions[[6]]$hidden_units[1], 6)

  expect_equal(fit$.predictions[[6]]$activation[1], "sigmoid")

  expect_equal(fit$.predictions[[6]]$.pred_High[1], 0.9212357, tolerance = 1e-2)

})

test_that("fine_tuning Grid Search CV works properly regression", {

  formula = "psych_well ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  analysis_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  analysis_object <- build_model(analysis_object = analysis_object,
                             model_name = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  analysis_object <- fine_tuning(analysis_object = analysis_object,
                             tuner = "Grid Search CV",
                             metrics = "rmse",
                             verbose = F)

  fit <- analysis_object$tuner_fit

  expect_equal(fit$.predictions[[1]]$hidden_units[1], 3)

  expect_equal(fit$.predictions[[1]]$activation[1], "relu")

  expect_equal(fit$.predictions[[1]]$.pred[1], 70.56475, tolerance = 1e-2)

})

test_that("Check fine_tuning wrong metric",{

  formula = "psych_well ~ gender + age + socioec_status + depression"

  hyper_nn_tune_list = list(
    learn_rate = c(-2, -1),
    hidden_units = c(3,10)
  )

  analysis_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  analysis_object <- build_model(analysis_object = analysis_object,
                             model_name = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  expect_error(fine_tuning(analysis_object = analysis_object,
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

  analysis_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  analysis_object <- build_model(analysis_object = analysis_object,
                             model_name = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  expect_error(fine_tuning(analysis_object = analysis_object,
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

  analysis_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

  analysis_object <- build_model(analysis_object = analysis_object,
                             model_name = "Neural Network",
                             hyperparameters = hyper_nn_tune_list)

  expect_error(fine_tuning(analysis_object = analysis_object,
                           tuner = "Bayesian Optimisation",
                           metrics = "rmse",
                           verbose = F))

})





