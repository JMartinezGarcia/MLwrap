formula = "psych_well ~ gender + age + socioec_status + depression"

hyper_nn_tune_list = list(
  learn_rate = c(-2, -1),
  hidden_units = c(3,10)
)

tidy_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

tidy_object <- build_model(tidy_object = tidy_object,
                           model_name = "Neural Network",
                           hyperparameters = hyper_nn_tune_list)

tidy_object <- fine_tuning(tidy_object = tidy_object,
                           tuner = "Bayesian Optimization",
                           metrics = "rmse",
                           verbose = F)

test_that("show_results works properly regression", {

  tidy_object <- show_results(tidy_object = tidy_object)

  expect_equal(tidy_object$fit_summary$RMSE, 13.68476, tolerance = 1e-2)


})

test_that("show_results wrong plots", {

  expect_error(show_results(tidy_object = tidy_object, confusion_matrix = T))

})

test_that("show_results wrong new_data", {

  expect_error(show_results(tidy_object = tidy_object, new_data = "validation"))

})
