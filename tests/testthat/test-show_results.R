formula = "psych_well ~ gender + age + socioec_status + depression"

hyper_nn_tune_list = list(
  learn_rate = c(-2, -1),
  hidden_units = c(3,10)
)

analysis_object <- preprocessing(df = sim_data, formula = formula, task = "regression")

analysis_object <- build_model(analysis_object = analysis_object,
                           model_name = "Random Forest",
                           hyperparameters = list(mtry = 3))

analysis_object <- fine_tuning(analysis_object = analysis_object,
                           tuner = "Bayesian Optimization",
                           metrics = "rmse",
                           verbose = F)

# test_that("show_results works properly regression", {
#
#   analysis_object <- show_results(analysis_object = analysis_object)
#
#   expect_equal(analysis_object$fit_summary$RMSE, 13.68476, tolerance = 1e-2)
#
#
# })

test_that("show_results wrong plots", {

  expect_error(show_results(analysis_object = analysis_object, confusion_matrix = T))

})

test_that("show_results wrong new_data", {

  expect_error(show_results(analysis_object = analysis_object, new_data = "validation"))

})
