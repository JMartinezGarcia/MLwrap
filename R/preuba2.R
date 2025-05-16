# formula = "psych_well ~ gender + age + socioec_status + depression"
#
# hyper_nn_tune_list = list(
#   learn_rate = c(-2, -1),
#   hidden_units = c(3,10)
# )
#
# tidy_object <- preprocessing(df = sim_data, formula = formula, task = "regression")
#
# tidy_object <- build_model(tidy_object = tidy_object,
#                            model_name = "Neural Network",
#                            hyperparameters = hyper_nn_tune_list)
#
# tidy_object <- fine_tuning(tidy_object = tidy_object,
#                            tuner = "Bayesian Optimization",
#                            metrics = "rmse",
#                            verbose = F)

# tidy_object <- sensitivity_analysis(tidy_object, type = "PFI") %>%
#                sensitivity_analysis(tidy_object, type = "SHAP")
#
#
# tidy_object <- sensitivity_analysis(tidy_object, type = "PFI")
