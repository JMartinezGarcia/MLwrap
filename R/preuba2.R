# library(TidyML)
#
# df <- TidyML::sim_data
#
# formula_reg = "psych_well ~ gender + age + socioec_status + depression + life_sat + emot_intel + resilience"
# formula_bin = "psych_well_bin ~ gender + age + socioec_status + depression + life_sat + emot_intel + resilience"
# formula_mul = "psych_well_pol ~ gender + age + socioec_status + depression + life_sat + emot_intel + resilience"
#
# hyper_nn_tune_list = list(
#   learn_rate = c(-3, -1),
#   hidden_units = c(3,10)
# )
#
# model_reg <- preprocessing(df = df, formula = formula_reg, task = "regression")
#
# model_reg <- build_model(tidy_object = model_reg,
#                            model_name = "Neural Network",
#                            hyperparameters = hyper_nn_tune_list)
#
# model_reg <- fine_tuning(tidy_object = model_reg,
#                            tuner = "Bayesian Optimization",
#                            metrics = "rmse",
#                            verbose = F)
