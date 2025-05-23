# ##### Formula
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
# ##### Ejemplo Regresión
#
# analysis_reg <- preprocessing(df = sim_data, formula = formula_reg, task = "regression")
#
# analysis_reg <- build_model(analysis_object = analysis_reg,
#                          model_name = "Neural Network",
#                          hyperparameters = list(
#                            hidden_units = c(10, 25),
#                            activation = "relu"
#                          ))
#
# analysis_reg <- fine_tuning(analysis_object = analysis_reg,
#                          tuner = "Bayesian Optimization",
#                          metrics = "rmse",
#                          plot_results = TRUE,
#                          verbose = F)
#
# analysis_reg <- show_results(analysis_object = analysis_reg, summary = T, scatter_residuals = T, scatter_predictions = T)
#
# analysis_reg <- sensitivity_analysis(tidy_object = model_reg, type = c("PFI", "Integrated Gradients"))
#
# model_reg <- sensitivity_analysis(tidy_object = model_reg, type = "SHAP")
#
# #### Ejemplo Binario
#
# model_bin <- preprocessing(df = sim_data, formula = formula_bin, task = "classification") %>%
#
#               build_model(model_name = "Random Forest",
#
#                           hyperparameters = list(
#                             mtry = c(3,5),
#                             trees = 100,
#                             min_n = 5
#                           )) %>%
#
#               fine_tuning(tuner = "Bayesian Optimization",
#                           metrics = "roc_auc",
#                           plot_results = T,
#                           verbose = F) %>%
#
#               show_results(summary = T, confusion_matrix = T, roc_curve = T, dist_by_class = T)
#
# model_bin <- sensitivity_analysis(model_bin, type = "SHAP")
#
# ##### Multi
#
# model_mul <- preprocessing(df = sim_data, formula = formula_mul, task = "classification") %>%
#
#   build_model(model_name = "XGBOOST") %>%
#
#   fine_tuning(tuner = "Bayesian Optimization",
#               metrics = "roc_auc",
#               plot_results = T,
#               verbose = F) %>%
#
#   show_results(summary = T, confusion_matrix = T, roc_curve = T, dist_by_class = T) %>%
#
#     sensitivity_analysis(type = "SHAP")
#
#
# model_mul$test_data



