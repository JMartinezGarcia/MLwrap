#devtools::load_all()
# #
# library(dplyr)
# library(TidyML)
#
# #################################################
# #             Comienzo       #
# #################################################

# df_reg <- sim_data %>%
#  dplyr::select(-c("psych_well_bin", "psych_well_pol"))

# df_mul <- sim_data %>%
#     dplyr::select(-c("psych_well", "psych_well_bin"))
#
# formula_mul = "psych_well_pol ~."
#
# formula = "psych_well_pol ~ ."
#
# tidy_object = preprocessing(df_mul, formula_mul, task = "classification")
#
# tidy_object = build_model(tidy_object, "Neural Network",
#                              hyperparameters = list(hidden_units = c(3, 50),
#                              activation = c("relu", "tanh", "sigmoid")))
#
# tidy_object = create_models(tidy_object, "XGBOOST",
#                              hyperparameters = list(mtry = 3, trees = 100, min_n = c(3,4), tree_depth = c(3,5)))
#
# tidy_object = fine_tuning(tidy_object, "Bayesian Optimization", c("rmse"))

# tidy_object = show_results(tidy_object, summary = T, scatter_residuals = T, scatter_predictions = T,
#                            residuals_dist = T)
#
#tidy_object = sensitivity_analysis(tidy_object, type = "SHAP")
# #################################################
# #             Otra Forma de Hacerlo      #
# #################################################
#

# formula = "Species ~ Sepal_Length + Sepal_Width + Petal_Length + Petal_Width"
# formula2 = "Sepal_Length ~ Species + Sepal_Width + Petal_Length + Petal_Width"
#
# model_fit <- dat_big %>%
#
#                   transformer(formula = formula, task = "classification") %>%
#
#                   create_models(model_names = "XGBOOST",
#                                 hyperparameters = list(
#                                     mtry= c(2, 4),
#                                     trees = 50
#                                   )
#                                 ) %>%
#
#                   model_tuning(tuner = "Bayesian Optimization",
#                                metrics = "roc_auc", plot_results = T)  %>%
#
#                    get_results(summary = T, roc_curve = T, dist_by_class = T) %>%
#
#                    sensitivity_analysis(shap_plot = T)
#
# get_results()
# model_fit <- dat_big %>%
#
#                   transformer(formula = formula2) %>%
#
#                   create_models(model_names = "Neural Network",
#                                 hyperparameters = list(hidden_units = 5, activation = c("relu", "sigmoid")),
#                                 task = "regression") %>%
#
#                   model_tuning(tuner = "Bayesian Optimization",
#                                metrics = "rmse") %>%
#
  #
  #
  # get_results(summary = T)

# plot_roc_curve_binary(model_fit)
#
# box_plot_binary(model_fit)
#
# plot_conf_mat_binary(model_fit)
#
# summary_binary(model_fit)
#
# permutation_feature_importance(model_fit)





