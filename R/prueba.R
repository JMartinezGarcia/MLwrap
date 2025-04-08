#devtools::load_all()
# #
# library(dplyr)
#
# #################################################
# #             Matriz de Datos Binario       #
# #################################################
#
# dat = TidyML::iris
#
# dat <- iris %>%
#   filter(Species %in% c("virginica", "versicolor")) %>%
#   mutate(Species = droplevels(Species))  # Eliminar niveles no usados
#
# # Agregar nuevas variables
# set.seed(123)
# dat <- dat %>%
#   mutate(
#     Petal.Area = Petal.Length * Petal.Width,  # Nueva variable derivada
#     Random.Num = runif(n(), 0, 100),  # Variable numérica aleatoria
#     Category = sample(c("A", "B", "C"), n(), replace = TRUE)  # Nueva variable categórica
#   )
#
# # Introducir ruido en algunas variables
# dat <- dat %>%
#   mutate(
#     Sepal.Length = Sepal.Length + rnorm(n(), mean = 0, sd = 0.5),  # Ruido normal
#     Petal.Width = ifelse(runif(n()) < 0.05, NA, Petal.Width)  # Introducir valores faltantes
#   )
#
# # Aumentar el tamaño del dataset con resampling
# dat_big <- dat %>%
#   slice_sample(n = 500, replace = TRUE) %>%
#   mutate(Species = sample(c("virginica", "versicolor"), n(), replace = TRUE))
#
# # Crear interacciones entre variables
# dat_big <- dat_big %>%
#   mutate(Sepal.Petal.Ratio = Sepal.Length / Petal.Length)
#
# # Generar datos fuera de la distribución normal
# dat_big <- dat_big %>%
#   mutate(Outlier.Var = ifelse(runif(n()) < 0.05, rnorm(n(), mean = 50, sd = 10), Sepal.Length))
#
# # Introducir sesgo en algunas especies
# dat_big <- dat_big %>%
#   mutate(
#     Petal.Width = ifelse(Species == "virginica", Petal.Width * 1.5, Petal.Width),
#     Sepal.Length = ifelse(Species == "versicolor", Sepal.Length + 1, Sepal.Length)
#   )
#
# dat_big <- na.omit(dat_big)
#
# ## Comparar
#
# dat
#
# dat_big
#
# #################################################
# #             Comienzo       #
# #################################################
#
# formula = "Species ~ ."
#
# tidy_object = transformer(dat_big, formula)
#
# tidy_object = create_models(tidy_object, "Neural Network",
#                             hyperparameters = list(hidden_units = 3, activation = c("relu", "tanh")),
#                             task = "classification")
#
# tidy_object = model_tuning(tidy_object, "Bayesian Optimization", c("roc_auc", "accuracy"))
#
# plot_roc_curve_binary(tidy_object)
#
# box_plot_binary(tidy_object)
#
# plot_conf_mat_binary(tidy_object)
#
# summary_binary(tidy_object)
#
# #################################################
# #             Otra Forma de Hacerlo      #
# #################################################
#

# formula = "Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width"
# formula2 = "Sepal.Length ~ Species + Sepal.Width + Petal.Length + Petal.Width"
#
model_fit <- dat_big %>%

                  transformer(formula = formula) %>%

                  create_models(model_names = "SVM",
                                hyperparameters = list(

                                  degree= c(2, 4),
                                  type = "poly"

                                  ),
                                task = "classification") %>%

                  model_tuning(tuner = "Bayesian Optimization",
                               metrics = "roc_auc")  %>%

                   get_results(summary = T, roc_curve = T)


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


