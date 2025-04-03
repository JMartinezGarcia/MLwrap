devtools::load_all()

formula_reg = "y2 ~ x1 + x2 + x3 + x4"
formula_reg = as.formula(formula_reg)

formula_bin = "y ~ x1 + x2 + x3 + x4"
formula_bin = as.formula(formula_bin)

set.seed(42)  # Para reproducibilidad

# Crear el dataframe original
df <- data.frame(
  x1 = runif(100, 2, 7),  # Variable continua
  x2 = runif(100, 3, 8),  # Variable continua
  x3 = sample(0:1, 100, replace = TRUE),  # Factor binario
  x4 = sample(0:2, 100, replace = TRUE)   # Factor con 3 niveles
)

# Introducir ruido en las variables continuas
df$x1 <- df$x1 + rnorm(100, mean = 0, sd = 0.5)
df$x2 <- df$x2 + rnorm(100, mean = 0, sd = 0.5)

# Crear interacciones no lineales y no triviales
df$x1_squared <- df$x1^2
df$x2_log <- log(df$x2 + 1)  # log transformada de x2

# Introducir observaciones atípicas (outliers) en x1 y x2
outlier_indices <- sample(1:100, size = 5)  # 5 outliers aleatorios
df$x1[outlier_indices] <- df$x1[outlier_indices] * 2  # Multiplicar por 2 para crear outliers
df$x2[outlier_indices] <- df$x2[outlier_indices] * 2  # Hacer lo mismo para x2

# Crear 'y' basado en reglas más complicadas
df$y <- with(df, ifelse((x1 + x2 > 12) | (x3 == 1 & x4 != 2) | (x1_squared < 25 & x2_log > 1), 1, 0))
df$y2 <- 3 * df$x1_squared + 0.8 * df$x2 + 1.3 * df$x2_log * df$x3

# Convertir las variables categóricas
df$x3 <- factor(df$x3)
df$x4 <- factor(df$x4, levels = c(0, 1, 2))
df$y <- as.factor(df$y)  # Para clasificación


transformer_ob_reg = transformer(df, formula_reg,
                                 num_vars = c("x1", "x2"),
                                 cat_vars = c("x3", "x4"),
                                 norm_num_vars = "all",
                                 encode_cat_vars = "all"
)

transformer_ob_bin = transformer(df, formula_bin,
                             num_vars = c("x1", "x2"),
                             cat_vars = c("x3", "x4"),
                             norm_num_vars = "all",
                             encode_cat_vars = "all"
                             )

#tidy_object <- TidyMLObject$new(df, transformer_ob)

hyper_nn_tune_list = list(
  learn_rate = c(-2, -1),
  hidden_units = c(3,10)
)

hyper_rf_tune_list = list(
  mtry = c(2,6),
  trees = 100
)

metrics = c("roc_auc")

test_that("create_workflow works properly", {

  model_object = create_models(tidy_object = transformer_ob_bin,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list,
                               task = "classification")

  workflow = create_workflow(model_object)

  expect_equal(class(workflow$pre$actions$recipe), c("action_recipe", "action_pre", "action"))

})

test_that("create_val_set works properly", {

  model_object = create_models(tidy_object = transformer_ob_bin,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list,
                               task = "classification")

  val_set_and_split <- create_val_set(model_object)
  val_set = val_set_and_split$val_set
  val_split = val_set_and_split$val_split

  expect_equal(class(val_set)[1], "validation_set")

  expect_equal(is.null(model_object$train_data), F)

  expect_equal(is.null(model_object$validation_data), F)

  expect_equal(is.null(model_object$test_data), F)

  expect_equal(class(val_split), c("initial_validation_split", "three_way_split" ))

})

test_that("create_metric_set works properly", {

  model_object = create_models(tidy_object = transformer_ob_bin,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list,
                               task = "classification")

  metrics = c("roc_auc", "accuracy")

  model_object$modify("metrics", metrics)

  metric_set <- create_metric_set(model_object$metrics)

  expect_equal(class(metric_set), c("class_prob_metric_set", "metric_set", "function"))

})

test_that("extract_hyperparams works properly", {

  model_object = create_models(tidy_object = transformer_ob_bin,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list,
                               task = "classification")

  metrics = c("roc_auc", "accuracy")

  model_object$modify("workflow", create_workflow(model_object))

  model_object$modify("metrics", metrics)

  extracted_hyperparams <- extract_hyperparams(model_object)

  expect_equal(extracted_hyperparams$object[[1]]$range, list(lower = 3, upper = 10))

  expect_equal(extracted_hyperparams$object[[2]]$values, c("relu", "tanh", "sigmoid"))

  expect_equal(extracted_hyperparams$object[[3]]$range, list(lower = -2, upper = -1))

})

test_that("tune_models_bayesian works properly classification", {

  model_object = create_models(tidy_object = transformer_ob_bin,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list,
                               task = "classification")

  model_object$modify("workflow", create_workflow(model_object))

  model_object$modify("metrics", metrics)

  set.seed(123)

  val_set_and_split <- create_val_set(model_object)

  val_set = val_set_and_split$val_set

  val_split = val_set_and_split$val_split

  tune_fit <- tune_models_bayesian(model_object, val_set, verbose = F)

  expect_equal(class(tune_fit)[1:2], c("iteration_results", "tune_results"))

  expect_equal(is.null(tune_fit$.predictions), F)

  expect_equal(is.null(tune_fit$.metrics), F)

  expect_equal(tune_fit$.iter, c(0,1,2,3,4,5))

})

test_that("tune_models_bayesian works properly regression", {

  model_object = create_models(tidy_object = transformer_ob_reg,
                               model_names = "Neural Network",
                               hyperparameters = hyper_nn_tune_list,
                               task = "regression")
  metrics = c("rmse")

  model_object$modify("workflow", create_workflow(model_object))

  model_object$modify("metrics", metrics)

  set.seed(123)

  val_set_and_split <- create_val_set(model_object)

  val_set = val_set_and_split$val_set

  val_split = val_set_and_split$val_split

  tune_fit <- tune_models_bayesian(model_object, val_set, verbose = F)

  expect_equal(class(tune_fit)[1:2], c("iteration_results", "tune_results"))

  expect_equal(is.null(tune_fit$.predictions), F)

  expect_equal(is.null(tune_fit$.metrics), F)

  expect_equal(tune_fit$.iter, c(0,1,2,3,4,5, 6, 7, 8))

})








