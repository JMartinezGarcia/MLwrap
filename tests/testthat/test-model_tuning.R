devtools::load_all()

formula = "y ~ x1 + x2 + x3 + x4"
formula = as.formula(formula)

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

# Convertir las variables categóricas
df$x3 <- factor(df$x3)
df$x4 <- factor(df$x4, levels = c(0, 1, 2))
df$y <- as.factor(df$y)  # Para clasificación


transformer_ob = transformer(df, formula, "y",
                             num_vars = c("x1", "x2"),
                             cat_vars = c("x3", "x4"),
                             norm_num_vars = c("x1", "x2"),
                             encode_cat_vars = c("x3", "x4")
                             )

#tidy_object <- TidyMLObject$new(df, transformer_ob)

hyper_nn_tune_list = list(
  learn_rate = c(-2, -1),
  hidden_units = 3
)

model_object = create_models(tidy_object = transformer_ob,
                             model_names = "Neural Network",
                             hyperparameters = hyper_nn_tune_list,
                             task = "classification")

metrics = c("roc_auc", "accuracy")


#test_that("hyperparams_grid_nn works properly", {

#  hyperparams = model_object$hyperparameters$hyperparams

#  workflow = create_workflow(model_object)

#  hyperparam_grid = hyperparams_grid_nn(hyperparams, tuner = "Bayesian Optimization", workflow = workflow)

#  expect_equal(hyperparam_grid, 1)

#})

test_that("Bayesian Optimization Neural Network works properly", {

    model_object$add_workflow(create_workflow(model_object))

    model_object$add_metrics(metrics)

    validation_split = rsample::initial_validation_split(model_object$full_data, prop = c(0.6, 0.3))

    model_object$add_train_data(rsample::training(validation_split))
    model_object$add_validation_data(rsample::validation(validation_split))
    model_object$add_test_data(rsample::testing(validation_split))

    val_set <- rsample::validation_set(validation_split)

    sampling_method = val_set

    hyperparams = model_object$hyperparameters

    tuner = "Bayesian Optimization"

    set.seed(123)

    if (tuner == "Bayesian Optimization"){

      bayes_control <-
        tune::control_bayes(
          no_improve    = 5L,
          time_limit    = 20,
          verbose = TRUE,
          verbose_iter  = TRUE,
          save_pred     = TRUE,
          save_workflow = TRUE
        )

      grid_control <-
        tune::control_grid(
          allow_par     = TRUE,
          save_pred     = TRUE,
          save_workflow = TRUE,
          parallel_over = NULL
        )




      mlp_brulee_params <-
        model_object$workflow |>
        workflows::extract_parameter_set_dials() |>
         update(!!!hyperparams$hyperparams_ranges)


      mlp_brulee_start <-
        mlp_brulee_params |>
        dials::grid_regular(levels = 3)

      # mlp_brulee_tune_grid <-
      #   model_object$workflow |>
      #   tune::tune_grid(
      #     resamples = sampling_method,
      #     grid      = mlp_brulee_start,
      #     control   = grid_control,
      #     #metrics = tidy_object$metrics
      #   )

      print("COMMENCING BAYESIAN OPTIMIZATION")

      mlp_brulee_bo <-
        model_object$workflow |>
        tune::tune_bayes(
          resamples = sampling_method,
          iter      = 10L,
          control   = bayes_control,
          initial   = 10,
          param_info = mlp_brulee_params,
          metrics = model_object$metrics
        )

      tuner_fit = tune_models(model_object, tuner, val_set)

      best_hyper <- as.list(tune::select_best(tuner_fit, metric = model_object$metrics))


      final_workflow = model_object$workflow %>%
        workflows::update_model(create_nn(best_hyper, task = task, epochs = 100))

      tidy_object$add_workflow(final_workflow)

      final_model <- workflows::finalize_workflow(
        x = tidy_object$workflow,
        parameters = best_hyper
      )

      final_model <- final_model %>%
        fit(
          data = tidy_object$train
        )


    }
    #tuner_fit = tune_models(model_object, "Bayesian Optimization", val_set)



    expect_equal(model_tuned, 4)


})






define_metrics <- function(metrics) {
  metric_list <- list("roc_auc()" = yardstick::roc_auc)
  metric_list <- purrr::imap(metric_list, function(f, nm) {
    print(f)
    attr(f, "id") <- nm
    f
  })
  return(metric_list)
}

# Usamos !!! para descomponer la lista y pasarla a metric_set
my_metric_set <- yardstick::metric_set(!!!define_metrics(NULL))
print(my_metric_set)

#################

pepe <- yardstick::roc_auc

# Creamos el wrapper
pepe_wrapper <- function(data, truth, ..., na_rm = TRUE,
                         event_level = yardstick::yardstick_event_level(),
                         case_weights = NULL) {
  pepe(data, truth, ..., na_rm = na_rm,
       event_level = event_level, case_weights = case_weights)
}

# Asignamos el atributo "id" y forzamos la misma clase que la función original
attr(pepe_wrapper, "id") <- "roc_auc"
class(pepe_wrapper) <- class(pepe)

# Ahora, metric_set debería aceptar el wrapper
my_metric_set <- yardstick::metric_set(!!!pepe_wrapper)
print(my_metric_set)

define_metrics <- function(metrics) {
  # Inicializamos una lista vacía
  metrics_list <- list()

  if (metrics == "Root Mean Squared") {

    metric <- list(rmse = yardstick::rmse)

    metrics_list <- append(metrics_list, metric)

  } else if (metrics == "Mean Absolute Value") {

    my_mae <- yardstick::mae

    attr(my_mae, "id") <- "mae"

    metric <- list(mae = my_mae)

    metrics_list <- append(metrics_list, metric)

  } else if (metrics == "ROC AUC") {

    my_roc_auc <- yardstick::roc_auc

    attr(my_roc_auc, "id") <- "roc_auc()"

    metric <- list(roc_auc = my_roc_auc)

    metrics_list <- append(metrics_list, metric)

  } else {

    # Valor por defecto: accuracy
    metric <- list(accuracy = yardstick::accuracy)

    metrics_list <- append(metrics_list, metric)

  }

  return(list(roc_auc = my_roc_auc))
  #return(metrics_list)

}

