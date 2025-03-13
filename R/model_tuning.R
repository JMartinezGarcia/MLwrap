create_workflow <- function(tidy_object){

    workflow = workflows::workflow() %>%
                      workflows::add_recipe(tidy_object$transformer) %>%
                      workflows::add_model(tidy_object$models)

    return(workflow)
}

hyperparams_grid_nn <- function(hyperparams, tuner, workflow){

   if (tuner == "Bayesian Optimization"){

     grid_params = workflow %>%
                        workflows::extract_parameter_set_dials() %>%
                        {if (length(hyperparams$n_neurons) > 1) dials::update(.,
                                                                    hidden_units = dials::hidden_units(hyperparams$n_neurons)) else .} %>%
                        {if (length(hyperparams$learning_rate > 1)) dials::update(.,
                                                                    learn_rate = dials::learn_rate(hyperparams$learning_rate)) else .} %>%
                        {if (length(hyperparams$activation_func > 1)) dials::update(.,
                                                                        activation = dials::activation(hyperparams$activation_func)) else .}

  }

  return(grid_params)

}

define_metrics <- function(metrics){

  if (metrics == "Root Mean Squared"){

    metric = yardstick:: rmse

  } else if (metrics == "Mean Absolute Value"){

    metric = yardstick::mae

  } else {

    #### ERROR

    metric = yardstick::accuracy

  }

  return(yardstick::metric_set(metric))

}

# Función para mapear nombres a funciones de dials
get_dials_param <- function(name, param) {
  if (inherits(param, "quant_param")) {
    return(rlang::expr(!!sym(name)(range = param$range)))
  } else if (inherits(param, "qual_param")) {
    return(rlang::expr(!!sym(name)(values = param$values)))
  } else {
    stop("Tipo de parámetro desconocido")
  }
}

tune_models <- function(tidy_object, tuner, sampling_method){

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

          #param_updates <- purrr::imap(tidy_object$hyperparameters$hyperparams_ranges, get_dials_param)



          mlp_brulee_params <-
            tidy_object$workflow |>
            workflows::extract_parameter_set_dials() |>
            update(!!!tidy_object$hyperparameters$hyperparams_ranges)    #update(epochs = dials::epochs(range = c(5, 20)))


          mlp_brulee_start <-
            mlp_brulee_params |>
            dials::grid_regular(levels = 3)

           mlp_brulee_tune_grid <-
             tidy_object$workflow |>
             tune::tune_grid(
               resamples = sampling_method,
               grid      = mlp_brulee_start,
               control   = grid_control,
               #metrics = tidy_object$metrics
             )

          print("COMMENCING BAYESIAN OPTIMIZATION")

          mlp_brulee_bo <-
            tidy_object$workflow |>
            tune::tune_bayes(
              resamples = sampling_method,
              iter      = 10L,
              control   = bayes_control,
              initial   = mlp_brulee_tune_grid,
              #metrics = tidy_object$metrics
            )

          #######################################
          #tuner_object <- tune::tune_bayes(
          #
          #  object = tidy_object$workflow,
          #  resamples = sampling_method,
          #  metrics = yardstick::metric_set(tidy_object$metrics),
          #  control = tune::control_bayes(save_pred = TRUE),
          #  initial = 25,
          #  param_info = hyperparams_grid
          #
          #)
          #
          #tuner_fit <- tuner_object
          #########################################


        } else if (tuner == "Grid Search CV"){

          ##ad_folds <- rsample::vfold_cv(tidy_object$train, v = 2)

          tuner_object <- tune::tune_grid(

            object = tidy_object$workflow,
            resamples = sampling_method,
            metrics = yardstick::metric_set(tidy_object$metrics),
            control = tune::control_grid(save_pred = TRUE),
            grid = hyperparams_grid

          )


        } else {


          #### ERROR

        }

  return(mlp_brulee_bo)

}

model_tuning <- function(tidy_object, tuner, metrics){

            tidy_object$add_workflow(create_workflow(tidy_object))

            tidy_object$add_metrics(define_metrics(metrics))

            if (tidy_object$models_names == "Neural Network"){

              set.seed(123)

              validation_split = rsample::initial_validation_split(tidy_object$full_data, prop = c(0.6, 0.3))

              tidy_object$add_train_data(rsample::training(validation_split))
              tidy_object$add_validation_data(rsample::validation(validation_split))
              tidy_object$add_test_data(rsample::testing(validation_split))

              val_set <- rsample::validation_set(validation_split)

              hyperparams = tidy_object$hyperparameters

              if (hyperparams$tuning == TRUE){

                #hyperparam_grid = hyperparams_grid_nn(hyperparams, tuner, tidy_object$workflow)

                print("STARTING FIT")

                tuner_fit = tune_models(tidy_object, tuner, val_set, hyperparams$hyperparams_dials)

                autoplot(tuner_fit)

                print("FINISHED FIT")

                print(tune::show_best(tuner_fit))

                tidy_object$add_tuner_fit(tuner_fit)

                # ENTRENAMIENTO FINAL
                # =============================================================================

                best_hyper <- tune::select_best(tuner_fit, metric = yardstick::metric_set(tidy_object$metrics))


                final_workflow = workflows::update_model(create_nn(best_hyper, task = task, epochs = 100))

                tidy_object$add_workflow(final_workflow)

                final_model <- workflows::finalize_workflow(
                  x = tidy_object$workflow,
                  parameters = best_hyper
                )

                final_model <- final_model %>%
                  fit(
                    data = tidy_object$train
                  )

                tidy_object$add_final_models(final_model)

              } else{

                ### NO TUNING


              }

            } else{

              ##### OTHER MODELS

              set.seed(123)

  train_test_split <- rsample::initial_split(
    data   = df,
    prop   = 0.8
  )

  train_data <- rsample::training(train_test_split)
  test_data  <- rsample::testing(train_test_split)
              ###### IF HYPS are all length 1, else

}

}
