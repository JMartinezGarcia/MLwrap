create_workflow <- function(tidy_object){

    workflow = workflows::workflow() %>%
                      workflows::add_recipe(tidy_object$transformer) %>%
                      workflows::add_model(tidy_object$models)

    return(workflow)
}

extract_hyperparams <- function(tidy_object){

  extracted_hyperparams <-
    tidy_object$workflow |>
    workflows::extract_parameter_set_dials() |>
    update(!!!tidy_object$hyperparameters$hyperparams_ranges)

  return(extracted_hyperparams)

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



tune_models_bayesian <- function(tidy_object, sampling_method){

        set.seed(123)

        bayes_control <-
            tune::control_bayes(
              no_improve    = 5L,
              time_limit    = 20,
              verbose = TRUE,
              verbose_iter  = TRUE,
              save_pred     = TRUE,
              save_workflow = TRUE
          )


          extracted_hyperparams <- extract_hyperparams(tidy_object)

          set_metrics <- yardstick::metric_set(!!!rlang::syms(tidy_object$metrics))

          print("COMMENCING BAYESIAN OPTIMIZATION")

          tuner_object <-
            tidy_object$workflow |>
            tune::tune_bayes(
              resamples = sampling_method,
              iter      = 10L,
              control   = bayes_control,
              initial   = 10,
              param_info = extracted_hyperparams,
              metrics = set_metrics

            )

          print("FINISHED BAYESIAN OPTIMIZATION")

          return(tuner_object)

        }

tune_models_grid_search_cv <- function(tidy_object, sampling_method){

          ##ad_folds <- rsample::vfold_cv(tidy_object$train, v = 2)

          grid_control <-
            tune::control_grid(
              allow_par     = TRUE,
              save_pred     = TRUE,
              save_workflow = TRUE,
              parallel_over = NULL
            )

          tuner_object <- tune::tune_grid(

            object = tidy_object$workflow,
            resamples = sampling_method,
            metrics = yardstick::metric_set(tidy_object$metrics),
            control = tune::control_grid(save_pred = TRUE),
            grid = hyperparams_grid,

          )

          return(tuner_object)


}

tune_models <- function(tidy_object, tuner, sampling_method){

  if (tuner == "Bayesian Optimization"){

    tuner_object <- tune_models_bayesian(tidy_object, sampling_method)

  } else if (tuner == "Grid Search CV"){

    tuner_object <- tune_models_grid_search_cv(tidy_object, sampling_method)

    }

  else {

    ##### ERRRORRRR

  }

  return(tuner_object)

}


model_tuning <- function(tidy_object, tuner, metrics){

            tidy_object$add_workflow(create_workflow(tidy_object))

            tidy_object$add_metrics(metrics)

            if (tidy_object$models_names == "Neural Network"){

              set.seed(123)

              validation_split = rsample::initial_validation_split(tidy_object$full_data, prop = c(0.6, 0.3))

              tidy_object$add_train_data(rsample::training(validation_split))
              tidy_object$add_validation_data(rsample::validation(validation_split))
              tidy_object$add_test_data(rsample::testing(validation_split))

              val_set <- rsample::validation_set(validation_split)

              hyperparams = tidy_object$hyperparameters

              if (hyperparams$tuning == TRUE){

                tuner_fit = tune_models(tidy_object, tuner, val_set)

                print(tune::show_best(tuner_fit))

                tidy_object$add_tuner_fit(tuner_fit)

                # ENTRENAMIENTO FINAL
                # ============================================================================

                best_hyper <- tune::select_best(tuner_fit, metric = tidy_object$metrics)

                final_hyperparams <- c(as.list(best_hyper), tidy_object$hyperparameters$hyperparams_constant)

                final_hyperparams <- HyperparamsNN$new(final_hyperparams)

                final_workflow = tidy_object$workflow %>%
                  workflows::update_model(create_nn(final_hyperparams, task = tidy_object$task, epochs = 100))

                tidy_object$add_workflow(final_workflow)

                final_model <- final_workflow %>%
                  fit(
                    data = rbind(tidy_object$train, tidy_object$validation)
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




